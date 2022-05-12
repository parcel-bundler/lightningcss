//! Style rules.

use std::ops::Range;

use super::Location;
use super::MinifyContext;
use crate::compat::Feature;
use crate::context::DeclarationContext;
use crate::declaration::DeclarationBlock;
use crate::error::ParserError;
use crate::error::{MinifyError, PrinterError, PrinterErrorKind};
use crate::printer::Printer;
use crate::rules::{CssRuleList, StyleContext, ToCssWithContext};
use crate::selector::{is_compatible, is_unused, Selectors};
use crate::targets::Browsers;
use crate::traits::ToCss;
use crate::vendor_prefix::VendorPrefix;
use cssparser::*;
use parcel_selectors::SelectorList;

#[cfg(feature = "serde")]
use crate::selector::{deserialize_selectors, serialize_selectors};

/// A CSS [style rule](https://drafts.csswg.org/css-syntax/#style-rules).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StyleRule<'i> {
  /// The selectors for the style rule.
  #[cfg_attr(
    feature = "serde",
    serde(
      serialize_with = "serialize_selectors",
      deserialize_with = "deserialize_selectors",
      borrow
    )
  )]
  pub selectors: SelectorList<'i, Selectors>,
  /// A vendor prefix override, used during selector printing.
  #[cfg_attr(feature = "serde", serde(skip, default = "VendorPrefix::empty"))]
  pub(crate) vendor_prefix: VendorPrefix,
  /// The declarations within the style rule.
  pub declarations: DeclarationBlock<'i>,
  /// Nested rules within the style rule.
  pub rules: CssRuleList<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> StyleRule<'i> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    let mut unused = false;
    if !context.unused_symbols.is_empty() {
      if is_unused(&mut self.selectors.0.iter(), &context.unused_symbols, parent_is_unused) {
        if self.rules.0.is_empty() {
          return Ok(true);
        }

        self.declarations.declarations.clear();
        self.declarations.important_declarations.clear();
        unused = true;
      }
    }

    context.handler_context.context = DeclarationContext::StyleRule;
    self
      .declarations
      .minify(context.handler, context.important_handler, context.handler_context);
    context.handler_context.context = DeclarationContext::None;

    if !self.rules.0.is_empty() {
      self.rules.minify(context, unused)?;
      if unused && self.rules.0.is_empty() {
        return Ok(true);
      }
    }

    Ok(false)
  }

  /// Returns whether the rule is empty.
  pub fn is_empty(&self) -> bool {
    self.declarations.is_empty() && self.rules.0.is_empty()
  }

  /// Returns whether the selectors in the rule are compatible
  /// with all of the given browser targets.
  pub fn is_compatible(&self, targets: Option<Browsers>) -> bool {
    is_compatible(&self.selectors, targets)
  }

  /// Returns the line and column range of the property key and value at the given index in this style rule.
  ///
  /// For performance and memory efficiency in non-error cases, source locations are not stored during parsing.
  /// Instead, they are computed lazily using the original source string that was used to parse the stylesheet/rule.
  pub fn property_location<'t>(
    &self,
    code: &'i str,
    index: usize,
  ) -> Result<(Range<SourceLocation>, Range<SourceLocation>), ParseError<'i, ParserError<'i>>> {
    let mut input = ParserInput::new(code);
    let mut parser = Parser::new(&mut input);

    // advance until start location of this rule.
    parse_at(&mut parser, self.loc, |parser| {
      // skip selector
      parser.parse_until_before(Delimiter::CurlyBracketBlock, |parser| {
        while parser.next().is_ok() {}
        Ok(())
      })?;

      parser.expect_curly_bracket_block()?;
      parser.parse_nested_block(|parser| {
        let loc = self.declarations.property_location(parser, index);
        while parser.next().is_ok() {}
        loc
      })
    })
  }
}

fn parse_at<'i, 't, T, F>(
  parser: &mut Parser<'i, 't>,
  dest: Location,
  parse: F,
) -> Result<T, ParseError<'i, ParserError<'i>>>
where
  F: Copy + for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, ParseError<'i, ParserError<'i>>>,
{
  loop {
    let loc = parser.current_source_location();
    if loc.line >= dest.line || (loc.line == dest.line && loc.column >= dest.column) {
      return parse(parser);
    }

    match parser.next()? {
      Token::CurlyBracketBlock => {
        // Recursively parse nested blocks.
        let res = parser.parse_nested_block(|parser| {
          let res = parse_at(parser, dest, parse);
          while parser.next().is_ok() {}
          res
        });

        if let Ok(v) = res {
          return Ok(v);
        }
      }
      _ => {}
    }
  }
}

impl<'a, 'i> ToCssWithContext<'a, 'i> for StyleRule<'i> {
  fn to_css_with_context<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.vendor_prefix.is_empty() {
      self.to_css_base(dest, context)
    } else {
      let mut first_rule = true;
      macro_rules! prefix {
        ($prefix: ident) => {
          if self.vendor_prefix.contains(VendorPrefix::$prefix) {
            #[allow(unused_assignments)]
            if first_rule {
              first_rule = false;
            } else {
              if !dest.minify {
                dest.write_char('\n')?; // no indent
              }
              dest.newline()?;
            }
            dest.vendor_prefix = VendorPrefix::$prefix;
            self.to_css_base(dest, context)?;
          }
        };
      }

      prefix!(WebKit);
      prefix!(Moz);
      prefix!(Ms);
      prefix!(O);
      prefix!(None);

      dest.vendor_prefix = VendorPrefix::empty();
      Ok(())
    }
  }
}

impl<'a, 'i> StyleRule<'i> {
  fn to_css_base<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    // If supported, or there are no targets, preserve nesting. Otherwise, write nested rules after parent.
    let supports_nesting = self.rules.0.is_empty()
      || dest.targets.is_none()
      || Feature::CssNesting.is_compatible(dest.targets.unwrap());
    let len = self.declarations.declarations.len() + self.declarations.important_declarations.len();
    let has_declarations = supports_nesting || len > 0 || self.rules.0.is_empty();

    if has_declarations {
      dest.add_mapping(self.loc);
      self.selectors.to_css_with_context(dest, context)?;
      dest.whitespace()?;
      dest.write_char('{')?;
      dest.indent();

      let mut i = 0;
      macro_rules! write {
        ($decls: ident, $important: literal) => {
          for decl in &self.declarations.$decls {
            // The CSS modules `composes` property is handled specially, and omitted during printing.
            // We need to add the classes it references to the list for the selectors in this rule.
            if let crate::properties::Property::Composes(composes) = &decl {
              if dest.is_nested() && dest.css_module.is_some() {
                return Err(dest.error(PrinterErrorKind::InvalidComposesNesting, composes.loc));
              }

              if let Some(css_module) = &mut dest.css_module {
                css_module
                  .handle_composes(&self.selectors, &composes)
                  .map_err(|e| dest.error(e, composes.loc))?;
                continue;
              }
            }

            dest.newline()?;
            decl.to_css(dest, $important)?;
            if i != len - 1 || !dest.minify {
              dest.write_char(';')?;
            }

            i += 1;
          }
        };
      }

      write!(declarations, false);
      write!(important_declarations, true);
    }

    macro_rules! newline {
      () => {
        if !dest.minify && (supports_nesting || len > 0) && !self.rules.0.is_empty() {
          if len > 0 {
            dest.write_char('\n')?;
          }
          dest.newline()?;
        }
      };
    }

    macro_rules! end {
      () => {
        if has_declarations {
          dest.dedent();
          dest.newline()?;
          dest.write_char('}')?;
        }
      };
    }

    // Write nested rules after the parent.
    if supports_nesting {
      newline!();
      self.rules.to_css(dest)?;
      end!();
    } else {
      end!();
      newline!();
      self.rules.to_css_with_context(
        dest,
        Some(&StyleContext {
          rule: self,
          parent: context,
        }),
      )?;
    }

    Ok(())
  }
}
