//! The `@page` rule.

use super::Location;
use crate::declaration::{parse_declaration, DeclarationBlock};
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::stylesheet::ParserOptions;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A [page selector](https://www.w3.org/TR/css-page-3/#typedef-page-selector)
/// within a `@page` rule.
///
/// Either a name or at least one pseudo class is required.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PageSelector<'i> {
  /// An optional named page type.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Option<CowArcStr<'i>>,
  /// A list of page pseudo classes.
  pub pseudo_classes: Vec<PagePseudoClass>,
}

enum_property! {
  /// A page pseudo class within an `@page` selector.
  ///
  /// See [PageSelector](PageSelector).
  pub enum PagePseudoClass {
    /// The `:left` pseudo class.
    Left,
    /// The `:right` pseudo class.
    Right,
    /// The `:first` pseudo class.
    First,
    /// The `:last` pseudo class.
    Last,
    /// The `:blank` pseudo class.
    Blank,
  }
}

impl<'i> Parse<'i> for PageSelector<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = input.try_parse(|input| input.expect_ident().map(|x| x.into())).ok();
    let mut pseudo_classes = vec![];

    loop {
      // Whitespace is not allowed between pseudo classes
      let state = input.state();
      match input.next_including_whitespace() {
        Ok(Token::Colon) => {
          pseudo_classes.push(PagePseudoClass::parse(input)?);
        }
        _ => {
          input.reset(&state);
          break;
        }
      }
    }

    if name.is_none() && pseudo_classes.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidPageSelector));
    }

    Ok(PageSelector { name, pseudo_classes })
  }
}

enum_property! {
  /// A [page margin box](https://www.w3.org/TR/css-page-3/#margin-boxes).
  pub enum PageMarginBox {
    /// A fixed-size box defined by the intersection of the top and left margins of the page box.
    TopLeftCorner,
    /// A variable-width box filling the top page margin between the top-left-corner and top-center page-margin boxes.
    TopLeft,
    /// A variable-width box centered horizontally between the page’s left and right border edges and filling the
    /// page top margin between the top-left and top-right page-margin boxes.
    TopCenter,
    /// A variable-width box filling the top page margin between the top-center and top-right-corner page-margin boxes.
    TopRight,
    /// A fixed-size box defined by the intersection of the top and right margins of the page box.
    TopRightCorner,
    /// A variable-height box filling the left page margin between the top-left-corner and left-middle page-margin boxes.
    LeftTop,
    /// A variable-height box centered vertically between the page’s top and bottom border edges and filling the
    /// left page margin between the left-top and left-bottom page-margin boxes.
    LeftMiddle,
    /// A variable-height box filling the left page margin between the left-middle and bottom-left-corner page-margin boxes.
    LeftBottom,
    /// A variable-height box filling the right page margin between the top-right-corner and right-middle page-margin boxes.
    RightTop,
    /// A variable-height box centered vertically between the page’s top and bottom border edges and filling the right
    /// page margin between the right-top and right-bottom page-margin boxes.
    RightMiddle,
    /// A variable-height box filling the right page margin between the right-middle and bottom-right-corner page-margin boxes.
    RightBottom,
    /// A fixed-size box defined by the intersection of the bottom and left margins of the page box.
    BottomLeftCorner,
    /// A variable-width box filling the bottom page margin between the bottom-left-corner and bottom-center page-margin boxes.
    BottomLeft,
    /// A variable-width box centered horizontally between the page’s left and right border edges and filling the bottom
    /// page margin between the bottom-left and bottom-right page-margin boxes.
    BottomCenter,
    /// A variable-width box filling the bottom page margin between the bottom-center and bottom-right-corner page-margin boxes.
    BottomRight,
    /// A fixed-size box defined by the intersection of the bottom and right margins of the page box.
    BottomRightCorner,
  }
}

/// A [page margin rule](https://www.w3.org/TR/css-page-3/#margin-at-rules) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PageMarginRule<'i> {
  /// The margin box identifier for this rule.
  pub margin_box: PageMarginBox,
  /// The declarations within the rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for PageMarginRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_char('@')?;
    self.margin_box.to_css(dest)?;
    self.declarations.to_css_block(dest)
  }
}

/// A [@page](https://www.w3.org/TR/css-page-3/#at-page-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PageRule<'i> {
  /// A list of page selectors.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub selectors: Vec<PageSelector<'i>>,
  /// The declarations within the `@page` rule.
  pub declarations: DeclarationBlock<'i>,
  /// The nested margin rules.
  pub rules: Vec<PageMarginRule<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> PageRule<'i> {
  pub(crate) fn parse<'t, 'o>(
    selectors: Vec<PageSelector<'i>>,
    input: &mut Parser<'i, 't>,
    loc: Location,
    options: &ParserOptions<'o, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut declarations = DeclarationBlock::new();
    let mut rules = Vec::new();
    let mut rule_parser = PageRuleParser {
      declarations: &mut declarations,
      rules: &mut rules,
      options: &options,
    };
    let mut parser = RuleBodyParser::new(input, &mut rule_parser);

    while let Some(decl) = parser.next() {
      if let Err((err, _)) = decl {
        if parser.parser.options.error_recovery {
          parser.parser.options.warn(err);
          continue;
        }
        return Err(err);
      }
    }

    Ok(PageRule {
      selectors,
      declarations,
      rules,
      loc,
    })
  }
}

impl<'i> ToCss for PageRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@page")?;
    if let Some(first) = self.selectors.first() {
      // Space is only required if the first selector has a name.
      if !dest.minify || first.name.is_some() {
        dest.write_char(' ')?;
      }
      let mut first = true;
      for selector in &self.selectors {
        if first {
          first = false;
        } else {
          dest.delim(',', false)?;
        }
        selector.to_css(dest)?;
      }
    }

    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();

    let mut i = 0;
    let len = self.declarations.len() + self.rules.len();

    macro_rules! write {
      ($decls: expr, $important: literal) => {
        for decl in &$decls {
          dest.newline()?;
          decl.to_css(dest, $important)?;
          if i != len - 1 || !dest.minify {
            dest.write_char(';')?;
          }
          i += 1;
        }
      };
    }

    write!(self.declarations.declarations, false);
    write!(self.declarations.important_declarations, true);

    if !self.rules.is_empty() {
      if !dest.minify && self.declarations.len() > 0 {
        dest.write_char('\n')?;
      }
      dest.newline()?;

      let mut first = true;
      for rule in &self.rules {
        if first {
          first = false;
        } else {
          if !dest.minify {
            dest.write_char('\n')?;
          }
          dest.newline()?;
        }
        rule.to_css(dest)?;
      }
    }

    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

impl<'i> ToCss for PageSelector<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if let Some(name) = &self.name {
      dest.write_str(&name)?;
    }

    for pseudo in &self.pseudo_classes {
      dest.write_char(':')?;
      pseudo.to_css(dest)?;
    }

    Ok(())
  }
}

struct PageRuleParser<'a, 'o, 'i> {
  declarations: &'a mut DeclarationBlock<'i>,
  rules: &'a mut Vec<PageMarginRule<'i>>,
  options: &'a ParserOptions<'o, 'i>,
}

impl<'a, 'o, 'i> cssparser::DeclarationParser<'i> for PageRuleParser<'a, 'o, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    parse_declaration(
      name,
      input,
      &mut self.declarations.declarations,
      &mut self.declarations.important_declarations,
      &self.options,
    )
  }
}

impl<'a, 'o, 'i> AtRuleParser<'i> for PageRuleParser<'a, 'o, 'i> {
  type Prelude = PageMarginBox;
  type AtRule = ();
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let loc = input.current_source_location();
    PageMarginBox::parse_string(&name)
      .map_err(|_| loc.new_custom_error(ParserError::AtRuleInvalid(name.clone().into())))
  }

  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    let declarations = DeclarationBlock::parse(input, self.options)?;
    self.rules.push(PageMarginRule {
      margin_box: prelude,
      declarations,
      loc: Location {
        source_index: self.options.source_index,
        line: loc.line,
        column: loc.column,
      },
    });
    Ok(())
  }
}

impl<'a, 'o, 'i> QualifiedRuleParser<'i> for PageRuleParser<'a, 'o, 'i> {
  type Prelude = ();
  type QualifiedRule = ();
  type Error = ParserError<'i>;
}

impl<'a, 'o, 'i> RuleBodyItemParser<'i, (), ParserError<'i>> for PageRuleParser<'a, 'o, 'i> {
  fn parse_qualified(&self) -> bool {
    false
  }

  fn parse_declarations(&self) -> bool {
    true
  }
}
