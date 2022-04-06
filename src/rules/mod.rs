pub mod counter_style;
pub mod custom_media;
pub mod document;
pub mod font_face;
pub mod font_palette_values;
pub mod import;
pub mod keyframes;
pub mod layer;
pub mod media;
pub mod namespace;
pub mod nesting;
pub mod page;
pub mod property;
pub mod style;
pub mod supports;
pub mod viewport;

use self::font_palette_values::FontPaletteValuesRule;
use self::layer::{LayerBlockRule, LayerStatementRule};
use self::property::PropertyRule;
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationHandler;
use crate::dependencies::{Dependency, ImportDependency};
use crate::error::{MinifyError, ParserError, PrinterError};
use crate::parser::TopLevelRuleParser;
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::selector::{downlevel_selectors, get_prefix, is_equivalent};
use crate::stylesheet::ParserOptions;
use crate::targets::Browsers;
use crate::traits::ToCss;
use crate::values::string::CowArcStr;
use crate::vendor_prefix::VendorPrefix;
use counter_style::CounterStyleRule;
use cssparser::{parse_one_rule, ParseError, Parser, ParserInput};
use custom_media::CustomMediaRule;
use document::MozDocumentRule;
use font_face::FontFaceRule;
use import::ImportRule;
use keyframes::KeyframesRule;
use media::MediaRule;
use namespace::NamespaceRule;
use nesting::NestingRule;
use page::PageRule;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use style::StyleRule;
use supports::SupportsRule;
use viewport::ViewportRule;

pub(crate) trait ToCssWithContext<'a, 'i> {
  fn to_css_with_context<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write;
}

pub(crate) struct StyleContext<'a, 'i> {
  pub rule: &'a StyleRule<'i>,
  pub parent: Option<&'a StyleContext<'a, 'i>>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize)]
pub struct Location {
  /// The index of the source file within the source map.
  pub source_index: u32,
  /// The line number, starting at 0.
  pub line: u32,
  /// The column number within a line, starting at 1 for first the character of the line.
  /// Column numbers are counted in UTF-16 code units.
  pub column: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CssRule<'i> {
  Media(MediaRule<'i>),
  Import(ImportRule<'i>),
  Style(StyleRule<'i>),
  Keyframes(KeyframesRule<'i>),
  FontFace(FontFaceRule<'i>),
  FontPaletteValues(FontPaletteValuesRule<'i>),
  Page(PageRule<'i>),
  Supports(SupportsRule<'i>),
  CounterStyle(CounterStyleRule<'i>),
  Namespace(NamespaceRule<'i>),
  MozDocument(MozDocumentRule<'i>),
  Nesting(NestingRule<'i>),
  Viewport(ViewportRule<'i>),
  CustomMedia(CustomMediaRule<'i>),
  LayerStatement(LayerStatementRule<'i>),
  LayerBlock(LayerBlockRule<'i>),
  Property(PropertyRule<'i>),
  Ignored,
}

impl<'a, 'i> ToCssWithContext<'a, 'i> for CssRule<'i> {
  fn to_css_with_context<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      CssRule::Media(media) => media.to_css_with_context(dest, context),
      CssRule::Import(import) => import.to_css(dest),
      CssRule::Style(style) => style.to_css_with_context(dest, context),
      CssRule::Keyframes(keyframes) => keyframes.to_css(dest),
      CssRule::FontFace(font_face) => font_face.to_css(dest),
      CssRule::FontPaletteValues(f) => f.to_css(dest),
      CssRule::Page(font_face) => font_face.to_css(dest),
      CssRule::Supports(supports) => supports.to_css_with_context(dest, context),
      CssRule::CounterStyle(counter_style) => counter_style.to_css(dest),
      CssRule::Namespace(namespace) => namespace.to_css(dest),
      CssRule::MozDocument(document) => document.to_css(dest),
      CssRule::Nesting(nesting) => nesting.to_css_with_context(dest, context),
      CssRule::Viewport(viewport) => viewport.to_css(dest),
      CssRule::CustomMedia(custom_media) => custom_media.to_css(dest),
      CssRule::LayerStatement(layer) => layer.to_css(dest),
      CssRule::LayerBlock(layer) => layer.to_css(dest),
      CssRule::Property(property) => property.to_css(dest),
      CssRule::Ignored => Ok(()),
    }
  }
}

impl<'i> CssRule<'i> {
  /// Parse a single rule.
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (_, rule) = parse_one_rule(input, &mut TopLevelRuleParser::new(&options))?;
    Ok(rule)
  }

  /// Parse a single rule from a string.
  pub fn parse_string(input: &'i str, options: ParserOptions) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut input = ParserInput::new(input);
    let mut parser = Parser::new(&mut input);
    Self::parse(&mut parser, &options)
  }
}

impl<'i> ToCss for CssRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.to_css_with_context(dest, None)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CssRuleList<'i>(pub Vec<CssRule<'i>>);

pub(crate) struct MinifyContext<'a, 'i> {
  pub targets: &'a Option<Browsers>,
  pub handler: &'a mut DeclarationHandler<'i>,
  pub important_handler: &'a mut DeclarationHandler<'i>,
  pub handler_context: &'a mut PropertyHandlerContext<'i>,
  pub unused_symbols: &'a HashSet<String>,
  pub custom_media: Option<HashMap<CowArcStr<'i>, CustomMediaRule<'i>>>,
}

impl<'i> CssRuleList<'i> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<(), MinifyError> {
    let mut keyframe_rules = HashMap::new();
    let mut rules = Vec::new();
    for mut rule in self.0.drain(..) {
      match &mut rule {
        CssRule::Keyframes(keyframes) => {
          if context.unused_symbols.contains(keyframes.name.0.as_ref()) {
            continue;
          }
          keyframes.minify(context);

          macro_rules! set_prefix {
            ($keyframes: ident) => {
              if $keyframes.vendor_prefix.contains(VendorPrefix::None) {
                if let Some(targets) = context.targets {
                  $keyframes.vendor_prefix = Feature::AtKeyframes.prefixes_for(*targets)
                }
              }
            };
          }

          // If there is an existing rule with the same name and identical keyframes,
          // merge the vendor prefixes from this rule into it.
          if let Some(existing_idx) = keyframe_rules.get(&keyframes.name) {
            if let Some(CssRule::Keyframes(existing)) = &mut rules.get_mut(*existing_idx) {
              if existing.keyframes == keyframes.keyframes {
                existing.vendor_prefix |= keyframes.vendor_prefix;
                set_prefix!(existing);
                continue;
              }
            }
          }

          set_prefix!(keyframes);
          keyframe_rules.insert(keyframes.name.clone(), rules.len());

          if let Some(targets) = context.targets {
            let fallbacks = keyframes.get_fallbacks(*targets);
            rules.push(rule);
            rules.extend(fallbacks);
            continue;
          }
        }
        CssRule::CustomMedia(_) => {
          if context.custom_media.is_some() {
            continue;
          }
        }
        CssRule::Media(media) => {
          if media.minify(context, parent_is_unused)? {
            continue;
          }
        }
        CssRule::Supports(supports) => {
          supports.minify(context, parent_is_unused)?;
          if supports.rules.0.is_empty() {
            continue;
          }
        }
        CssRule::MozDocument(document) => document.minify(context)?,
        CssRule::Style(style) => {
          if parent_is_unused || style.minify(context, parent_is_unused)? {
            continue;
          }

          if let Some(targets) = context.targets {
            style.vendor_prefix = get_prefix(&style.selectors);
            if style.vendor_prefix.contains(VendorPrefix::None) {
              style.vendor_prefix = downlevel_selectors(&mut style.selectors, *targets);
            }
          }

          if let Some(CssRule::Style(last_style_rule)) = rules.last_mut() {
            // Merge declarations if the selectors are equivalent, and both are compatible with all targets.
            if style.selectors == last_style_rule.selectors
              && style.is_compatible(*context.targets)
              && last_style_rule.is_compatible(*context.targets)
              && style.rules.0.is_empty()
              && last_style_rule.rules.0.is_empty()
            {
              last_style_rule
                .declarations
                .declarations
                .extend(style.declarations.declarations.drain(..));
              last_style_rule
                .declarations
                .important_declarations
                .extend(style.declarations.important_declarations.drain(..));
              last_style_rule.declarations.minify(
                context.handler,
                context.important_handler,
                context.handler_context,
              );
              rules.extend(context.handler_context.get_supports_rules(&style));
              continue;
            } else if style.declarations == last_style_rule.declarations
              && style.rules.0.is_empty()
              && last_style_rule.rules.0.is_empty()
            {
              // Append the selectors to the last rule if the declarations are the same, and all selectors are compatible.
              if style.is_compatible(*context.targets) && last_style_rule.is_compatible(*context.targets) {
                last_style_rule.selectors.0.extend(style.selectors.0.drain(..));
                continue;
              }

              // If both selectors are potentially vendor prefixable, and they are
              // equivalent minus prefixes, add the prefix to the last rule.
              if !style.vendor_prefix.is_empty()
                && !last_style_rule.vendor_prefix.is_empty()
                && is_equivalent(&style.selectors, &last_style_rule.selectors)
              {
                // If the new rule is unprefixed, replace the prefixes of the last rule.
                // Otherwise, add the new prefix.
                if style.vendor_prefix.contains(VendorPrefix::None) {
                  last_style_rule.vendor_prefix = style.vendor_prefix;
                } else {
                  last_style_rule.vendor_prefix |= style.vendor_prefix;
                }
                continue;
              }
            }
          }

          let supports = context.handler_context.get_supports_rules(&style);
          let logical = context.handler_context.get_logical_rules(&style);
          if !style.is_empty() {
            rules.push(rule);
          }

          if !logical.is_empty() {
            let mut logical = CssRuleList(logical);
            logical.minify(context, parent_is_unused)?;
            rules.extend(logical.0)
          }

          rules.extend(supports);
          continue;
        }
        CssRule::CounterStyle(counter_style) => {
          if context.unused_symbols.contains(counter_style.name.0.as_ref()) {
            continue;
          }
        }
        CssRule::Nesting(nesting) => {
          if nesting.minify(context, parent_is_unused)? {
            continue;
          }
        }
        CssRule::FontPaletteValues(f) => {
          f.minify(context, parent_is_unused);

          if let Some(targets) = context.targets {
            let fallbacks = f.get_fallbacks(*targets);
            rules.push(rule);
            rules.extend(fallbacks);
            continue;
          }
        }
        _ => {}
      }

      rules.push(rule)
    }

    self.0 = rules;
    Ok(())
  }
}

impl<'i> ToCss for CssRuleList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.to_css_with_context(dest, None)
  }
}

impl<'a, 'i> ToCssWithContext<'a, 'i> for CssRuleList<'i> {
  fn to_css_with_context<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    let mut last_without_block = false;

    for rule in &self.0 {
      if let CssRule::Ignored = &rule {
        continue;
      }

      // Skip @import rules if collecting dependencies.
      if let CssRule::Import(rule) = &rule {
        let dep = if dest.dependencies.is_some() {
          Some(Dependency::Import(ImportDependency::new(&rule, dest.filename())))
        } else {
          None
        };

        if let Some(dependencies) = &mut dest.dependencies {
          dependencies.push(dep.unwrap());
          continue;
        }
      }

      if first {
        first = false;
      } else {
        if !dest.minify
          && !(last_without_block
            && matches!(
              rule,
              CssRule::Import(..) | CssRule::Namespace(..) | CssRule::LayerStatement(..)
            ))
        {
          dest.write_char('\n')?;
        }
        dest.newline()?;
      }
      rule.to_css_with_context(dest, context)?;
      last_without_block = matches!(
        rule,
        CssRule::Import(..) | CssRule::Namespace(..) | CssRule::LayerStatement(..)
      );
    }

    Ok(())
  }
}
