//! CSS rules.
//!
//! The [CssRule](CssRule) enum includes all supported rules, and can be used to parse
//! and serialize rules from CSS. Lists of rules (i.e. within a stylesheet, or inside
//! another rule such as `@media`) are represented by [CssRuleList](CssRuleList).
//!
//! Each rule includes a source location, which indicates the line and column within
//! the source file where it was parsed. This is used when generating source maps.
//!
//! # Example
//!
//! This example shows how you could parse a single CSS rule, and serialize it to a string.
//!
//! ```
//! use parcel_css::{
//!   rules::CssRule,
//!   traits::ToCss,
//!   stylesheet::{ParserOptions, PrinterOptions}
//! };
//!
//! let rule = CssRule::parse_string(
//!   ".foo { color: red; }",
//!   ParserOptions::default()
//! ).unwrap();
//!
//! assert_eq!(
//!   rule.to_css_string(PrinterOptions::default()).unwrap(),
//!   ".foo {\n  color: red;\n}"
//! );
//! ```
//!
//! If you have a [cssparser::Parser](cssparser::Parser) already, you can also use the `parse` and `to_css`
//! methods instead, rather than parsing from a string.
//!
//! See [StyleSheet](super::stylesheet::StyleSheet) to parse an entire file of multiple rules.

#![deny(missing_docs)]

pub mod container;
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
pub mod unknown;
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
use container::ContainerRule;
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
use unknown::UnknownAtRule;
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

/// A source location.
#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize))]
pub struct Location {
  /// The index of the source file within the source map.
  pub source_index: u32,
  /// The line number, starting at 0.
  pub line: u32,
  /// The column number within a line, starting at 1 for first the character of the line.
  /// Column numbers are counted in UTF-16 code units.
  pub column: u32,
}

/// A CSS rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum CssRule<'i> {
  /// A `@media` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Media(MediaRule<'i>),
  /// An `@import` rule.
  Import(ImportRule<'i>),
  /// A style rule.
  Style(StyleRule<'i>),
  /// A `@keyframes` rule.
  Keyframes(KeyframesRule<'i>),
  /// A `@font-face` rule.
  FontFace(FontFaceRule<'i>),
  /// A `@font-palette-values` rule.
  FontPaletteValues(FontPaletteValuesRule<'i>),
  /// A `@page` rule.
  Page(PageRule<'i>),
  /// A `@supports` rule.
  Supports(SupportsRule<'i>),
  /// A `@counter-style` rule.
  CounterStyle(CounterStyleRule<'i>),
  /// A `@namespace` rule.
  Namespace(NamespaceRule<'i>),
  /// A `@-moz-document` rule.
  MozDocument(MozDocumentRule<'i>),
  /// A `@nest` rule.
  Nesting(NestingRule<'i>),
  /// A `@viewport` rule.
  Viewport(ViewportRule<'i>),
  /// A `@custom-media` rule.
  CustomMedia(CustomMediaRule<'i>),
  /// A `@layer` statement rule.
  LayerStatement(LayerStatementRule<'i>),
  /// A `@layer` block rule.
  LayerBlock(LayerBlockRule<'i>),
  /// A `@property` rule.
  Property(PropertyRule<'i>),
  /// A `@container` rule.
  Container(ContainerRule<'i>),
  /// A placeholder for a rule that was removed.
  Ignored,
  /// An unknown at-rule.
  Unknown(UnknownAtRule<'i>),
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
      CssRule::Container(container) => container.to_css_with_context(dest, context),
      CssRule::Unknown(unknown) => unknown.to_css(dest),
      CssRule::Ignored => Ok(()),
    }
  }
}

impl<'i> CssRule<'i> {
  /// Parse a single rule.
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (_, rule) = parse_one_rule(input, &mut TopLevelRuleParser::new(&options))?;
    Ok(rule)
  }

  /// Parse a single rule from a string.
  pub fn parse_string(
    input: &'i str,
    options: ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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

/// A list of CSS rules.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CssRuleList<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub Vec<CssRule<'i>>);

pub(crate) struct MinifyContext<'a, 'i> {
  pub targets: &'a Option<Browsers>,
  pub handler: &'a mut DeclarationHandler<'i>,
  pub important_handler: &'a mut DeclarationHandler<'i>,
  pub handler_context: &'a mut PropertyHandlerContext<'i, 'a>,
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
          if let Some(CssRule::Media(last_rule)) = rules.last_mut() {
            if last_rule.query == media.query {
              last_rule.rules.0.extend(media.rules.0.drain(..));
              last_rule.minify(context, parent_is_unused)?;
              continue;
            }
          }

          if media.minify(context, parent_is_unused)? {
            continue;
          }
        }
        CssRule::Supports(supports) => {
          if let Some(CssRule::Supports(last_rule)) = rules.last_mut() {
            if last_rule.condition == supports.condition {
              last_rule.rules.0.extend(supports.rules.0.drain(..));
              last_rule.minify(context, parent_is_unused)?;
              continue;
            }
          }

          supports.minify(context, parent_is_unused)?;
          if supports.rules.0.is_empty() {
            continue;
          }
        }
        CssRule::Container(container) => {
          if let Some(CssRule::Container(last_rule)) = rules.last_mut() {
            if last_rule.name == container.name && last_rule.condition == container.condition {
              last_rule.rules.0.extend(container.rules.0.drain(..));
              last_rule.minify(context, parent_is_unused)?;
              continue;
            }
          }

          if container.minify(context, parent_is_unused)? {
            continue;
          }
        }
        CssRule::LayerBlock(layer) => {
          if let Some(CssRule::LayerBlock(last_rule)) = rules.last_mut() {
            if last_rule.name == layer.name {
              last_rule.rules.0.extend(layer.rules.0.drain(..));
              last_rule.minify(context, parent_is_unused)?;
              continue;
            }
          }
          if layer.minify(context, parent_is_unused)? {
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

          // Attempt to merge the new rule with the last rule we added.
          let mut merged = false;
          if let Some(CssRule::Style(last_style_rule)) = rules.last_mut() {
            if merge_style_rules(style, last_style_rule, context) {
              // If that was successful, then the last rule has been updated to include the
              // selectors/declarations of the new rule. This might mean that we can merge it
              // with the previous rule, so continue trying while we have style rules available.
              while rules.len() >= 2 {
                let len = rules.len();
                let (a, b) = rules.split_at_mut(len - 1);
                if let (CssRule::Style(last), CssRule::Style(prev)) = (&mut b[0], &mut a[len - 2]) {
                  if merge_style_rules(last, prev, context) {
                    // If we were able to merge the last rule into the previous one, remove the last.
                    rules.pop();
                    continue;
                  }
                }
                // If we didn't see a style rule, or were unable to merge, stop.
                break;
              }
              merged = true;
            }
          }

          let supports = context.handler_context.get_supports_rules(&style);
          let logical = context.handler_context.get_logical_rules(&style);
          if !merged && !style.is_empty() {
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
          if context.unused_symbols.contains(f.name.0.as_ref()) {
            continue;
          }

          f.minify(context, parent_is_unused);

          if let Some(targets) = context.targets {
            let fallbacks = f.get_fallbacks(*targets);
            rules.push(rule);
            rules.extend(fallbacks);
            continue;
          }
        }
        CssRule::Property(property) => {
          if context.unused_symbols.contains(property.name.0.as_ref()) {
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

fn merge_style_rules<'i>(
  style: &mut StyleRule<'i>,
  last_style_rule: &mut StyleRule<'i>,
  context: &mut MinifyContext<'_, 'i>,
) -> bool {
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
    last_style_rule
      .declarations
      .minify(context.handler, context.important_handler, context.handler_context);
    return true;
  } else if style.declarations == last_style_rule.declarations
    && style.rules.0.is_empty()
    && last_style_rule.rules.0.is_empty()
  {
    // Append the selectors to the last rule if the declarations are the same, and all selectors are compatible.
    if style.is_compatible(*context.targets) && last_style_rule.is_compatible(*context.targets) {
      last_style_rule.selectors.0.extend(style.selectors.0.drain(..));
      return true;
    }

    // If both selectors are potentially vendor prefixable, and they are
    // equivalent minus prefixes, add the prefix to the last rule.
    if !style.vendor_prefix.is_empty()
      && !last_style_rule.vendor_prefix.is_empty()
      && !last_style_rule.vendor_prefix.contains(style.vendor_prefix)
      && is_equivalent(&style.selectors, &last_style_rule.selectors)
    {
      // If the new rule is unprefixed, replace the prefixes of the last rule.
      // Otherwise, add the new prefix.
      if style.vendor_prefix.contains(VendorPrefix::None) {
        last_style_rule.vendor_prefix = style.vendor_prefix;
      } else {
        last_style_rule.vendor_prefix |= style.vendor_prefix;
      }
      return true;
    }
  }
  false
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
