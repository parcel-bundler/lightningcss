pub mod keyframes;
pub mod font_face;
pub mod page;
pub mod supports;
pub mod counter_style;
pub mod namespace;
pub mod import;
pub mod media;
pub mod style;
pub mod document;
pub mod nesting;

use media::MediaRule;
use import::ImportRule;
use style::StyleRule;
use keyframes::KeyframesRule;
use font_face::FontFaceRule;
use page::PageRule;
use supports::SupportsRule;
use counter_style::CounterStyleRule;
use namespace::NamespaceRule;
use document::MozDocumentRule;
use nesting::NestingRule;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::declaration::DeclarationHandler;
use crate::vendor_prefix::VendorPrefix;
use crate::prefixes::Feature;
use crate::targets::Browsers;
use std::collections::{HashMap, HashSet};
use crate::selector::{is_equivalent, get_prefix, get_necessary_prefixes, is_unused};
use crate::error::PrinterError;
use crate::logical::LogicalProperties;

pub(crate) trait ToCssWithContext {
  fn to_css_with_context<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> Result<(), PrinterError> where W: std::fmt::Write;
}

pub(crate) struct StyleContext<'a> {
  pub rule: &'a StyleRule,
  pub parent: Option<&'a StyleContext<'a>>
}

#[derive(Debug, PartialEq)]
pub enum CssRule {
  Media(MediaRule),
  Import(ImportRule),
  Style(StyleRule),
  Keyframes(KeyframesRule),
  FontFace(FontFaceRule),
  Page(PageRule),
  Supports(SupportsRule),
  CounterStyle(CounterStyleRule),
  Namespace(NamespaceRule),
  MozDocument(MozDocumentRule),
  Nesting(NestingRule),
  Ignored
}

impl ToCssWithContext for CssRule {
  fn to_css_with_context<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      CssRule::Media(media) => media.to_css_with_context(dest, context),
      CssRule::Import(import) => import.to_css(dest),
      CssRule::Style(style) => style.to_css_with_context(dest, context),
      CssRule::Keyframes(keyframes) => keyframes.to_css(dest),
      CssRule::FontFace(font_face) => font_face.to_css(dest),
      CssRule::Page(font_face) => font_face.to_css(dest),
      CssRule::Supports(supports) => supports.to_css_with_context(dest, context),
      CssRule::CounterStyle(counter_style) => counter_style.to_css(dest),
      CssRule::Namespace(namespace) => namespace.to_css(dest),
      CssRule::MozDocument(document) => document.to_css(dest),
      CssRule::Nesting(nesting) => nesting.to_css_with_context(dest, context),
      CssRule::Ignored => Ok(())
    }
  }
}

impl ToCss for CssRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.to_css_with_context(dest, None)
  }
}

#[derive(Debug, PartialEq)]
pub struct CssRuleList(pub Vec<CssRule>);

pub(crate) struct MinifyContext<'a> {
  pub targets: &'a Option<Browsers>,
  pub handler: &'a mut DeclarationHandler,
  pub important_handler: &'a mut DeclarationHandler,
  pub logical_properties: &'a mut LogicalProperties,
  pub unused_symbols: &'a HashSet<String>
}

impl CssRuleList {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext) {
    let mut keyframe_rules = HashMap::new();
    let mut rules = Vec::new();
    for mut rule in self.0.drain(..) {
      match &mut rule {
        CssRule::Keyframes(keyframes) => {
          if context.unused_symbols.contains(&keyframes.name.0) {
            continue
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
        },
        CssRule::Media(media) => media.minify(context),
        CssRule::Supports(supports) => supports.minify(context),
        CssRule::MozDocument(document) => document.minify(context),
        CssRule::Style(style) => {
          if is_unused(&mut style.selectors.0.iter(), &context.unused_symbols) {
            continue
          }

          style.minify(context);

          if let Some(targets) = context.targets {
            style.vendor_prefix = get_prefix(&style.selectors);
            if style.vendor_prefix.contains(VendorPrefix::None) {
              style.vendor_prefix = get_necessary_prefixes(&style.selectors, *targets);
            }
          }

          if let Some(CssRule::Style(last_style_rule)) = rules.last_mut() {
            // Merge declarations if the selectors are equivalent, and both are compatible with all targets.
            if style.selectors == last_style_rule.selectors && style.is_compatible(*context.targets) && last_style_rule.is_compatible(*context.targets) && style.rules.0.is_empty() && last_style_rule.rules.0.is_empty() {
              last_style_rule.declarations.declarations.extend(style.declarations.declarations.drain(..));
              last_style_rule.declarations.minify(context.handler, context.important_handler, context.logical_properties);
              continue
            } else if style.declarations == last_style_rule.declarations && style.rules.0.is_empty() && last_style_rule.rules.0.is_empty() {
              // Append the selectors to the last rule if the declarations are the same, and all selectors are compatible.
              if style.is_compatible(*context.targets) && last_style_rule.is_compatible(*context.targets) {
                last_style_rule.selectors.0.extend(style.selectors.0.drain(..));
                continue
              }

              // If both selectors are potentially vendor prefixable, and they are 
              // equivalent minus prefixes, add the prefix to the last rule.
              if !style.vendor_prefix.is_empty() && 
                !last_style_rule.vendor_prefix.is_empty() &&
                is_equivalent(&style.selectors, &last_style_rule.selectors)
              {
                // If the new rule is unprefixed, replace the prefixes of the last rule.
                // Otherwise, add the new prefix.
                if style.vendor_prefix.contains(VendorPrefix::None) {
                  last_style_rule.vendor_prefix = style.vendor_prefix;
                } else {
                  last_style_rule.vendor_prefix |= style.vendor_prefix;
                }
                continue
              }
            }
          }
        },
        CssRule::CounterStyle(counter_style) => {
          if context.unused_symbols.contains(&counter_style.name.0) {
            continue
          }
        }
        CssRule::Nesting(nesting) => nesting.minify(context),
        _ => {}
      }

      rules.push(rule)
    }

    self.0 = rules;
  }
}

impl ToCss for CssRuleList {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.to_css_with_context(dest, None)
  }
}

impl ToCssWithContext for CssRuleList {
  fn to_css_with_context<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let mut first = true;
    let mut last_without_block = false;

    for rule in &self.0 {
      // Skip @import rules if collecting dependencies.
      if let CssRule::Import(rule) = &rule {
        if let Some(dependencies) = &mut dest.dependencies {
          dependencies.push(rule.into());
          continue;
        }
      }

      if first {
        first = false;
      } else {
        if !dest.minify && !(last_without_block && matches!(rule, CssRule::Import(..) | CssRule::Namespace(..))) {
          dest.write_char('\n')?;
        }
        dest.newline()?;
      }
      rule.to_css_with_context(dest, context)?;
      last_without_block = matches!(rule, CssRule::Import(..) | CssRule::Namespace(..));
    }

    Ok(())
  }
}
