pub mod keyframes;
pub mod font_face;
pub mod page;
pub mod supports;
pub mod counter_style;
pub mod namespace;
pub mod import;
pub mod media;
pub mod style;

use media::MediaRule;
use import::ImportRule;
use style::StyleRule;
use keyframes::KeyframesRule;
use font_face::FontFaceRule;
use page::PageRule;
use supports::SupportsRule;
use counter_style::CounterStyleRule;
use namespace::NamespaceRule;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::declaration::DeclarationHandler;
use crate::vendor_prefix::VendorPrefix;
use crate::prefixes::Feature;
use crate::targets::Browsers;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::selector::{is_equivalent, get_prefix, get_necessary_prefixes};

#[derive(Debug, PartialEq, Clone)]
pub enum CssRule {
  Media(Rc<RefCell<MediaRule>>),
  Import(Rc<RefCell<ImportRule>>),
  Style(Rc<RefCell<StyleRule>>),
  Keyframes(Rc<RefCell<KeyframesRule>>),
  FontFace(Rc<RefCell<FontFaceRule>>),
  Page(Rc<RefCell<PageRule>>),
  Supports(Rc<RefCell<SupportsRule>>),
  CounterStyle(Rc<RefCell<CounterStyleRule>>),
  Namespace(Rc<RefCell<NamespaceRule>>)
}

impl ToCss for CssRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      CssRule::Media(media) => media.borrow().to_css(dest),
      CssRule::Import(import) => import.borrow().to_css(dest),
      CssRule::Style(style) => style.borrow().to_css(dest),
      CssRule::Keyframes(keyframes) => keyframes.borrow().to_css(dest),
      CssRule::FontFace(font_face) => font_face.borrow().to_css(dest),
      CssRule::Page(font_face) => font_face.borrow().to_css(dest),
      CssRule::Supports(supports) => supports.borrow().to_css(dest),
      CssRule::CounterStyle(counter_style) => counter_style.borrow().to_css(dest),
      CssRule::Namespace(namespace) => namespace.borrow().to_css(dest)
    }
  }
}

impl CssRule {
  pub fn to_css_string(&self) -> String {
    let mut s = String::new();
    let mut printer = Printer::new(&mut s, None, false, None);
    self.to_css(&mut printer).unwrap();
    s
  }
}

#[derive(Debug, PartialEq)]
pub struct CssRuleList(pub Vec<CssRule>);

impl CssRuleList {
  pub(crate) fn minify(&mut self, targets: Option<Browsers>, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    let mut keyframe_rules = HashMap::new();
    let mut rules = Vec::new();
    for mut rule in self.0.drain(..) {
      match &mut rule {
        CssRule::Keyframes(keyframes) => {
          let mut keyframes = keyframes.borrow_mut();
          keyframes.minify(handler, important_handler);

          macro_rules! set_prefix {
            ($keyframes: ident) => {
              if $keyframes.vendor_prefix.contains(VendorPrefix::None) {
                if let Some(targets) = targets {
                  $keyframes.vendor_prefix = Feature::AtKeyframes.prefixes_for(targets)
                }
              }
            };
          }
  
          // If there is an existing rule with the same name and identical keyframes,
          // merge the vendor prefixes from this rule into it.
          if let Some(existing_idx) = keyframe_rules.get(&keyframes.name) {
            if let Some(CssRule::Keyframes(existing)) = &mut rules.get_mut(*existing_idx) {
              let mut existing = existing.borrow_mut();
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
        CssRule::Media(media) => media.borrow_mut().minify(targets, handler, important_handler),
        CssRule::Supports(supports) => supports.borrow_mut().minify(targets, handler, important_handler),
        CssRule::Style(style) => {
          let mut style = style.borrow_mut();
          style.minify(handler, important_handler);

          if let Some(targets) = targets {
            style.vendor_prefix = get_prefix(&style.selectors);
            if style.vendor_prefix.contains(VendorPrefix::None) {
              style.vendor_prefix = get_necessary_prefixes(&style.selectors, targets);
            }
          }

          if let Some(CssRule::Style(last_style_rule)) = rules.last_mut() {
            let mut last_style_rule = last_style_rule.borrow_mut();
            // Merge declarations if the selectors are equivalent, and both are compatible with all targets.
            if style.selectors == last_style_rule.selectors && style.is_compatible(targets) && last_style_rule.is_compatible(targets) {
              last_style_rule.declarations.declarations.extend(style.declarations.declarations.drain(..));
              last_style_rule.declarations.minify(handler, important_handler);
              continue
            } else if style.declarations == last_style_rule.declarations {
              // Append the selectors to the last rule if the declarations are the same, and all selectors are compatible.
              if style.is_compatible(targets) && last_style_rule.is_compatible(targets) {
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
        _ => {}
      }

      rules.push(rule)
    }

    self.0 = rules;
  }
}
