use std::collections::HashSet;

use crate::compat::Feature;
use crate::declaration::DeclarationBlock;
use crate::media_query::{
  MediaCondition, MediaFeatureId, MediaFeatureName, MediaFeatureValue, MediaList, MediaQuery, MediaType,
  QueryFeature,
};
use crate::properties::custom::UnparsedProperty;
use crate::properties::Property;
use crate::rules::media::MediaRule;
use crate::rules::supports::{SupportsCondition, SupportsRule};
use crate::rules::{style::StyleRule, CssRule, CssRuleList};
use crate::selector::{Direction, PseudoClass};
use crate::targets::Targets;
use crate::values::ident::Ident;
use crate::vendor_prefix::VendorPrefix;
use parcel_selectors::parser::Component;

#[derive(Debug)]
pub(crate) struct SupportsEntry<'i> {
  pub condition: SupportsCondition<'i>,
  pub declarations: Vec<Property<'i>>,
  pub important_declarations: Vec<Property<'i>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum DeclarationContext {
  None,
  StyleRule,
  Keyframes,
  StyleAttribute,
}

#[derive(Debug)]
pub(crate) struct PropertyHandlerContext<'i, 'o> {
  pub targets: Targets,
  pub is_important: bool,
  supports: Vec<SupportsEntry<'i>>,
  ltr: Vec<Property<'i>>,
  rtl: Vec<Property<'i>>,
  dark: Vec<Property<'i>>,
  pub context: DeclarationContext,
  pub unused_symbols: &'o HashSet<String>,
}

impl<'i, 'o> PropertyHandlerContext<'i, 'o> {
  pub fn new(targets: Targets, unused_symbols: &'o HashSet<String>) -> Self {
    PropertyHandlerContext {
      targets,
      is_important: false,
      supports: Vec::new(),
      ltr: Vec::new(),
      rtl: Vec::new(),
      dark: Vec::new(),
      context: DeclarationContext::None,
      unused_symbols,
    }
  }

  pub fn child(&self, context: DeclarationContext) -> Self {
    PropertyHandlerContext {
      targets: self.targets,
      is_important: false,
      supports: Vec::new(),
      ltr: Vec::new(),
      rtl: Vec::new(),
      dark: Vec::new(),
      context,
      unused_symbols: self.unused_symbols,
    }
  }

  pub fn should_compile_logical(&self, feature: Feature) -> bool {
    // Don't convert logical properties in style attributes because
    // our fallbacks rely on extra rules to define --ltr and --rtl.
    if self.context == DeclarationContext::StyleAttribute {
      return false;
    }

    self.targets.should_compile_logical(feature)
  }

  pub fn add_logical_rule(&mut self, ltr: Property<'i>, rtl: Property<'i>) {
    self.ltr.push(ltr);
    self.rtl.push(rtl);
  }

  pub fn add_dark_rule(&mut self, property: Property<'i>) {
    self.dark.push(property);
  }

  pub fn get_additional_rules<T>(&self, style_rule: &StyleRule<'i, T>) -> Vec<CssRule<'i, T>> {
    // TODO: :dir/:lang raises the specificity of the selector. Use :where to lower it?
    let mut dest = Vec::new();

    macro_rules! rule {
      ($dir: ident, $decls: ident) => {
        let mut selectors = style_rule.selectors.clone();
        for selector in &mut selectors.0 {
          selector.append(Component::NonTSPseudoClass(PseudoClass::Dir {
            direction: Direction::$dir,
          }));
        }

        let rule = StyleRule {
          selectors,
          vendor_prefix: VendorPrefix::None,
          declarations: DeclarationBlock {
            declarations: self.$decls.clone(),
            important_declarations: vec![],
          },
          rules: CssRuleList(vec![]),
          loc: style_rule.loc.clone(),
        };

        dest.push(CssRule::Style(rule));
      };
    }

    if !self.ltr.is_empty() {
      rule!(Ltr, ltr);
    }

    if !self.rtl.is_empty() {
      rule!(Rtl, rtl);
    }

    if !self.dark.is_empty() {
      dest.push(CssRule::Media(MediaRule {
        query: MediaList {
          media_queries: vec![MediaQuery {
            qualifier: None,
            media_type: MediaType::All,
            condition: Some(MediaCondition::Feature(QueryFeature::Plain {
              name: MediaFeatureName::Standard(MediaFeatureId::PrefersColorScheme),
              value: MediaFeatureValue::Ident(Ident("dark".into())),
            })),
          }],
        },
        rules: CssRuleList(vec![CssRule::Style(StyleRule {
          selectors: style_rule.selectors.clone(),
          vendor_prefix: VendorPrefix::None,
          declarations: DeclarationBlock {
            declarations: self.dark.clone(),
            important_declarations: vec![],
          },
          rules: CssRuleList(vec![]),
          loc: style_rule.loc.clone(),
        })]),
        loc: style_rule.loc.clone(),
      }))
    }

    dest
  }

  pub fn add_conditional_property(&mut self, condition: SupportsCondition<'i>, property: Property<'i>) {
    if self.context != DeclarationContext::StyleRule {
      return;
    }

    if let Some(entry) = self.supports.iter_mut().find(|supports| condition == supports.condition) {
      if self.is_important {
        entry.important_declarations.push(property);
      } else {
        entry.declarations.push(property);
      }
    } else {
      let mut important_declarations = Vec::new();
      let mut declarations = Vec::new();
      if self.is_important {
        important_declarations.push(property);
      } else {
        declarations.push(property);
      }
      self.supports.push(SupportsEntry {
        condition,
        important_declarations,
        declarations,
      });
    }
  }

  pub fn add_unparsed_fallbacks(&mut self, unparsed: &mut UnparsedProperty<'i>) {
    if self.context != DeclarationContext::StyleRule && self.context != DeclarationContext::StyleAttribute {
      return;
    }

    let fallbacks = unparsed.value.get_fallbacks(self.targets);
    for (condition, fallback) in fallbacks {
      self.add_conditional_property(
        condition,
        Property::Unparsed(UnparsedProperty {
          property_id: unparsed.property_id.clone(),
          value: fallback,
        }),
      );
    }
  }

  pub fn get_supports_rules<T>(&self, style_rule: &StyleRule<'i, T>) -> Vec<CssRule<'i, T>> {
    if self.supports.is_empty() {
      return Vec::new();
    }

    let mut dest = Vec::new();
    for entry in &self.supports {
      dest.push(CssRule::Supports(SupportsRule {
        condition: entry.condition.clone(),
        rules: CssRuleList(vec![CssRule::Style(StyleRule {
          selectors: style_rule.selectors.clone(),
          vendor_prefix: VendorPrefix::None,
          declarations: DeclarationBlock {
            declarations: entry.declarations.clone(),
            important_declarations: entry.important_declarations.clone(),
          },
          rules: CssRuleList(vec![]),
          loc: style_rule.loc.clone(),
        })]),
        loc: style_rule.loc.clone(),
      }));
    }

    dest
  }

  pub fn reset(&mut self) {
    self.supports.clear();
    self.ltr.clear();
    self.rtl.clear();
    self.dark.clear();
  }
}
