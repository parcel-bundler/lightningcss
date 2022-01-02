use cssparser::SourceLocation;
use std::collections::BTreeMap;
use crate::rules::{CssRule, CssRuleList, style::StyleRule};
use parcel_selectors::SelectorList;
use crate::selector::{SelectorIdent, SelectorString};
use crate::declaration::{Declaration, DeclarationList, DeclarationBlock};
use crate::css_modules::hash;
use crate::vendor_prefix::VendorPrefix;
use crate::compat::Feature;
use crate::targets::Browsers;
use parcel_selectors::{
  parser::{Selector, Component},
  attr::{AttrSelectorOperator, ParsedCaseSensitivity}
};
use crate::properties::{
  Property,
  custom::CustomProperty,
  custom::CustomPropertyWithValue,
};

#[derive(Debug)]
pub(crate) struct LogicalProperties {
  targets: Option<Browsers>,
  hash: String,
  count: u32,
  ltr: BTreeMap<String, Property>,
  rtl: BTreeMap<String, Property>
}

impl LogicalProperties {
  pub fn new(filename: &str, targets: Option<Browsers>) -> LogicalProperties {
    LogicalProperties {
      targets,
      hash: hash(filename),
      count: 0,
      ltr: BTreeMap::new(),
      rtl: BTreeMap::new()
    }
  }

  pub fn is_supported(&self, feature: Feature) -> bool {
    if let Some(targets) = self.targets {
      feature.is_compatible(targets)
    } else {
      true
    }
  }

  pub fn next_rule(&mut self) {
    self.count += 1;
  }

  pub fn add(&mut self, ltr: Property, rtl: Property, dest: Option<&mut DeclarationList>) {
    // Generate unique variable names based on the property names, a hash of the filename, and a count.
    let ltr_name = ltr.name();
    let rtl_name = rtl.name();
    let left_name = format!("--{}-{}-{}", ltr_name, self.hash, self.count);
    let right_name = format!("--{}-{}-{}", rtl_name, self.hash, self.count);

    // If a declaration list is passed, then add variable references for the original property names.
    if let Some(dest) = dest {
      dest.push(Property::Custom(CustomProperty {
        name: ltr_name.into(),
        value: format!("var({})", left_name)
      }));
      dest.push(Property::Custom(CustomProperty {
        name: rtl_name.into(),
        value: format!("var({})", right_name)
      }));
    }

    // Set the ltr and rtl variables to the values. Also insert an invalid value for the opposite
    // variable if needed. This ensures it is overridden so that nested `dir` attributes are supported.
    self.ltr.insert(left_name.clone(), Property::CustomWithValue(CustomPropertyWithValue {
      name: left_name.clone(),
      value: Box::new(ltr)
    }));
    self.ltr.entry(right_name.clone()).or_insert_with(|| Property::Custom(CustomProperty {
      name: right_name.clone(),
      value: " ".into()
    }));
    self.rtl.insert(right_name.clone(), Property::CustomWithValue(CustomPropertyWithValue {
      name: right_name.clone(),
      value: Box::new(rtl)
    }));
    self.rtl.entry(left_name.clone()).or_insert_with(|| Property::Custom(CustomProperty {
      name: left_name.clone(),
      value: " ".into()
    }));
  }

  pub fn to_rules(&mut self, dest: &mut CssRuleList) {
    // Generate rules for `[dir="ltr"]` and `[dir="rtl"]` containing the declared variables.
    macro_rules! style_rule {
      ($dir: ident) => {
        dest.0.push(CssRule::Style(StyleRule {
          selectors: SelectorList(smallvec::smallvec![
            Selector::from_vec2(vec![
              Component::AttributeInNoNamespace {
                local_name: SelectorIdent("dir".into()),
                operator: AttrSelectorOperator::Equal,
                value: SelectorString(stringify!($dir).into()),
                case_sensitivity: ParsedCaseSensitivity::CaseSensitive,
                never_matches: false
              }
            ])
          ]),
          rules: CssRuleList(vec![]),
          vendor_prefix: VendorPrefix::empty(),
          declarations: DeclarationBlock {
            declarations: std::mem::take(&mut self.$dir)
              .into_values()
              .map(|p| Declaration { property: p, important: false })
              .collect()
          },
          loc: SourceLocation {
            line: 0,
            column: 0
          }
        }));
      };
    }

    if !self.ltr.is_empty() {
      style_rule!(ltr);
    }

    if !self.rtl.is_empty() {
      style_rule!(rtl);
    }
  }
}
