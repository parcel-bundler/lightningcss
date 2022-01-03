use cssparser::SourceLocation;
use crate::rules::{CssRule, CssRuleList, style::StyleRule};
use parcel_selectors::SelectorList;
use crate::selector::{SelectorIdent, SelectorString};
use crate::declaration::{Declaration, DeclarationBlock};
use crate::css_modules::hash;
use crate::vendor_prefix::VendorPrefix;
use crate::compat::Feature;
use crate::targets::Browsers;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::error::PrinterError;
use parcel_selectors::{
  parser::{Selector, Component},
  attr::{AttrSelectorOperator, ParsedCaseSensitivity}
};
use crate::properties::{
  Property,
  PropertyId,
  custom::CustomProperty,
};

#[derive(Debug)]
pub(crate) struct LogicalProperties {
  targets: Option<Browsers>,
  hash: String,
  count: u32,
  pub used: bool
}

impl LogicalProperties {
  pub fn new(filename: &str, targets: Option<Browsers>) -> LogicalProperties {
    LogicalProperties {
      used: false,
      targets,
      hash: hash(filename),
      count: 0,
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

  pub fn to_rules(&mut self, dest: &mut CssRuleList) {
    // Generate rules for [dir="ltr"] and [dir="rtl"] to define --ltr and --rtl vars.
    macro_rules! style_rule {
      ($dir: ident, $ltr: expr, $rtl: expr) => {
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
            declarations: vec![
              Declaration {
                property: Property::Custom(CustomProperty {
                  name: "--ltr".into(),
                  value: $ltr.into()
                }),
                important: false
              },
              Declaration {
                property: Property::Custom(CustomProperty {
                  name: "--rtl".into(),
                  value: $rtl.into()
                }),
                important: false
              }
            ]
          },
          loc: SourceLocation {
            line: 0,
            column: 0
          }
        }));
      };
    }

    if self.used {
      style_rule!(ltr, "initial", " ");
      style_rule!(rtl, " ", "initial");
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalProperty {
  pub property_id: PropertyId,
  pub ltr: Option<Box<Property>>,
  pub rtl: Option<Box<Property>>
}

impl ToCss for LogicalProperty {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    if let Some(ltr) = &self.ltr {
      dest.write_str("var(--ltr,")?;
      dest.whitespace()?;
      ltr.value_to_css(dest)?;
      dest.write_char(')')?;
    }

    if self.ltr.is_some() && self.rtl.is_some() {
      dest.whitespace()?;
    }

    if let Some(rtl) = &self.rtl {
      dest.write_str("var(--rtl,")?;
      dest.whitespace()?;
      rtl.value_to_css(dest)?;
      dest.write_char(')')?;
    }

    Ok(())
  }
}
