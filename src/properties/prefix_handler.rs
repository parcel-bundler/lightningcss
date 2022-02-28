#![allow(non_snake_case)]
use crate::targets::Browsers;
use crate::prefixes::Feature;
use super::Property;
use crate::vendor_prefix::VendorPrefix;
use crate::traits::{PropertyHandler, FallbackValues};
use crate::declaration::DeclarationList;
use crate::logical::LogicalProperties;
use crate::properties::custom::CustomProperty;

macro_rules! define_prefixes {
  (
    $( $name: ident, )+
  ) => {
    #[derive(Default)]
    pub(crate) struct PrefixHandler {
      targets: Option<Browsers>,
      $(
        $name: Option<usize>,
      )+
    }

    impl PrefixHandler {
      pub fn new(targets: Option<Browsers>) -> PrefixHandler {
        PrefixHandler {
          targets,
          ..PrefixHandler::default()
        }
      }
    }

    impl<'i> PropertyHandler<'i> for PrefixHandler {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, _: &mut LogicalProperties) -> bool {
        match property {
          $(
            Property::$name(val, prefix) => {
              if let Some(i) = self.$name {
                if let Some(decl) = dest.get_mut(i) {
                  if let Property::$name(cur, prefixes) = decl {
                    // If the value is the same, update the prefix.
                    // If the prefix is the same, then update the value.
                    if val == cur || prefixes.contains(*prefix) {
                      *cur = val.clone();
                      *prefixes |= *prefix;
                      if prefixes.contains(VendorPrefix::None) {
                        if let Some(targets) = self.targets {
                          *prefixes = Feature::$name.prefixes_for(targets);
                        }
                      }

                      return true
                    }
                  }
                }
              }

              // Update the prefixes based on the targets.
              let prefixes = if prefix.contains(VendorPrefix::None) {
                if let Some(targets) = self.targets {
                  Feature::$name.prefixes_for(targets)
                } else {
                  *prefix
                }
              } else {
                *prefix
              };

              // Store the index of the property, so we can update it later.
              self.$name = Some(dest.len());
              dest.push(Property::$name(val.clone(), prefixes))
            }
          )+
          _ => return false
        }

        true
      }

      fn finalize(&mut self, _: &mut DeclarationList, _: &mut LogicalProperties) {}
    }
  };
}

define_prefixes! {
  TransformOrigin,
  TransformStyle,
  BackfaceVisibility,
  Perspective,
  PerspectiveOrigin,
  BoxSizing,
  TabSize,
  Hyphens,
  TextAlignLast,
  TextDecorationSkipInk,
  TextOverflow,
  UserSelect,
  Appearance,
}

macro_rules! define_fallbacks {
  (
    $( $name: ident, )+
  ) => {
    #[derive(Default)]
    pub(crate) struct FallbackHandler {
      targets: Option<Browsers>
    }

    impl FallbackHandler {
      pub fn new(targets: Option<Browsers>) -> FallbackHandler {
        FallbackHandler {
          targets
        }
      }
    }

    impl<'i> PropertyHandler<'i> for FallbackHandler {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, logical: &mut LogicalProperties<'i>) -> bool {
        match property {
          $(
            Property::$name(val) => {
              let mut val = val.clone();
              if let Some(targets) = self.targets {
                let fallbacks = val.get_fallbacks(targets);
                for fallback in fallbacks {
                  dest.push(Property::$name(fallback.clone()))
                }
              }

              dest.push(Property::$name(val))
            }
          )+
          Property::Custom(custom) => {
            let mut custom = custom.clone();
            if logical.in_style_rule {
              if let Some(targets) = self.targets {
                let fallbacks = custom.value.get_fallbacks(targets);
                for (condition, fallback) in fallbacks {
                  logical.add_conditional_property(
                    condition,
                    Property::Custom(CustomProperty {
                      name: custom.name.clone(),
                      value: fallback
                    })
                  );
                }
              }
            }

            dest.push(Property::Custom(custom))
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, _: &mut DeclarationList, _: &mut LogicalProperties) {}
    }
  };
}

define_fallbacks! {
  Color,
  TextShadow,
  Filter,
  Fill,
  Stroke,
  CaretColor,
  Caret,
  MaskImage,
  Mask,
}
