#![allow(non_snake_case)]
use crate::targets::Browsers;
use crate::prefixes::Feature;
use super::Property;
use crate::vendor_prefix::VendorPrefix;
use crate::traits::{PropertyHandler};
use crate::declaration::DeclarationList;

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

    impl PropertyHandler for PrefixHandler {
      fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
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

      fn finalize(&mut self, _: &mut DeclarationList) {}
    }
  };
}

define_prefixes! {
  TransformOrigin,
  TransformStyle,
  BackfaceVisibility,
  Perspective,
  PerspectiveOrigin,
  BoxShadow,
  BoxSizing,
  TabSize,
  Hyphens,
  TextAlignLast,
  TextDecorationSkipInk,
  TextOverflow,
  UserSelect,
  Appearance,
}
