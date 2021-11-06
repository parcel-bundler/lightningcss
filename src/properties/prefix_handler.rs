#![allow(non_snake_case)]
use super::prefixes::{Browsers, Feature};
use super::{Property, VendorPrefix};
use crate::traits::{PropertyHandler};

macro_rules! define_prefixes {
  (
    $( $name: ident, )+
  ) => {
    #[derive(Default)]
    pub struct PrefixHandler {
      targets: Option<Browsers>,
      $(
        $name: Option<Property>,
      )+
      decls: Vec<Property>
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
      fn handle_property(&mut self, property: &Property) -> bool {
        match property {
          $(
            Property::$name(val, prefix) => {
              // If two vendor prefixes for the same property have different
              // values, we need to flush what we have immediately to preserve order.
              if let Some(Property::$name(cur, prefixes)) = &self.$name {
                if val != cur && !prefixes.contains(*prefix) {
                  self.flush();
                }
              }

              // Otherwise, update the value and add the prefix.
              if let Some(Property::$name(val, prefixes)) = &mut self.$name {
                *val = val.clone();
                *prefixes |= *prefix;
                if prefixes.contains(VendorPrefix::None) {
                  if let Some(targets) = self.targets {
                    *prefixes = Feature::$name.prefixes_for(targets);
                  }
                }
              } else {
                let prefixes = if prefix.contains(VendorPrefix::None) {
                  if let Some(targets) = self.targets {
                    Feature::$name.prefixes_for(targets)
                  } else {
                    *prefix
                  }
                } else {
                  *prefix
                };
                self.$name = Some(Property::$name(val.clone(), prefixes))
              }
            }
          )+
          _ => return false
        }

        true
      }

      fn finalize(&mut self) -> Vec<Property> {
        self.flush();
        std::mem::take(&mut self.decls)
      }
    }

    impl PrefixHandler {
      fn flush(&mut self) {
        $(
          let $name = std::mem::take(&mut self.$name);
          if let Some(p) = $name {
            self.decls.push(p)
          }
        )+
      }
    }
  };
}

define_prefixes! {
  Transform,
  TransformOrigin,
  TransformStyle,
  BackfaceVisibility,
  Perspective,
  PerspectiveOrigin,
}
