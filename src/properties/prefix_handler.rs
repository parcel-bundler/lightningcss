#![allow(non_snake_case)]
use super::{Property, PropertyId};
use crate::context::{DeclarationContext, PropertyHandlerContext};
use crate::declaration::DeclarationList;
use crate::prefixes::Feature;
use crate::properties::custom::CustomProperty;
use crate::traits::{FallbackValues, IsCompatible, PropertyHandler};
use crate::vendor_prefix::VendorPrefix;

macro_rules! define_prefixes {
  (
    $( $name: ident, )+
  ) => {
    #[derive(Default)]
    pub(crate) struct PrefixHandler {
      $(
        $name: Option<usize>,
      )+
    }

    impl<'i> PropertyHandler<'i> for PrefixHandler {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext) -> bool {
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
                        if let Some(targets) = context.targets {
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
                if let Some(targets) = context.targets {
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

      fn finalize(&mut self, _: &mut DeclarationList, _: &mut PropertyHandlerContext) {}
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
  ClipPath,
  BoxDecorationBreak,
  TextSizeAdjust,
}

macro_rules! define_fallbacks {
  (
    $( $name: ident$(($p: ident))?, )+
  ) => {
    paste::paste! {
      #[derive(Default)]
      pub(crate) struct FallbackHandler {
        $(
          [<$name:snake>]: Option<usize>
        ),+
      }
    }

    impl<'i> PropertyHandler<'i> for FallbackHandler {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) -> bool {
        match property {
          $(
            Property::$name(val $(, mut $p)?) => {
              let mut val = val.clone();
              if let (Some(targets), None) = (context.targets, paste::paste! { self.[<$name:snake>] }) {
                $(
                  if $p.contains(VendorPrefix::None) {
                    $p = Feature::$name.prefixes_for(targets);
                  }
                )?

                let fallbacks = val.get_fallbacks(targets);
                #[allow(unused_variables)]
                let has_fallbacks = !fallbacks.is_empty();
                for fallback in fallbacks {
                  dest.push(Property::$name(fallback $(, $p)?))
                }

                $(
                  if has_fallbacks && $p.contains(VendorPrefix::None) {
                    $p = VendorPrefix::None;
                  }
                )?
              }

              if paste::paste! { self.[<$name:snake>] }.is_none() || matches!(context.targets, Some(targets) if !val.is_compatible(targets)) {
                paste::paste! { self.[<$name:snake>] = Some(dest.len()) };
                dest.push(Property::$name(val $(, $p)?));
              } else if let Some(index) = paste::paste! { self.[<$name:snake>] } {
                dest[index] = Property::$name(val $(, $p)?);
              }
            }
          )+
          Property::Custom(custom) => {
            let mut custom = custom.clone();
            if context.context != DeclarationContext::Keyframes {
              if let Some(targets) = context.targets {
                let fallbacks = custom.value.get_fallbacks(targets);
                for (condition, fallback) in fallbacks {
                  context.add_conditional_property(
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
          Property::Unparsed(val) => {
            let (mut unparsed, index) = match val.property_id {
              $(
                PropertyId::$name$(($p))? => {
                  macro_rules! get_prefixed {
                    ($vp: ident) => {
                      if $vp.contains(VendorPrefix::None) {
                        val.get_prefixed(context.targets, Feature::$name)
                      } else {
                        val.clone()
                      }
                    };
                    () => {
                      val.clone()
                    };
                  }

                  let val = get_prefixed!($($p)?);
                  (val, paste::paste! { &mut self.[<$name:snake>] })
                }
              )+
              PropertyId::All => {
                let mut unparsed = val.clone();
                context.add_unparsed_fallbacks(&mut unparsed);
                dest.push(Property::Unparsed(unparsed));
                return true
              },
              _ => return false
            };

            // Unparsed properties are always "valid", meaning they always override previous declarations.
            context.add_unparsed_fallbacks(&mut unparsed);
            if let Some(index) = *index {
              dest[index] = Property::Unparsed(unparsed);
            } else {
              *index = Some(dest.len());
              dest.push(Property::Unparsed(unparsed));
            }
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, _: &mut DeclarationList, _: &mut PropertyHandlerContext) {
        $(
          paste::paste! { self.[<$name:snake>] = None };
        )+
      }
    }
  };
}

define_fallbacks! {
  Color,
  TextShadow,
  Filter(prefix),
  BackdropFilter(prefix),
  Fill,
  Stroke,
  CaretColor,
  Caret,
}
