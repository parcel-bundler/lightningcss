#![allow(non_snake_case)]
use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::prefixes::Feature;
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
                      *prefixes = context.targets.prefixes(*prefixes, Feature::$name);
                      return true
                    }
                  }
                }
              }

              // Update the prefixes based on the targets.
              let prefixes = context.targets.prefixes(*prefix, Feature::$name);

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
  PrintColorAdjust,
}

macro_rules! define_fallbacks {
  (
    $( $name: ident$(($p: ident))?, )+
  ) => {
    pastey::paste! {
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
              $(
                $p = context.targets.prefixes($p, Feature::$name);
              )?
              if pastey::paste! { self.[<$name:snake>] }.is_none() {
                let fallbacks = val.get_fallbacks(context.targets);
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

              if pastey::paste! { self.[<$name:snake>] }.is_none() || matches!(context.targets.browsers, Some(targets) if !val.is_compatible(targets)) {
                pastey::paste! { self.[<$name:snake>] = Some(dest.len()) };
                dest.push(Property::$name(val $(, $p)?));
              } else if let Some(index) = pastey::paste! { self.[<$name:snake>] } {
                dest[index] = Property::$name(val $(, $p)?);
              }
            }
          )+
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
                  (val, pastey::paste! { &mut self.[<$name:snake>] })
                }
              )+
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
          pastey::paste! { self.[<$name:snake>] = None };
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
