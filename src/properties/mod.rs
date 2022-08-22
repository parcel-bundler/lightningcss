//! CSS property values.
//!
//! Each property provides parsing and serialization support using the [Parse](super::traits::Parse)
//! and [ToCss](super::traits::ToCss) traits. Properties are fully parsed as defined by the CSS spec,
//! and printed in their canonical form. For example, most CSS properties are case-insensitive, and
//! may be written in various orders, but when printed they are lower cased as appropriate and in a
//! standard order.
//!
//! CSS properties often also contain many implicit values that are automatically filled in during
//! parsing when omitted. These are also omitted when possible during serialization. Many properties
//! also implement the [Default](std::default::Default) trait, which returns the initial value for the property.
//!
//! Shorthand properties are represented as structs containing fields for each of the sub-properties.
//! If some of the sub-properties are not specified in the shorthand, their default values are filled in.
//!
//! The [Property](Property) enum contains the values of all properties, and can be used to parse property values by name.
//! The [PropertyId](PropertyId) enum represents only property names, and not values and is used to refer to known properties.
//!
//! # Example
//!
//! This example shows how the `background` shorthand property is parsed and serialized. The `parse_string`
//! function parses the background into a structure with all missing fields filled in with their default values.
//! When printed using the `to_css_string` function, the components are in their canonical order, and default
//! values are removed.
//!
//! ```
//! use smallvec::smallvec;
//! use parcel_css::{
//!   properties::{Property, PropertyId, background::*},
//!   values::{url::Url, image::Image, color::CssColor, position::*, length::*},
//!   stylesheet::{ParserOptions, PrinterOptions},
//!   dependencies::Location,
//! };
//!
//! let background = Property::parse_string(
//!   PropertyId::from("background"),
//!   "url('img.png') repeat fixed 20px 10px / 50px 100px",
//!   ParserOptions::default()
//! ).unwrap();
//!
//! assert_eq!(
//!   background,
//!   Property::Background(smallvec![Background {
//!     image: Image::Url(Url {
//!       url: "img.png".into(),
//!       loc: Location { line: 1, column: 1 }
//!     }),
//!     color: CssColor::RGBA(cssparser::RGBA {
//!       red: 0,
//!       green: 0,
//!       blue: 0,
//!       alpha: 0
//!     }),
//!     position: BackgroundPosition {
//!       x: HorizontalPosition::Length(LengthPercentage::px(20.0)),
//!       y: VerticalPosition::Length(LengthPercentage::px(10.0)),
//!     },
//!     repeat: BackgroundRepeat {
//!       x: BackgroundRepeatKeyword::Repeat,
//!       y: BackgroundRepeatKeyword::Repeat,
//!     },
//!     size: BackgroundSize::Explicit {
//!       width: LengthPercentageOrAuto::LengthPercentage(LengthPercentage::px(50.0)),
//!       height: LengthPercentageOrAuto::LengthPercentage(LengthPercentage::px(100.0)),
//!     },
//!     attachment: BackgroundAttachment::Fixed,
//!     origin: BackgroundOrigin::PaddingBox,
//!     clip: BackgroundClip::BorderBox,
//!   }])
//! );
//!
//! assert_eq!(
//!   background.to_css_string(false, PrinterOptions::default()).unwrap(),
//!   r#"background: url("img.png") 20px 10px / 50px 100px fixed"#
//! );
//! ```
//!
//! If you have a [cssparser::Parser](cssparser::Parser) already, you can also use the `parse` and `to_css`
//! methods instead, rather than parsing from a string.
//!
//! # Unparsed and custom properties
//!
//! Custom and unknown properties are represented by the [CustomProperty](custom::CustomProperty) struct, and the
//! `Property::Custom` variant. The value of these properties is not parsed, and is stored as a raw
//! [TokenList](custom::TokenList), with the name as a string.
//!
//! If a known property is unable to be parsed, e.g. it contains `var()` references, then it is represented by the
//! [UnparsedProperty](custom::UnparsedProperty) struct, and the `Property::Unparsed` variant. The value is stored
//! as a raw [TokenList](custom::TokenList), with a [PropertyId](PropertyId) as the name.

#![deny(missing_docs)]

pub mod align;
pub mod animation;
pub mod background;
pub mod border;
pub mod border_image;
pub mod border_radius;
pub mod box_shadow;
pub mod contain;
pub mod css_modules;
pub mod custom;
pub mod display;
pub mod effects;
pub mod flex;
pub mod font;
#[cfg(feature = "grid")]
pub mod grid;
pub mod list;
pub(crate) mod margin_padding;
pub mod masking;
pub mod outline;
pub mod overflow;
pub mod position;
pub(crate) mod prefix_handler;
pub mod size;
pub mod svg;
pub mod text;
pub mod transform;
pub mod transition;
pub mod ui;

use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::logical::{LogicalGroup, PropertyCategory};
use crate::parser::starts_with_ignore_ascii_case;
use crate::parser::ParserOptions;
use crate::prefixes::Feature;
use crate::printer::{Printer, PrinterOptions};
use crate::targets::Browsers;
use crate::traits::{Parse, ParseWithOptions, Shorthand, ToCss};
use crate::values::number::{CSSInteger, CSSNumber};
use crate::values::string::CowArcStr;
use crate::values::{
  alpha::*, color::*, easing::EasingFunction, ident::DashedIdentReference, image::*, length::*, position::*,
  rect::*, shape::FillRule, size::Size2D, time::Time,
};
use crate::vendor_prefix::VendorPrefix;
use align::*;
use animation::*;
use background::*;
use border::*;
use border_image::*;
use border_radius::*;
use box_shadow::*;
use contain::*;
use css_modules::*;
use cssparser::*;
use custom::*;
use display::*;
use effects::*;
use flex::*;
use font::*;
#[cfg(feature = "grid")]
use grid::*;
use list::*;
use margin_padding::*;
use masking::*;
use outline::*;
use overflow::*;
use size::*;
use smallvec::{smallvec, SmallVec};
use svg::*;
use text::*;
use transform::*;
use transition::*;
use ui::*;

macro_rules! define_properties {
  (
    $(
      $(#[$meta: meta])*
      $name: literal: $property: ident($type: ty $(, $vp: ty)?) $( / $prefix: ident )* $( unprefixed: $unprefixed: literal )? $( options: $options: literal )? $( shorthand: $shorthand: literal )? $( [ logical_group: $logical_group: ident, category: $logical_category: ident ] )? $( if $condition: ident )?,
    )+
  ) => {
    /// A CSS property id.
    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    pub enum PropertyId<'i> {
      $(
        #[doc=concat!("The `", $name, "` property.")]
        $(#[$meta])*
        #[cfg_attr(feature = "serde", serde(rename = $name))]
        $property$(($vp))?,
      )+
      /// The `all` property.
      #[cfg_attr(feature = "serde", serde(rename = "all"))]
      All,
      /// An unknown or custom property name.
      #[cfg_attr(feature = "serde", serde(borrow, rename = "custom"))]
      Custom(CowArcStr<'i>)
    }

    macro_rules! vp_name {
      ($x: ty, $n: ident) => {
        $n
      };
      ($x: ty, $n: expr) => {
        $n
      };
    }

    impl<'i> From<CowArcStr<'i>> for PropertyId<'i> {
      fn from(name: CowArcStr<'i>) -> PropertyId<'i> {
        let name_ref = name.as_ref();
        let (prefix, name_ref) = if starts_with_ignore_ascii_case(name_ref, "-webkit-") {
          (VendorPrefix::WebKit, &name_ref[8..])
        } else if starts_with_ignore_ascii_case(name_ref, "-moz-") {
          (VendorPrefix::Moz, &name_ref[5..])
        } else if starts_with_ignore_ascii_case(name_ref, "-o-") {
          (VendorPrefix::O, &name_ref[3..])
        } else if starts_with_ignore_ascii_case(name_ref, "-ms-") {
          (VendorPrefix::Ms, &name_ref[4..])
        } else {
          (VendorPrefix::None, name_ref)
        };

        macro_rules! get_allowed_prefixes {
          ($v: literal) => {
            VendorPrefix::empty()
          };
          () => {
            VendorPrefix::None
          };
        }

        match_ignore_ascii_case! { name_ref,
          $(
            $(#[$meta])*
            $name => {
              macro_rules! get_propertyid {
                ($v: ty) => {
                  PropertyId::$property(prefix)
                };
                () => {
                  PropertyId::$property
                };
              }

              let allowed_prefixes = get_allowed_prefixes!($($unprefixed)?) $(| VendorPrefix::$prefix)*;
              if allowed_prefixes.contains(prefix) {
                return get_propertyid!($($vp)?)
              }
            },
          )+
          "all" => return PropertyId::All,
          _ => {}
        }
        PropertyId::Custom(name)
      }
    }

    impl<'i> From<&'i str> for PropertyId<'i> {
      #[inline]
      fn from(name: &'i str) -> PropertyId<'i> {
        PropertyId::from(CowArcStr::from(name))
      }
    }

    impl<'i> Parse<'i> for PropertyId<'i> {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let name = input.expect_ident()?;
        Ok(CowArcStr::from(name).into())
      }
    }

    impl<'i> ToCss for PropertyId<'i> {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
        use PropertyId::*;

        let (name, prefix) = match self {
          $(
            $(#[$meta])*
            $property$((vp_name!($vp, prefix)))? => {
              macro_rules! get_prefix {
                ($v: ty) => {
                  *prefix
                };
                () => {
                  VendorPrefix::None
                };
              }

              ($name, get_prefix!($($vp)?))
            },
          )+
          All => ("all", VendorPrefix::None),
          Custom(name) => (name.as_ref(), VendorPrefix::None),
        };

        let mut first = true;
        macro_rules! delim {
          () => {
            #[allow(unused_assignments)]
            if first {
              first = false;
            } else {
              dest.delim(',', false)?;
            }
          };
        }

        macro_rules! write {
          ($p: expr) => {
            if prefix.contains($p) {
              delim!();
              $p.to_css(dest)?;
              dest.write_str(name)?;
            }
          };
        }

        write!(VendorPrefix::WebKit);
        write!(VendorPrefix::Moz);
        write!(VendorPrefix::Ms);
        write!(VendorPrefix::O);
        write!(VendorPrefix::None);
        Ok(())
      }
    }

    impl<'i> PropertyId<'i> {
      fn prefix(&self) -> VendorPrefix {
        use PropertyId::*;
        match self {
          $(
            $(#[$meta])*
            $property$((vp_name!($vp, prefix)))? => {
              $(
                macro_rules! return_prefix {
                  ($v: ty) => {
                    return *prefix;
                  };
                }

                return_prefix!($vp);
              )?
              #[allow(unreachable_code)]
              VendorPrefix::None
            },
          )+
          _ => VendorPrefix::None
        }
      }

      fn with_prefix(&self, prefix: VendorPrefix) -> PropertyId<'i> {
        use PropertyId::*;
        match self {
          $(
            $(#[$meta])*
            $property$((vp_name!($vp, _p)))? => {
              macro_rules! get_prefixed {
                ($v: ty) => {
                  PropertyId::$property(prefix)
                };
                () => {
                  PropertyId::$property
                }
              }

              get_prefixed!($($vp)?)
            },
          )+
          _ => self.clone()
        }
      }

      fn set_prefixes_for_targets(&mut self, targets: Option<Browsers>) {
        match self {
          $(
            $(#[$meta])*
            #[allow(unused_variables)]
            PropertyId::$property$((vp_name!($vp, prefix)))? => {
              macro_rules! get_prefixed {
                ($v: ty, $u: literal) => {};
                ($v: ty) => {{
                  if prefix.contains(VendorPrefix::None) {
                    if let Some(targets) = targets {
                      *prefix = Feature::$property.prefixes_for(targets);
                    }
                  };
                }};
                () => {};
              }

              get_prefixed!($($vp)? $(, $unprefixed)?);
            },
          )+
          _ => {}
        }
      }

      /// Returns the property name, without any vendor prefixes.
      pub fn name(&self) -> &str {
        use PropertyId::*;

        match self {
          $(
            $(#[$meta])*
            $property$((vp_name!($vp, _p)))? => $name,
          )+
          All => "all",
          Custom(name) => &name
        }
      }

      /// Returns whether a property is a shorthand.
      pub fn is_shorthand(&self) -> bool {
        $(
          macro_rules! shorthand {
            ($s: literal) => {
              if let PropertyId::$property$((vp_name!($vp, _prefix)))? = self {
                return true
              }
            };
            () => {}
          }

          shorthand!($($shorthand)?);
        )+

        false
      }

      /// Returns a shorthand value for this property id from the given declaration block.
      pub(crate) fn shorthand_value<'a>(&self, decls: &DeclarationBlock<'a>) -> Option<(Property<'a>, bool)> {
        // Inline function to remap lifetime names.
        #[inline]
        fn shorthand_value<'a, 'i>(property_id: &PropertyId<'a>, decls: &DeclarationBlock<'i>) -> Option<(Property<'i>, bool)> {
          $(
            #[allow(unused_macros)]
            macro_rules! prefix {
              ($v: ty, $p: ident) => {
                *$p
              };
              ($p: ident) => {
                VendorPrefix::None
              };
            }

            macro_rules! shorthand {
              ($s: literal) => {
                if let PropertyId::$property$((vp_name!($vp, prefix)))? = &property_id {
                  if let Some((val, important)) = <$type>::from_longhands(decls, prefix!($($vp,)? prefix)) {
                    return Some((Property::$property(val $(, *vp_name!($vp, prefix))?), important))
                  }
                }
              };
              () => {}
            }

            shorthand!($($shorthand)?);
          )+

          None
        }

        shorthand_value(self, decls)
      }

      /// Returns a list of longhand property ids for a shorthand.
      pub fn longhands(&self) -> Option<Vec<PropertyId<'static>>> {
        macro_rules! prefix_default {
          ($x: ty, $p: ident) => {
            *$p
          };
          () => {
            VendorPrefix::None
          };
        }

        $(
          macro_rules! shorthand {
            ($s: literal) => {
              if let PropertyId::$property$((vp_name!($vp, prefix)))? = self {
                return Some(<$type>::longhands(prefix_default!($($vp, prefix)?)));
              }
            };
            () => {}
          }

          shorthand!($($shorthand)?);
        )+

        None
      }

      /// Returns the logical property group for this property.
      pub(crate) fn logical_group(&self) -> Option<LogicalGroup> {
        $(
          macro_rules! group {
            ($g: ident) => {
              if let PropertyId::$property$((vp_name!($vp, _prefix)))? = self {
                return Some(LogicalGroup::$g)
              }
            };
            () => {}
          }

          group!($($logical_group)?);
        )+

        None
      }

      /// Returns whether the property is logical or physical.
      pub(crate) fn category(&self) -> Option<PropertyCategory> {
        $(
          macro_rules! category {
            ($c: ident) => {
              if let PropertyId::$property$((vp_name!($vp, _prefix)))? = self {
                return Some(PropertyCategory::$c)
              }
            };
            () => {}
          }

          category!($($logical_category)?);
        )+

        None
      }
    }

    /// A CSS property.
    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    #[cfg_attr(feature = "serde", serde(tag = "property", content = "value"))]
    pub enum Property<'i> {
      $(
        #[doc=concat!("The `", $name, "` property.")]
        $(#[$meta])*
        #[cfg_attr(feature = "serde", serde(rename = $name))]
        $property($type, $($vp)?),
      )+
      /// An unparsed property.
      #[cfg_attr(feature = "serde", serde(borrow, rename = "unparsed"))]
      Unparsed(UnparsedProperty<'i>),
      /// A custom or unknown property.
      #[cfg_attr(feature = "serde", serde(borrow, rename = "custom"))]
      Custom(CustomProperty<'i>),
    }

    impl<'i> Property<'i> {
      /// Parses a CSS property by name.
      pub fn parse<'t>(property_id: PropertyId<'i>, input: &mut Parser<'i, 't>, options: &ParserOptions) -> Result<Property<'i>, ParseError<'i, ParserError<'i>>> {
        let state = input.state();

        match property_id {
          $(
            $(#[$meta])*
            PropertyId::$property$((vp_name!($vp, prefix)))? $(if options.$condition.is_some())? => {
              if let Ok(c) = <$type>::parse_with_options(input, options) {
                if input.expect_exhausted().is_ok() {
                  return Ok(Property::$property(c $(, vp_name!($vp, prefix))?))
                }
              }
            },
          )+
          PropertyId::Custom(name) => return Ok(Property::Custom(CustomProperty::parse(name, input, options)?)),
          _ => {}
        };

        // If a value was unable to be parsed, treat as an unparsed property.
        // This is different from a custom property, handled below, in that the property name is known
        // and stored as an enum rather than a string. This lets property handlers more easily deal with it.
        // Ideally we'd only do this if var() or env() references were seen, but err on the safe side for now.
        input.reset(&state);
        return Ok(Property::Unparsed(UnparsedProperty::parse(property_id, input, options)?))
      }

      /// Returns the property id for this property.
      pub fn property_id(&self) -> PropertyId<'i> {
        use Property::*;

        match self {
          $(
            $(#[$meta])*
            $property(_, $(vp_name!($vp, p))?) => PropertyId::$property$((*vp_name!($vp, p)))?,
          )+
          Unparsed(unparsed) => unparsed.property_id.clone(),
          Custom(custom) => PropertyId::Custom(custom.name.clone())
        }
      }

      /// Parses a CSS property from a string.
      pub fn parse_string(property_id: PropertyId<'i>, input: &'i str, options: ParserOptions) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let mut input = ParserInput::new(input);
        let mut parser = Parser::new(&mut input);
        Self::parse(property_id, &mut parser, &options)
      }

      /// Serializes the value of a CSS property without its name or `!important` flag.
      pub fn value_to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
        use Property::*;

        match self {
          $(
            $(#[$meta])*
            $property(val, $(vp_name!($vp, _p))?) => {
              val.to_css(dest)
            }
          )+
          Unparsed(unparsed) => {
            unparsed.value.to_css(dest, false)
          }
          Custom(custom) => {
            custom.value.to_css(dest, custom.name.starts_with("--"))
          }
        }
      }

      /// Serializes the value of a CSS property as a string.
      pub fn value_to_css_string(&self, options: PrinterOptions) -> Result<String, PrinterError> {
        let mut s = String::new();
        let mut printer = Printer::new(&mut s, options);
        self.value_to_css(&mut printer)?;
        Ok(s)
      }

      /// Serializes the CSS property, with an optional `!important` flag.
      pub fn to_css<W>(&self, dest: &mut Printer<W>, important: bool) -> Result<(), PrinterError> where W: std::fmt::Write {
        use Property::*;

        let mut first = true;
        macro_rules! start {
          () => {
            #[allow(unused_assignments)]
            if first {
              first = false;
            } else {
              dest.write_char(';')?;
              dest.newline()?;
            }
          };
        }

        macro_rules! write_important {
          () => {
            if important {
              dest.whitespace()?;
              dest.write_str("!important")?;
            }
          }
        }

        let (name, prefix) = match self {
          $(
            $(#[$meta])*
            $property(_, $(vp_name!($vp, prefix))?) => {
              macro_rules! get_prefix {
                ($v: ty) => {
                  *prefix
                };
                () => {
                  VendorPrefix::None
                };
              }

              ($name, get_prefix!($($vp)?))
            },
          )+
          Unparsed(unparsed) => (unparsed.property_id.name(), unparsed.property_id.prefix()),
          Custom(custom) => {
            // Ensure custom property names are escaped.
            let name = custom.name.as_ref();
            if name.starts_with("--") {
              dest.write_dashed_ident(&name, true)?;
            } else {
              serialize_name(&name, dest)?;
            }
            dest.delim(':', false)?;
            self.value_to_css(dest)?;
            write_important!();
            return Ok(())
          }
        };

        macro_rules! write {
          ($p: expr) => {
            if prefix.contains($p) {
              start!();
              $p.to_css(dest)?;
              dest.write_str(name)?;
              dest.delim(':', false)?;
              self.value_to_css(dest)?;
              write_important!();
            }
          }
        }

        write!(VendorPrefix::WebKit);
        write!(VendorPrefix::Moz);
        write!(VendorPrefix::Ms);
        write!(VendorPrefix::O);
        write!(VendorPrefix::None);
        Ok(())
      }

      /// Serializes the CSS property to a string, with an optional `!important` flag.
      pub fn to_css_string(&self, important: bool, options: PrinterOptions) -> Result<String, PrinterError> {
        let mut s = String::new();
        let mut printer = Printer::new(&mut s, options);
        self.to_css(&mut printer, important)?;
        Ok(s)
      }

      /// Returns the given longhand property for a shorthand.
      pub fn longhand(&self, property_id: &PropertyId) -> Option<Property<'i>> {
        $(
          macro_rules! shorthand {
            ($s: literal) => {
              if let Property::$property(val $(, vp_name!($vp, prefix))?) = self {
                $(
                  if *vp_name!($vp, prefix) != property_id.prefix() {
                    return None
                  }
                )?
                return val.longhand(property_id)
              }
            };
            () => {}
          }

          shorthand!($($shorthand)?);
        )+

        None
      }

      /// Updates this shorthand from a longhand property.
      pub fn set_longhand(&mut self, property: &Property<'i>) -> Result<(), ()> {
        $(
          macro_rules! shorthand {
            ($s: literal) => {
              if let Property::$property(val $(, vp_name!($vp, prefix))?) = self {
                $(
                  if *vp_name!($vp, prefix) != property.property_id().prefix() {
                    return Err(())
                  }
                )?
                return val.set_longhand(property)
              }
            };
            () => {}
          }

          shorthand!($($shorthand)?);
        )+
        Err(())
      }
    }
  };
}

define_properties! {
  "background-color": BackgroundColor(CssColor),
  "background-image": BackgroundImage(SmallVec<[Image<'i>; 1]>),
  "background-position-x": BackgroundPositionX(SmallVec<[HorizontalPosition; 1]>),
  "background-position-y": BackgroundPositionY(SmallVec<[VerticalPosition; 1]>),
  "background-position": BackgroundPosition(SmallVec<[BackgroundPosition; 1]>) shorthand: true,
  "background-size": BackgroundSize(SmallVec<[BackgroundSize; 1]>),
  "background-repeat": BackgroundRepeat(SmallVec<[BackgroundRepeat; 1]>),
  "background-attachment": BackgroundAttachment(SmallVec<[BackgroundAttachment; 1]>),
  "background-clip": BackgroundClip(SmallVec<[BackgroundClip; 1]>, VendorPrefix) / WebKit / Moz,
  "background-origin": BackgroundOrigin(SmallVec<[BackgroundOrigin; 1]>),
  "background": Background(SmallVec<[Background<'i>; 1]>) shorthand: true,

  "box-shadow": BoxShadow(SmallVec<[BoxShadow; 1]>, VendorPrefix) / WebKit / Moz,
  "opacity": Opacity(AlphaValue),
  "color": Color(CssColor),
  "display": Display(Display),
  "visibility": Visibility(Visibility),

  "width": Width(Size) [logical_group: Size, category: Physical],
  "height": Height(Size) [logical_group: Size, category: Physical],
  "min-width": MinWidth(Size) [logical_group: MinSize, category: Physical],
  "min-height": MinHeight(Size) [logical_group: MinSize, category: Physical],
  "max-width": MaxWidth(MaxSize) [logical_group: MaxSize, category: Physical],
  "max-height": MaxHeight(MaxSize) [logical_group: MaxSize, category: Physical],
  "block-size": BlockSize(Size) [logical_group: Size, category: Logical],
  "inline-size": InlineSize(Size) [logical_group: Size, category: Logical],
  "min-block-size": MinBlockSize(Size) [logical_group: MinSize, category: Logical],
  "min-inline-size": MinInlineSize(Size) [logical_group: MinSize, category: Logical],
  "max-block-size": MaxBlockSize(MaxSize) [logical_group: MaxSize, category: Logical],
  "max-inline-size": MaxInlineSize(MaxSize) [logical_group: MaxSize, category: Logical],
  "box-sizing": BoxSizing(BoxSizing, VendorPrefix) / WebKit / Moz,

  "overflow": Overflow(Overflow) shorthand: true,
  "overflow-x": OverflowX(OverflowKeyword),
  "overflow-y": OverflowY(OverflowKeyword),
  "text-overflow": TextOverflow(TextOverflow, VendorPrefix) / O,

  // https://www.w3.org/TR/2020/WD-css-position-3-20200519
  "position": Position(position::Position),
  "top": Top(LengthPercentageOrAuto) [logical_group: Inset, category: Physical],
  "bottom": Bottom(LengthPercentageOrAuto) [logical_group: Inset, category: Physical],
  "left": Left(LengthPercentageOrAuto) [logical_group: Inset, category: Physical],
  "right": Right(LengthPercentageOrAuto) [logical_group: Inset, category: Physical],
  "inset-block-start": InsetBlockStart(LengthPercentageOrAuto) [logical_group: Inset, category: Logical],
  "inset-block-end": InsetBlockEnd(LengthPercentageOrAuto) [logical_group: Inset, category: Logical],
  "inset-inline-start": InsetInlineStart(LengthPercentageOrAuto) [logical_group: Inset, category: Logical],
  "inset-inline-end": InsetInlineEnd(LengthPercentageOrAuto) [logical_group: Inset, category: Logical],
  "inset-block": InsetBlock(InsetBlock) shorthand: true,
  "inset-inline": InsetInline(InsetInline) shorthand: true,
  "inset": Inset(Inset) shorthand: true,

  "border-top-color": BorderTopColor(CssColor) [logical_group: BorderColor, category: Physical],
  "border-bottom-color": BorderBottomColor(CssColor) [logical_group: BorderColor, category: Physical],
  "border-left-color": BorderLeftColor(CssColor) [logical_group: BorderColor, category: Physical],
  "border-right-color": BorderRightColor(CssColor) [logical_group: BorderColor, category: Physical],
  "border-block-start-color": BorderBlockStartColor(CssColor) [logical_group: BorderColor, category: Logical],
  "border-block-end-color": BorderBlockEndColor(CssColor) [logical_group: BorderColor, category: Logical],
  "border-inline-start-color": BorderInlineStartColor(CssColor) [logical_group: BorderColor, category: Logical],
  "border-inline-end-color": BorderInlineEndColor(CssColor) [logical_group: BorderColor, category: Logical],

  "border-top-style": BorderTopStyle(LineStyle) [logical_group: BorderStyle, category: Physical],
  "border-bottom-style": BorderBottomStyle(LineStyle) [logical_group: BorderStyle, category: Physical],
  "border-left-style": BorderLeftStyle(LineStyle) [logical_group: BorderStyle, category: Physical],
  "border-right-style": BorderRightStyle(LineStyle) [logical_group: BorderStyle, category: Physical],
  "border-block-start-style": BorderBlockStartStyle(LineStyle) [logical_group: BorderStyle, category: Logical],
  "border-block-end-style": BorderBlockEndStyle(LineStyle) [logical_group: BorderStyle, category: Logical],
  "border-inline-start-style": BorderInlineStartStyle(LineStyle) [logical_group: BorderStyle, category: Logical],
  "border-inline-end-style": BorderInlineEndStyle(LineStyle) [logical_group: BorderStyle, category: Logical],

  "border-top-width": BorderTopWidth(BorderSideWidth) [logical_group: BorderWidth, category: Physical],
  "border-bottom-width": BorderBottomWidth(BorderSideWidth) [logical_group: BorderWidth, category: Physical],
  "border-left-width": BorderLeftWidth(BorderSideWidth) [logical_group: BorderWidth, category: Physical],
  "border-right-width": BorderRightWidth(BorderSideWidth) [logical_group: BorderWidth, category: Physical],
  "border-block-start-width": BorderBlockStartWidth(BorderSideWidth) [logical_group: BorderWidth, category: Logical],
  "border-block-end-width": BorderBlockEndWidth(BorderSideWidth) [logical_group: BorderWidth, category: Logical],
  "border-inline-start-width": BorderInlineStartWidth(BorderSideWidth) [logical_group: BorderWidth, category: Logical],
  "border-inline-end-width": BorderInlineEndWidth(BorderSideWidth) [logical_group: BorderWidth, category: Logical],

  "border-top-left-radius": BorderTopLeftRadius(Size2D<LengthPercentage>, VendorPrefix) / WebKit / Moz [logical_group: BorderRadius, category: Physical],
  "border-top-right-radius": BorderTopRightRadius(Size2D<LengthPercentage>, VendorPrefix) / WebKit / Moz [logical_group: BorderRadius, category: Physical],
  "border-bottom-left-radius": BorderBottomLeftRadius(Size2D<LengthPercentage>, VendorPrefix) / WebKit / Moz [logical_group: BorderRadius, category: Physical],
  "border-bottom-right-radius": BorderBottomRightRadius(Size2D<LengthPercentage>, VendorPrefix) / WebKit / Moz [logical_group: BorderRadius, category: Physical],
  "border-start-start-radius": BorderStartStartRadius(Size2D<LengthPercentage>) [logical_group: BorderRadius, category: Logical],
  "border-start-end-radius": BorderStartEndRadius(Size2D<LengthPercentage>) [logical_group: BorderRadius, category: Logical],
  "border-end-start-radius": BorderEndStartRadius(Size2D<LengthPercentage>) [logical_group: BorderRadius, category: Logical],
  "border-end-end-radius": BorderEndEndRadius(Size2D<LengthPercentage>) [logical_group: BorderRadius, category: Logical],
  "border-radius": BorderRadius(BorderRadius, VendorPrefix) / WebKit / Moz shorthand: true,

  "border-image-source": BorderImageSource(Image<'i>),
  "border-image-outset": BorderImageOutset(Rect<LengthOrNumber>),
  "border-image-repeat": BorderImageRepeat(BorderImageRepeat),
  "border-image-width": BorderImageWidth(Rect<BorderImageSideWidth>),
  "border-image-slice": BorderImageSlice(BorderImageSlice),
  "border-image": BorderImage(BorderImage<'i>, VendorPrefix) / WebKit / Moz / O shorthand: true,

  "border-color": BorderColor(BorderColor) shorthand: true,
  "border-style": BorderStyle(BorderStyle) shorthand: true,
  "border-width": BorderWidth(BorderWidth) shorthand: true,

  "border-block-color": BorderBlockColor(BorderBlockColor) shorthand: true,
  "border-block-style": BorderBlockStyle(BorderBlockStyle) shorthand: true,
  "border-block-width": BorderBlockWidth(BorderBlockWidth) shorthand: true,

  "border-inline-color": BorderInlineColor(BorderInlineColor) shorthand: true,
  "border-inline-style": BorderInlineStyle(BorderInlineStyle) shorthand: true,
  "border-inline-width": BorderInlineWidth(BorderInlineWidth) shorthand: true,

  "border": Border(Border) shorthand: true,
  "border-top": BorderTop(BorderTop) shorthand: true,
  "border-bottom": BorderBottom(BorderBottom) shorthand: true,
  "border-left": BorderLeft(BorderLeft) shorthand: true,
  "border-right": BorderRight(BorderRight) shorthand: true,
  "border-block": BorderBlock(BorderBlock) shorthand: true,
  "border-block-start": BorderBlockStart(BorderBlockStart) shorthand: true,
  "border-block-end": BorderBlockEnd(BorderBlockEnd) shorthand: true,
  "border-inline": BorderInline(BorderInline) shorthand: true,
  "border-inline-start": BorderInlineStart(BorderInlineStart) shorthand: true,
  "border-inline-end": BorderInlineEnd(BorderInlineEnd) shorthand: true,

  "outline": Outline(Outline) shorthand: true,
  "outline-color": OutlineColor(CssColor),
  "outline-style": OutlineStyle(OutlineStyle),
  "outline-width": OutlineWidth(BorderSideWidth),

  // Flex properties: https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119
  "flex-direction": FlexDirection(FlexDirection, VendorPrefix) / WebKit / Ms,
  "flex-wrap": FlexWrap(FlexWrap, VendorPrefix) / WebKit / Ms,
  "flex-flow": FlexFlow(FlexFlow, VendorPrefix) / WebKit / Ms shorthand: true,
  "flex-grow": FlexGrow(CSSNumber, VendorPrefix) / WebKit,
  "flex-shrink": FlexShrink(CSSNumber, VendorPrefix) / WebKit,
  "flex-basis": FlexBasis(LengthPercentageOrAuto, VendorPrefix) / WebKit,
  "flex": Flex(Flex, VendorPrefix) / WebKit / Ms shorthand: true,
  "order": Order(CSSInteger, VendorPrefix) / WebKit,

  // Align properties: https://www.w3.org/TR/2020/WD-css-align-3-20200421
  "align-content": AlignContent(AlignContent, VendorPrefix) / WebKit,
  "justify-content": JustifyContent(JustifyContent, VendorPrefix) / WebKit,
  "place-content": PlaceContent(PlaceContent) shorthand: true,
  "align-self": AlignSelf(AlignSelf, VendorPrefix) / WebKit,
  "justify-self": JustifySelf(JustifySelf),
  "place-self": PlaceSelf(PlaceSelf) shorthand: true,
  "align-items": AlignItems(AlignItems, VendorPrefix) / WebKit,
  "justify-items": JustifyItems(JustifyItems),
  "place-items": PlaceItems(PlaceItems) shorthand: true,
  "row-gap": RowGap(GapValue),
  "column-gap": ColumnGap(GapValue),
  "gap": Gap(Gap) shorthand: true,

  // Old flex (2009): https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/
  "box-orient": BoxOrient(BoxOrient, VendorPrefix) / WebKit / Moz unprefixed: false,
  "box-direction": BoxDirection(BoxDirection, VendorPrefix) / WebKit / Moz unprefixed: false,
  "box-ordinal-group": BoxOrdinalGroup(CSSInteger, VendorPrefix) / WebKit / Moz unprefixed: false,
  "box-align": BoxAlign(BoxAlign, VendorPrefix) / WebKit / Moz unprefixed: false,
  "box-flex": BoxFlex(CSSNumber, VendorPrefix) / WebKit / Moz unprefixed: false,
  "box-flex-group": BoxFlexGroup(CSSInteger, VendorPrefix) / WebKit unprefixed: false,
  "box-pack": BoxPack(BoxPack, VendorPrefix) / WebKit / Moz unprefixed: false,
  "box-lines": BoxLines(BoxLines, VendorPrefix) / WebKit / Moz unprefixed: false,

  // Old flex (2012): https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/
  "flex-pack": FlexPack(FlexPack, VendorPrefix) / Ms unprefixed: false,
  "flex-order": FlexOrder(CSSInteger, VendorPrefix) / Ms unprefixed: false,
  "flex-align": FlexAlign(BoxAlign, VendorPrefix) / Ms unprefixed: false,
  "flex-item-align": FlexItemAlign(FlexItemAlign, VendorPrefix) / Ms unprefixed: false,
  "flex-line-pack": FlexLinePack(FlexLinePack, VendorPrefix) / Ms unprefixed: false,

  // Microsoft extensions
  "flex-positive": FlexPositive(CSSNumber, VendorPrefix) / Ms unprefixed: false,
  "flex-negative": FlexNegative(CSSNumber, VendorPrefix) / Ms unprefixed: false,
  "flex-preferred-size": FlexPreferredSize(LengthPercentageOrAuto, VendorPrefix) / Ms unprefixed: false,

  #[cfg(feature = "grid")]
  "grid-template-columns": GridTemplateColumns(TrackSizing<'i>),
  #[cfg(feature = "grid")]
  "grid-template-rows": GridTemplateRows(TrackSizing<'i>),
  #[cfg(feature = "grid")]
  "grid-auto-columns": GridAutoColumns(TrackSizeList),
  #[cfg(feature = "grid")]
  "grid-auto-rows": GridAutoRows(TrackSizeList),
  #[cfg(feature = "grid")]
  "grid-auto-flow": GridAutoFlow(GridAutoFlow),
  #[cfg(feature = "grid")]
  "grid-template-areas": GridTemplateAreas(GridTemplateAreas),
  #[cfg(feature = "grid")]
  "grid-template": GridTemplate(GridTemplate<'i>) shorthand: true,
  #[cfg(feature = "grid")]
  "grid": Grid(Grid<'i>) shorthand: true,
  #[cfg(feature = "grid")]
  "grid-row-start": GridRowStart(GridLine<'i>),
  #[cfg(feature = "grid")]
  "grid-row-end": GridRowEnd(GridLine<'i>),
  #[cfg(feature = "grid")]
  "grid-column-start": GridColumnStart(GridLine<'i>),
  #[cfg(feature = "grid")]
  "grid-column-end": GridColumnEnd(GridLine<'i>),
  #[cfg(feature = "grid")]
  "grid-row": GridRow(GridRow<'i>) shorthand: true,
  #[cfg(feature = "grid")]
  "grid-column": GridColumn(GridColumn<'i>) shorthand: true,
  #[cfg(feature = "grid")]
  "grid-area": GridArea(GridArea<'i>) shorthand: true,

  "margin-top": MarginTop(LengthPercentageOrAuto) [logical_group: Margin, category: Physical],
  "margin-bottom": MarginBottom(LengthPercentageOrAuto) [logical_group: Margin, category: Physical],
  "margin-left": MarginLeft(LengthPercentageOrAuto) [logical_group: Margin, category: Physical],
  "margin-right": MarginRight(LengthPercentageOrAuto) [logical_group: Margin, category: Physical],
  "margin-block-start": MarginBlockStart(LengthPercentageOrAuto) [logical_group: Margin, category: Logical],
  "margin-block-end": MarginBlockEnd(LengthPercentageOrAuto) [logical_group: Margin, category: Logical],
  "margin-inline-start": MarginInlineStart(LengthPercentageOrAuto) [logical_group: Margin, category: Logical],
  "margin-inline-end": MarginInlineEnd(LengthPercentageOrAuto) [logical_group: Margin, category: Logical],
  "margin-block": MarginBlock(MarginBlock) shorthand: true,
  "margin-inline": MarginInline(MarginInline) shorthand: true,
  "margin": Margin(Margin) shorthand: true,

  "padding-top": PaddingTop(LengthPercentageOrAuto) [logical_group: Padding, category: Physical],
  "padding-bottom": PaddingBottom(LengthPercentageOrAuto) [logical_group: Padding, category: Physical],
  "padding-left": PaddingLeft(LengthPercentageOrAuto) [logical_group: Padding, category: Physical],
  "padding-right": PaddingRight(LengthPercentageOrAuto) [logical_group: Padding, category: Physical],
  "padding-block-start": PaddingBlockStart(LengthPercentageOrAuto) [logical_group: Padding, category: Logical],
  "padding-block-end": PaddingBlockEnd(LengthPercentageOrAuto) [logical_group: Padding, category: Logical],
  "padding-inline-start": PaddingInlineStart(LengthPercentageOrAuto) [logical_group: Padding, category: Logical],
  "padding-inline-end": PaddingInlineEnd(LengthPercentageOrAuto) [logical_group: Padding, category: Logical],
  "padding-block": PaddingBlock(PaddingBlock) shorthand: true,
  "padding-inline": PaddingInline(PaddingInline) shorthand: true,
  "padding": Padding(Padding) shorthand: true,

  "scroll-margin-top": ScrollMarginTop(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Physical],
  "scroll-margin-bottom": ScrollMarginBottom(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Physical],
  "scroll-margin-left": ScrollMarginLeft(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Physical],
  "scroll-margin-right": ScrollMarginRight(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Physical],
  "scroll-margin-block-start": ScrollMarginBlockStart(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Logical],
  "scroll-margin-block-end": ScrollMarginBlockEnd(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Logical],
  "scroll-margin-inline-start": ScrollMarginInlineStart(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Logical],
  "scroll-margin-inline-end": ScrollMarginInlineEnd(LengthPercentageOrAuto) [logical_group: ScrollMargin, category: Logical],
  "scroll-margin-block": ScrollMarginBlock(ScrollMarginBlock) shorthand: true,
  "scroll-margin-inline": ScrollMarginInline(ScrollMarginInline) shorthand: true,
  "scroll-margin": ScrollMargin(ScrollMargin) shorthand: true,

  "scroll-padding-top": ScrollPaddingTop(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Physical],
  "scroll-padding-bottom": ScrollPaddingBottom(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Physical],
  "scroll-padding-left": ScrollPaddingLeft(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Physical],
  "scroll-padding-right": ScrollPaddingRight(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Physical],
  "scroll-padding-block-start": ScrollPaddingBlockStart(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Logical],
  "scroll-padding-block-end": ScrollPaddingBlockEnd(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Logical],
  "scroll-padding-inline-start": ScrollPaddingInlineStart(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Logical],
  "scroll-padding-inline-end": ScrollPaddingInlineEnd(LengthPercentageOrAuto) [logical_group: ScrollPadding, category: Logical],
  "scroll-padding-block": ScrollPaddingBlock(ScrollPaddingBlock) shorthand: true,
  "scroll-padding-inline": ScrollPaddingInline(ScrollPaddingInline) shorthand: true,
  "scroll-padding": ScrollPadding(ScrollPadding) shorthand: true,

  "font-weight": FontWeight(FontWeight),
  "font-size": FontSize(FontSize),
  "font-stretch": FontStretch(FontStretch),
  "font-family": FontFamily(Vec<FontFamily<'i>>),
  "font-style": FontStyle(FontStyle),
  "font-variant-caps": FontVariantCaps(FontVariantCaps),
  "line-height": LineHeight(LineHeight),
  "font": Font(Font<'i>) shorthand: true,
  "vertical-align": VerticalAlign(VerticalAlign),
  "font-palette": FontPalette(DashedIdentReference<'i>),

  "transition-property": TransitionProperty(SmallVec<[PropertyId<'i>; 1]>, VendorPrefix) / WebKit / Moz / Ms,
  "transition-duration": TransitionDuration(SmallVec<[Time; 1]>, VendorPrefix) / WebKit / Moz / Ms,
  "transition-delay": TransitionDelay(SmallVec<[Time; 1]>, VendorPrefix) / WebKit / Moz / Ms,
  "transition-timing-function": TransitionTimingFunction(SmallVec<[EasingFunction; 1]>, VendorPrefix) / WebKit / Moz / Ms,
  "transition": Transition(SmallVec<[Transition<'i>; 1]>, VendorPrefix) / WebKit / Moz / Ms shorthand: true,

  "animation-name": AnimationName(AnimationNameList<'i>, VendorPrefix) / WebKit / Moz / O,
  "animation-duration": AnimationDuration(SmallVec<[Time; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation-timing-function": AnimationTimingFunction(SmallVec<[EasingFunction; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation-iteration-count": AnimationIterationCount(SmallVec<[AnimationIterationCount; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation-direction": AnimationDirection(SmallVec<[AnimationDirection; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation-play-state": AnimationPlayState(SmallVec<[AnimationPlayState; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation-delay": AnimationDelay(SmallVec<[Time; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation-fill-mode": AnimationFillMode(SmallVec<[AnimationFillMode; 1]>, VendorPrefix) / WebKit / Moz / O,
  "animation": Animation(AnimationList<'i>, VendorPrefix) / WebKit / Moz / O shorthand: true,

  // https://drafts.csswg.org/css-transforms-2/
  "transform": Transform(TransformList, VendorPrefix) / WebKit / Moz / Ms / O,
  "transform-origin": TransformOrigin(Position, VendorPrefix) / WebKit / Moz / Ms / O, // TODO: handle z offset syntax
  "transform-style": TransformStyle(TransformStyle, VendorPrefix) / WebKit / Moz,
  "transform-box": TransformBox(TransformBox),
  "backface-visibility": BackfaceVisibility(BackfaceVisibility, VendorPrefix) / WebKit / Moz,
  "perspective": Perspective(Perspective, VendorPrefix) / WebKit / Moz,
  "perspective-origin": PerspectiveOrigin(Position, VendorPrefix) / WebKit / Moz,
  "translate": Translate(Translate),
  "rotate": Rotate(Rotate),
  "scale": Scale(Scale),

  // https://www.w3.org/TR/2021/CRD-css-text-3-20210422
  "text-transform": TextTransform(TextTransform),
  "white-space": WhiteSpace(WhiteSpace),
  "tab-size": TabSize(LengthOrNumber, VendorPrefix) / Moz / O,
  "word-break": WordBreak(WordBreak),
  "line-break": LineBreak(LineBreak),
  "hyphens": Hyphens(Hyphens, VendorPrefix) / WebKit / Moz / Ms,
  "overflow-wrap": OverflowWrap(OverflowWrap),
  "word-wrap": WordWrap(OverflowWrap),
  "text-align": TextAlign(TextAlign),
  "text-align-last": TextAlignLast(TextAlignLast, VendorPrefix) / Moz,
  "text-justify": TextJustify(TextJustify),
  "word-spacing": WordSpacing(Spacing),
  "letter-spacing": LetterSpacing(Spacing),
  "text-indent": TextIndent(TextIndent),

  // https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506
  "text-decoration-line": TextDecorationLine(TextDecorationLine, VendorPrefix) / WebKit / Moz,
  "text-decoration-style": TextDecorationStyle(TextDecorationStyle, VendorPrefix) / WebKit / Moz,
  "text-decoration-color": TextDecorationColor(CssColor, VendorPrefix) / WebKit / Moz,
  "text-decoration-thickness": TextDecorationThickness(TextDecorationThickness),
  "text-decoration": TextDecoration(TextDecoration, VendorPrefix) / WebKit / Moz shorthand: true,
  "text-decoration-skip-ink": TextDecorationSkipInk(TextDecorationSkipInk, VendorPrefix) / WebKit,
  "text-emphasis-style": TextEmphasisStyle(TextEmphasisStyle<'i>, VendorPrefix) / WebKit,
  "text-emphasis-color": TextEmphasisColor(CssColor, VendorPrefix) / WebKit,
  "text-emphasis": TextEmphasis(TextEmphasis<'i>, VendorPrefix) / WebKit shorthand: true,
  "text-emphasis-position": TextEmphasisPosition(TextEmphasisPosition, VendorPrefix) / WebKit,
  "text-shadow": TextShadow(SmallVec<[TextShadow; 1]>),

  // https://www.w3.org/TR/css-break-3/
  "box-decoration-break": BoxDecorationBreak(BoxDecorationBreak, VendorPrefix) / WebKit,

  // https://www.w3.org/TR/2021/WD-css-ui-4-20210316
  "resize": Resize(Resize),
  "cursor": Cursor(Cursor<'i>),
  "caret-color": CaretColor(ColorOrAuto),
  "caret-shape": CaretShape(CaretShape),
  "caret": Caret(Caret) shorthand: true,
  "user-select": UserSelect(UserSelect, VendorPrefix) / WebKit / Moz / Ms,
  "accent-color": AccentColor(ColorOrAuto),
  "appearance": Appearance(Appearance<'i>, VendorPrefix) / WebKit / Moz / Ms,

  // https://www.w3.org/TR/2020/WD-css-lists-3-20201117
  "list-style-type": ListStyleType(ListStyleType<'i>),
  "list-style-image": ListStyleImage(Image<'i>),
  "list-style-position": ListStylePosition(ListStylePosition),
  "list-style": ListStyle(ListStyle<'i>) shorthand: true,
  "marker-side": MarkerSide(MarkerSide),

  // CSS modules
  "composes": Composes(Composes<'i>) if css_modules,

  // https://www.w3.org/TR/SVG2/painting.html
  "fill": Fill(SVGPaint<'i>),
  "fill-rule": FillRule(FillRule),
  "fill-opacity": FillOpacity(AlphaValue),
  "stroke": Stroke(SVGPaint<'i>),
  "stroke-opacity": StrokeOpacity(AlphaValue),
  "stroke-width": StrokeWidth(LengthPercentage),
  "stroke-linecap": StrokeLinecap(StrokeLinecap),
  "stroke-linejoin": StrokeLinejoin(StrokeLinejoin),
  "stroke-miterlimit": StrokeMiterlimit(CSSNumber),
  "stroke-dasharray": StrokeDasharray(StrokeDasharray),
  "stroke-dashoffset": StrokeDashoffset(LengthPercentage),
  "marker-start": MarkerStart(Marker<'i>),
  "marker-mid": MarkerMid(Marker<'i>),
  "marker-end": MarkerEnd(Marker<'i>),
  "marker": Marker(Marker<'i>),
  "color-interpolation": ColorInterpolation(ColorInterpolation),
  "color-interpolation-filters": ColorInterpolationFilters(ColorInterpolation),
  "color-rendering": ColorRendering(ColorRendering),
  "shape-rendering": ShapeRendering(ShapeRendering),
  "text-rendering": TextRendering(TextRendering),
  "image-rendering": ImageRendering(ImageRendering),

  // https://www.w3.org/TR/css-masking-1/
  "clip-path": ClipPath(ClipPath<'i>, VendorPrefix) / WebKit,
  "clip-rule": ClipRule(FillRule),
  "mask-image": MaskImage(SmallVec<[Image<'i>; 1]>, VendorPrefix) / WebKit,
  "mask-mode": MaskMode(SmallVec<[MaskMode; 1]>),
  "mask-repeat": MaskRepeat(SmallVec<[BackgroundRepeat; 1]>, VendorPrefix) / WebKit,
  "mask-position-x": MaskPositionX(SmallVec<[HorizontalPosition; 1]>),
  "mask-position-y": MaskPositionY(SmallVec<[VerticalPosition; 1]>),
  "mask-position": MaskPosition(SmallVec<[Position; 1]>, VendorPrefix) / WebKit,
  "mask-clip": MaskClip(SmallVec<[MaskClip; 1]>, VendorPrefix) / WebKit,
  "mask-origin": MaskOrigin(SmallVec<[GeometryBox; 1]>, VendorPrefix) / WebKit,
  "mask-size": MaskSize(SmallVec<[BackgroundSize; 1]>, VendorPrefix) / WebKit,
  "mask-composite": MaskComposite(SmallVec<[MaskComposite; 1]>),
  "mask-type": MaskType(MaskType),
  "mask": Mask(SmallVec<[Mask<'i>; 1]>, VendorPrefix) / WebKit shorthand: true,
  "mask-border-source": MaskBorderSource(Image<'i>),
  "mask-border-mode": MaskBorderMode(MaskBorderMode),
  "mask-border-slice": MaskBorderSlice(BorderImageSlice),
  "mask-border-width": MaskBorderWidth(Rect<BorderImageSideWidth>),
  "mask-border-outset": MaskBorderOutset(Rect<LengthOrNumber>),
  "mask-border-repeat": MaskBorderRepeat(BorderImageRepeat),
  "mask-border": MaskBorder(MaskBorder<'i>) shorthand: true,

  // WebKit additions
  "-webkit-mask-composite": WebKitMaskComposite(SmallVec<[WebKitMaskComposite; 1]>),
  "mask-source-type": WebKitMaskSourceType(SmallVec<[WebKitMaskSourceType; 1]>, VendorPrefix) / WebKit unprefixed: false,
  "mask-box-image": WebKitMaskBoxImage(BorderImage<'i>, VendorPrefix) / WebKit unprefixed: false,
  "mask-box-image-source": WebKitMaskBoxImageSource(Image<'i>, VendorPrefix) / WebKit unprefixed: false,
  "mask-box-image-slice": WebKitMaskBoxImageSlice(BorderImageSlice, VendorPrefix) / WebKit unprefixed: false,
  "mask-box-image-width": WebKitMaskBoxImageWidth(Rect<BorderImageSideWidth>, VendorPrefix) / WebKit unprefixed: false,
  "mask-box-image-outset": WebKitMaskBoxImageOutset(Rect<LengthOrNumber>, VendorPrefix) / WebKit unprefixed: false,
  "mask-box-image-repeat": WebKitMaskBoxImageRepeat(BorderImageRepeat, VendorPrefix) / WebKit unprefixed: false,

  // https://drafts.fxtf.org/filter-effects-1/
  "filter": Filter(FilterList<'i>, VendorPrefix) / WebKit,
  "backdrop-filter": BackdropFilter(FilterList<'i>, VendorPrefix) / WebKit,

  // https://drafts.csswg.org/css2/
  "z-index": ZIndex(position::ZIndex),

  // https://drafts.csswg.org/css-contain-3/
  "container-type": ContainerType(ContainerType),
  "container-name": ContainerName(ContainerNameList<'i>),
  "container": Container(Container<'i>) shorthand: true,
}

impl<'i, T: smallvec::Array<Item = V>, V: Parse<'i>> Parse<'i> for SmallVec<T> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Copied from cssparser `parse_comma_separated` but using SmallVec instead of Vec.
    let mut values = smallvec![];
    loop {
      input.skip_whitespace(); // Unnecessary for correctness, but may help try() in parse_one rewind less.
      match input.parse_until_before(Delimiter::Comma, &mut V::parse) {
        Ok(v) => values.push(v),
        Err(err) => return Err(err),
      }
      match input.next() {
        Err(_) => return Ok(values),
        Ok(&Token::Comma) => continue,
        Ok(_) => unreachable!(),
      }
    }
  }
}

impl<T: smallvec::Array<Item = V>, V: ToCss> ToCss for SmallVec<T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let len = self.len();
    for (idx, val) in self.iter().enumerate() {
      val.to_css(dest)?;
      if idx < len - 1 {
        dest.delim(',', false)?;
      }
    }
    Ok(())
  }
}

impl<'i, T: Parse<'i>> Parse<'i> for Vec<T> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_comma_separated(|input| T::parse(input))
  }
}

impl<T: ToCss> ToCss for Vec<T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let len = self.len();
    for (idx, val) in self.iter().enumerate() {
      val.to_css(dest)?;
      if idx < len - 1 {
        dest.delim(',', false)?;
      }
    }
    Ok(())
  }
}
