//! CSS properties related to box sizing.

use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::traits::{Parse, PropertyHandler, ToCss};
use crate::values::length::LengthPercentage;
use crate::vendor_prefix::VendorPrefix;
use cssparser::*;

// https://drafts.csswg.org/css-sizing-3/#specifying-sizes
// https://www.w3.org/TR/css-sizing-4/#sizing-values

/// A value for the [preferred size properties](https://drafts.csswg.org/css-sizing-3/#preferred-size-properties),
/// i.e. `width` and `height.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Size {
  /// The `auto` keyword.
  Auto,
  /// An explicit length or percentage.
  LengthPercentage(LengthPercentage),
  /// The `min-content` keyword.
  MinContent(VendorPrefix),
  /// The `max-content` keyword.
  MaxContent(VendorPrefix),
  /// The `fit-content` keyword.
  FitContent(VendorPrefix),
  /// The `fit-content()` function.
  FitContentFunction(LengthPercentage),
  /// The `stretch` keyword, or the `-webkit-fill-available` or `-moz-available` prefixed keywords.
  Stretch(VendorPrefix),
  /// The `contain` keyword.
  Contain,
}

impl<'i> Parse<'i> for Size {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let res = input.try_parse(|input| {
      let ident = input.expect_ident()?;
      Ok(match_ignore_ascii_case! { &*ident,
        "auto" => Size::Auto,
        "min-content" => Size::MinContent(VendorPrefix::None),
        "-webkit-min-content" => Size::MinContent(VendorPrefix::WebKit),
        "-moz-min-content" => Size::MinContent(VendorPrefix::Moz),
        "max-content" => Size::MaxContent(VendorPrefix::None),
        "-webkit-max-content" => Size::MaxContent(VendorPrefix::WebKit),
        "-moz-max-content" => Size::MaxContent(VendorPrefix::Moz),
        "stretch" => Size::Stretch(VendorPrefix::None),
        "-webkit-fill-available" => Size::Stretch(VendorPrefix::WebKit),
        "-moz-available" => Size::Stretch(VendorPrefix::Moz),
        "fit-content" => Size::FitContent(VendorPrefix::None),
        "-webkit-fit-content" => Size::FitContent(VendorPrefix::WebKit),
        "-moz-fit-content" => Size::FitContent(VendorPrefix::Moz),
        "contain" => Size::Contain,
        _ => return Err(input.new_custom_error(ParserError::InvalidValue))
      })
    });

    if res.is_ok() {
      return res;
    }

    if let Ok(res) = input.try_parse(parse_fit_content) {
      return Ok(Size::FitContentFunction(res));
    }

    let lp = input.try_parse(LengthPercentage::parse)?;
    Ok(Size::LengthPercentage(lp))
  }
}

impl ToCss for Size {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use Size::*;
    match self {
      Auto => dest.write_str("auto"),
      Contain => dest.write_str("contain"),
      MinContent(vp) => {
        vp.to_css(dest)?;
        dest.write_str("min-content")
      }
      MaxContent(vp) => {
        vp.to_css(dest)?;
        dest.write_str("max-content")
      }
      FitContent(vp) => {
        vp.to_css(dest)?;
        dest.write_str("fit-content")
      }
      Stretch(vp) => match *vp {
        VendorPrefix::None => dest.write_str("stretch"),
        VendorPrefix::WebKit => dest.write_str("-webkit-fill-available"),
        VendorPrefix::Moz => dest.write_str("-moz-available"),
        _ => unreachable!(),
      },
      FitContentFunction(l) => {
        dest.write_str("fit-content(")?;
        l.to_css(dest)?;
        dest.write_str(")")
      }
      LengthPercentage(l) => l.to_css(dest),
    }
  }
}

/// A value for the [minimum](https://drafts.csswg.org/css-sizing-3/#min-size-properties)
/// and [maximum](https://drafts.csswg.org/css-sizing-3/#max-size-properties) size properties,
/// e.g. `min-width` and `max-height`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MaxSize {
  /// The `none` keyword.
  None,
  /// An explicit length or percentage.
  LengthPercentage(LengthPercentage),
  /// The `min-content` keyword.
  MinContent(VendorPrefix),
  /// The `max-content` keyword.
  MaxContent(VendorPrefix),
  /// The `fit-content` keyword.
  FitContent(VendorPrefix),
  /// The `fit-content()` function.
  FitContentFunction(LengthPercentage),
  /// The `stretch` keyword, or the `-webkit-fill-available` or `-moz-available` prefixed keywords.
  Stretch(VendorPrefix),
  /// The `contain` keyword.
  Contain,
}

impl<'i> Parse<'i> for MaxSize {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let res = input.try_parse(|input| {
      let ident = input.expect_ident()?;
      Ok(match_ignore_ascii_case! { &*ident,
        "none" => MaxSize::None,
        "min-content" => MaxSize::MinContent(VendorPrefix::None),
        "-webkit-min-content" => MaxSize::MinContent(VendorPrefix::WebKit),
        "-moz-min-content" => MaxSize::MinContent(VendorPrefix::Moz),
        "max-content" => MaxSize::MaxContent(VendorPrefix::None),
        "-webkit-max-content" => MaxSize::MaxContent(VendorPrefix::WebKit),
        "-moz-max-content" => MaxSize::MaxContent(VendorPrefix::Moz),
        "stretch" => MaxSize::Stretch(VendorPrefix::None),
        "-webkit-fill-available" => MaxSize::Stretch(VendorPrefix::WebKit),
        "-moz-available" => MaxSize::Stretch(VendorPrefix::Moz),
        "fit-content" => MaxSize::FitContent(VendorPrefix::None),
        "-webkit-fit-content" => MaxSize::FitContent(VendorPrefix::WebKit),
        "-moz-fit-content" => MaxSize::FitContent(VendorPrefix::Moz),
        "contain" => MaxSize::Contain,
        _ => return Err(input.new_custom_error(ParserError::InvalidValue))
      })
    });

    if res.is_ok() {
      return res;
    }

    if let Ok(res) = input.try_parse(parse_fit_content) {
      return Ok(MaxSize::FitContentFunction(res));
    }

    let lp = input.try_parse(LengthPercentage::parse)?;
    Ok(MaxSize::LengthPercentage(lp))
  }
}

impl ToCss for MaxSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use MaxSize::*;
    match self {
      None => dest.write_str("none"),
      Contain => dest.write_str("contain"),
      MinContent(vp) => {
        vp.to_css(dest)?;
        dest.write_str("min-content")
      }
      MaxContent(vp) => {
        vp.to_css(dest)?;
        dest.write_str("max-content")
      }
      FitContent(vp) => {
        vp.to_css(dest)?;
        dest.write_str("fit-content")
      }
      Stretch(vp) => match *vp {
        VendorPrefix::None => dest.write_str("stretch"),
        VendorPrefix::WebKit => dest.write_str("-webkit-fill-available"),
        VendorPrefix::Moz => dest.write_str("-moz-available"),
        _ => unreachable!(),
      },
      FitContentFunction(l) => {
        dest.write_str("fit-content(")?;
        l.to_css(dest)?;
        dest.write_str(")")
      }
      LengthPercentage(l) => l.to_css(dest),
    }
  }
}

fn parse_fit_content<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<LengthPercentage, ParseError<'i, ParserError<'i>>> {
  input.expect_function_matching("fit-content")?;
  input.parse_nested_block(|input| LengthPercentage::parse(input))
}

enum_property! {
  /// A value for the [box-sizing](https://drafts.csswg.org/css-sizing-3/#box-sizing) property.
  pub enum BoxSizing {
    /// Exclude the margin/border/padding from the width and height.
    "content-box": ContentBox,
    /// Include the padding and border (but not the margin) in the width and height.
    "border-box": BorderBox,
  }
}

#[derive(Default)]
pub(crate) struct SizeHandler;

impl<'i> PropertyHandler<'i> for SizeHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    let logical_supported = context.is_supported(Feature::LogicalSize);

    macro_rules! prefix {
      ($prop: ident, $size: ident, $feature: ident) => {
        if let Some(targets) = context.targets {
          let prefixes = crate::prefixes::Feature::$feature.prefixes_for(targets);
          if prefixes.contains(VendorPrefix::WebKit) {
            dest.push(Property::$prop($size::$feature(VendorPrefix::WebKit)));
          }
          if prefixes.contains(VendorPrefix::Moz) {
            dest.push(Property::$prop($size::$feature(VendorPrefix::Moz)));
          }
        }
      };
    }

    macro_rules! property {
      ($prop: ident, $val: ident, $size: ident) => {{
        match $val {
          $size::Stretch(VendorPrefix::None) => prefix!($prop, $size, Stretch),
          $size::MinContent(VendorPrefix::None) => prefix!($prop, $size, MinContent),
          $size::MaxContent(VendorPrefix::None) => prefix!($prop, $size, MaxContent),
          $size::FitContent(VendorPrefix::None) => prefix!($prop, $size, FitContent),
          _ => {}
        }
        dest.push(Property::$prop($val.clone()));
      }};
    }

    macro_rules! logical {
      ($prop: ident, $val: ident, $physical: ident, $size: ident) => {
        if logical_supported {
          property!($prop, $val, $size);
        } else {
          property!($physical, $val, $size);
        }
      };
    }

    match property {
      Property::Width(v) => property!(Width, v, Size),
      Property::Height(v) => property!(Height, v, Size),
      Property::MinWidth(v) => property!(MinWidth, v, Size),
      Property::MinHeight(v) => property!(MinHeight, v, Size),
      Property::MaxWidth(v) => property!(MaxWidth, v, MaxSize),
      Property::MaxHeight(v) => property!(MaxHeight, v, MaxSize),
      Property::BlockSize(size) => logical!(BlockSize, size, Height, Size),
      Property::MinBlockSize(size) => logical!(MinBlockSize, size, MinHeight, Size),
      Property::MaxBlockSize(size) => logical!(MaxBlockSize, size, MaxHeight, MaxSize),
      Property::InlineSize(size) => logical!(InlineSize, size, Width, Size),
      Property::MinInlineSize(size) => logical!(MinInlineSize, size, MinWidth, Size),
      Property::MaxInlineSize(size) => logical!(MaxInlineSize, size, MaxWidth, MaxSize),
      Property::Unparsed(unparsed) => {
        macro_rules! logical_unparsed {
          ($physical: ident) => {
            if logical_supported {
              dest.push(property.clone());
            } else {
              dest.push(Property::Unparsed(
                unparsed.with_property_id(PropertyId::$physical),
              ));
            }
          };
        }

        match &unparsed.property_id {
          PropertyId::Width
          | PropertyId::Height
          | PropertyId::MinWidth
          | PropertyId::MaxWidth
          | PropertyId::MinHeight
          | PropertyId::MaxHeight => {
            dest.push(property.clone());
          }
          PropertyId::BlockSize => logical_unparsed!(Height),
          PropertyId::MinBlockSize => logical_unparsed!(MinHeight),
          PropertyId::MaxBlockSize => logical_unparsed!(MaxHeight),
          PropertyId::InlineSize => logical_unparsed!(Width),
          PropertyId::MinInlineSize => logical_unparsed!(MinWidth),
          PropertyId::MaxInlineSize => logical_unparsed!(MaxWidth),
          _ => return false,
        }
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, _: &mut DeclarationList, _: &mut PropertyHandlerContext<'i, '_>) {}
}
