//! CSS properties related to box sizing.

use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::logical::PropertyCategory;
use crate::macros::{enum_property, property_bitflags};
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::traits::{IsCompatible, Parse, PropertyHandler, ToCss};
use crate::values::length::LengthPercentage;
use crate::values::ratio::Ratio;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

#[cfg(feature = "serde")]
use crate::serialization::*;

// https://drafts.csswg.org/css-sizing-3/#specifying-sizes
// https://www.w3.org/TR/css-sizing-4/#sizing-values

/// A value for the [preferred size properties](https://drafts.csswg.org/css-sizing-3/#preferred-size-properties),
/// i.e. `width` and `height.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum Size {
  /// The `auto` keyword.
  Auto,
  /// An explicit length or percentage.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LengthPercentage>"))]
  LengthPercentage(LengthPercentage),
  /// The `min-content` keyword.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
  MinContent(VendorPrefix),
  /// The `max-content` keyword.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
  MaxContent(VendorPrefix),
  /// The `fit-content` keyword.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
  FitContent(VendorPrefix),
  /// The `fit-content()` function.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LengthPercentage>"))]
  FitContentFunction(LengthPercentage),
  /// The `stretch` keyword, or the `-webkit-fill-available` or `-moz-available` prefixed keywords.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
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

impl IsCompatible for Size {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    use Size::*;
    match self {
      LengthPercentage(l) => l.is_compatible(browsers),
      MinContent(..) => Feature::MinContentSize.is_compatible(browsers),
      MaxContent(..) => Feature::MaxContentSize.is_compatible(browsers),
      FitContent(..) => Feature::FitContentSize.is_compatible(browsers),
      FitContentFunction(l) => {
        Feature::FitContentFunctionSize.is_compatible(browsers) && l.is_compatible(browsers)
      }
      Stretch(vp) => match *vp {
        VendorPrefix::None => Feature::StretchSize,
        VendorPrefix::WebKit | VendorPrefix::Moz => Feature::WebkitFillAvailableSize,
        _ => return false,
      }
      .is_compatible(browsers),
      Contain => false, // ??? no data in mdn
      Auto => true,
    }
  }
}

/// A value for the [minimum](https://drafts.csswg.org/css-sizing-3/#min-size-properties)
/// and [maximum](https://drafts.csswg.org/css-sizing-3/#max-size-properties) size properties,
/// e.g. `min-width` and `max-height`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum MaxSize {
  /// The `none` keyword.
  None,
  /// An explicit length or percentage.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LengthPercentage>"))]
  LengthPercentage(LengthPercentage),
  /// The `min-content` keyword.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
  MinContent(VendorPrefix),
  /// The `max-content` keyword.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
  MaxContent(VendorPrefix),
  /// The `fit-content` keyword.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
  FitContent(VendorPrefix),
  /// The `fit-content()` function.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LengthPercentage>"))]
  FitContentFunction(LengthPercentage),
  /// The `stretch` keyword, or the `-webkit-fill-available` or `-moz-available` prefixed keywords.
  #[cfg_attr(feature = "serde", serde(with = "PrefixWrapper"))]
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

impl IsCompatible for MaxSize {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    use MaxSize::*;
    match self {
      LengthPercentage(l) => l.is_compatible(browsers),
      MinContent(..) => Feature::MinContentSize.is_compatible(browsers),
      MaxContent(..) => Feature::MaxContentSize.is_compatible(browsers),
      FitContent(..) => Feature::FitContentSize.is_compatible(browsers),
      FitContentFunction(l) => {
        Feature::FitContentFunctionSize.is_compatible(browsers) && l.is_compatible(browsers)
      }
      Stretch(vp) => match *vp {
        VendorPrefix::None => Feature::StretchSize,
        VendorPrefix::WebKit | VendorPrefix::Moz => Feature::WebkitFillAvailableSize,
        _ => return false,
      }
      .is_compatible(browsers),
      Contain => false, // ??? no data in mdn
      None => true,
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
    ContentBox,
    /// Include the padding and border (but not the margin) in the width and height.
    BorderBox,
  }
}

/// A value for the [aspect-ratio](https://drafts.csswg.org/css-sizing-4/#aspect-ratio) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct AspectRatio {
  /// The `auto` keyword.
  pub auto: bool,
  /// A preferred aspect ratio for the box, specified as width / height.
  pub ratio: Option<Ratio>,
}

impl<'i> Parse<'i> for AspectRatio {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let mut auto = input.try_parse(|i| i.expect_ident_matching("auto"));
    let ratio = input.try_parse(Ratio::parse);
    if auto.is_err() {
      auto = input.try_parse(|i| i.expect_ident_matching("auto"));
    }
    if auto.is_err() && ratio.is_err() {
      return Err(location.new_custom_error(ParserError::InvalidValue));
    }

    Ok(AspectRatio {
      auto: auto.is_ok(),
      ratio: ratio.ok(),
    })
  }
}

impl ToCss for AspectRatio {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.auto {
      dest.write_str("auto")?;
    }

    if let Some(ratio) = &self.ratio {
      if self.auto {
        dest.write_char(' ')?;
      }
      ratio.to_css(dest)?;
    }

    Ok(())
  }
}

property_bitflags! {
  #[derive(Default)]
  struct SizeProperty: u16 {
    const Width = 1 << 0;
    const Height = 1 << 1;
    const MinWidth = 1 << 2;
    const MinHeight = 1 << 3;
    const MaxWidth = 1 << 4;
    const MaxHeight = 1 << 5;
    const BlockSize = 1 << 6;
    const InlineSize = 1 << 7;
    const MinBlockSize  = 1 << 8;
    const MinInlineSize = 1 << 9;
    const MaxBlockSize = 1 << 10;
    const MaxInlineSize = 1 << 11;
  }
}

#[derive(Default)]
pub(crate) struct SizeHandler {
  width: Option<Size>,
  height: Option<Size>,
  min_width: Option<Size>,
  min_height: Option<Size>,
  max_width: Option<MaxSize>,
  max_height: Option<MaxSize>,
  block_size: Option<Size>,
  inline_size: Option<Size>,
  min_block_size: Option<Size>,
  min_inline_size: Option<Size>,
  max_block_size: Option<MaxSize>,
  max_inline_size: Option<MaxSize>,
  has_any: bool,
  flushed_properties: SizeProperty,
  category: PropertyCategory,
}

impl<'i> PropertyHandler<'i> for SizeHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    let logical_supported = !context.should_compile_logical(Feature::LogicalSize);

    macro_rules! property {
      ($prop: ident, $val: ident, $category: ident) => {{
        // If the category changes betweet logical and physical,
        // or if the value contains syntax that isn't supported across all targets,
        // preserve the previous value as a fallback.
        if PropertyCategory::$category != self.category || (self.$prop.is_some() && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets))) {
          self.flush(dest, context);
        }

        self.$prop = Some($val.clone());
        self.category = PropertyCategory::$category;
        self.has_any = true;
      }};
    }

    match property {
      Property::Width(v) => property!(width, v, Physical),
      Property::Height(v) => property!(height, v, Physical),
      Property::MinWidth(v) => property!(min_width, v, Physical),
      Property::MinHeight(v) => property!(min_height, v, Physical),
      Property::MaxWidth(v) => property!(max_width, v, Physical),
      Property::MaxHeight(v) => property!(max_height, v, Physical),
      Property::BlockSize(size) => property!(block_size, size, Logical),
      Property::MinBlockSize(size) => property!(min_block_size, size, Logical),
      Property::MaxBlockSize(size) => property!(max_block_size, size, Logical),
      Property::InlineSize(size) => property!(inline_size, size, Logical),
      Property::MinInlineSize(size) => property!(min_inline_size, size, Logical),
      Property::MaxInlineSize(size) => property!(max_inline_size, size, Logical),
      Property::Unparsed(unparsed) => {
        self.flush(dest, context);
        macro_rules! logical_unparsed {
          ($physical: ident) => {
            if logical_supported {
              self
                .flushed_properties
                .insert(SizeProperty::try_from(&unparsed.property_id).unwrap());
              dest.push(property.clone());
            } else {
              dest.push(Property::Unparsed(
                unparsed.with_property_id(PropertyId::$physical),
              ));
              self.flushed_properties.insert(SizeProperty::$physical);
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
            self
              .flushed_properties
              .insert(SizeProperty::try_from(&unparsed.property_id).unwrap());
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

  fn finalize(&mut self, dest: &mut DeclarationList, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
    self.flushed_properties = SizeProperty::empty();
  }
}

impl SizeHandler {
  fn flush<'i>(&mut self, dest: &mut DeclarationList, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;
    let logical_supported = !context.should_compile_logical(Feature::LogicalSize);

    macro_rules! prefix {
      ($prop: ident, $size: ident, $feature: ident) => {
        if !self.flushed_properties.contains(SizeProperty::$prop) {
          let prefixes =
            context.targets.prefixes(VendorPrefix::None, crate::prefixes::Feature::$feature) - VendorPrefix::None;
          for prefix in prefixes {
            dest.push(Property::$prop($size::$feature(prefix)));
          }
        }
      };
    }

    macro_rules! property {
      ($prop: ident, $val: ident, $size: ident) => {{
        if let Some(val) = std::mem::take(&mut self.$val) {
          match val {
            $size::Stretch(VendorPrefix::None) => prefix!($prop, $size, Stretch),
            $size::MinContent(VendorPrefix::None) => prefix!($prop, $size, MinContent),
            $size::MaxContent(VendorPrefix::None) => prefix!($prop, $size, MaxContent),
            $size::FitContent(VendorPrefix::None) => prefix!($prop, $size, FitContent),
            _ => {}
          }
          dest.push(Property::$prop(val.clone()));
          self.flushed_properties.insert(SizeProperty::$prop);
        }
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

    property!(Width, width, Size);
    property!(MinWidth, min_width, Size);
    property!(MaxWidth, max_width, MaxSize);
    property!(Height, height, Size);
    property!(MinHeight, min_height, Size);
    property!(MaxHeight, max_height, MaxSize);
    logical!(BlockSize, block_size, Height, Size);
    logical!(MinBlockSize, min_block_size, MinHeight, Size);
    logical!(MaxBlockSize, max_block_size, MaxHeight, MaxSize);
    logical!(InlineSize, inline_size, Width, Size);
    logical!(MinInlineSize, min_inline_size, MinWidth, Size);
    logical!(MaxInlineSize, max_inline_size, MaxWidth, MaxSize);
  }
}
