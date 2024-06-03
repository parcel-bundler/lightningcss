//! CSS properties related to border images.

use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::targets::{Browsers, Targets};
use crate::traits::{IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::image::Image;
use crate::values::number::CSSNumber;
use crate::values::rect::Rect;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use crate::{compat, macros::*};
use crate::{
  traits::FallbackValues,
  values::{
    length::*,
    percentage::{NumberOrPercentage, Percentage},
  },
};
use cssparser::*;

enum_property! {
  /// A single [border-image-repeat](https://www.w3.org/TR/css-backgrounds-3/#border-image-repeat) keyword.
  pub enum BorderImageRepeatKeyword {
    /// The image is stretched to fill the area.
    Stretch,
    /// The image is tiled (repeated) to fill the area.
    Repeat,
     /// The image is scaled so that it repeats an even number of times.
    Round,
    /// The image is repeated so that it fits, and then spaced apart evenly.
    Space,
  }
}

impl IsCompatible for BorderImageRepeatKeyword {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    use BorderImageRepeatKeyword::*;
    match self {
      Round => compat::Feature::BorderImageRepeatRound.is_compatible(browsers),
      Space => compat::Feature::BorderImageRepeatSpace.is_compatible(browsers),
      Stretch | Repeat => true,
    }
  }
}

/// A value for the [border-image-repeat](https://www.w3.org/TR/css-backgrounds-3/#border-image-repeat) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct BorderImageRepeat {
  /// The horizontal repeat value.
  pub horizontal: BorderImageRepeatKeyword,
  /// The vertical repeat value.
  pub vertical: BorderImageRepeatKeyword,
}

impl Default for BorderImageRepeat {
  fn default() -> BorderImageRepeat {
    BorderImageRepeat {
      horizontal: BorderImageRepeatKeyword::Stretch,
      vertical: BorderImageRepeatKeyword::Stretch,
    }
  }
}

impl<'i> Parse<'i> for BorderImageRepeat {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let horizontal = BorderImageRepeatKeyword::parse(input)?;
    let vertical = input.try_parse(BorderImageRepeatKeyword::parse).ok();
    Ok(BorderImageRepeat {
      horizontal,
      vertical: vertical.unwrap_or(horizontal),
    })
  }
}

impl ToCss for BorderImageRepeat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.horizontal.to_css(dest)?;
    if self.horizontal != self.vertical {
      dest.write_str(" ")?;
      self.vertical.to_css(dest)?;
    }
    Ok(())
  }
}

impl IsCompatible for BorderImageRepeat {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.horizontal.is_compatible(browsers) && self.vertical.is_compatible(browsers)
  }
}

/// A value for the [border-image-width](https://www.w3.org/TR/css-backgrounds-3/#border-image-width) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum BorderImageSideWidth {
  /// A number representing a multiple of the border width.
  Number(CSSNumber),
  /// An explicit length or percentage.
  LengthPercentage(LengthPercentage),
  /// The `auto` keyword, representing the natural width of the image slice.
  Auto,
}

impl Default for BorderImageSideWidth {
  fn default() -> BorderImageSideWidth {
    BorderImageSideWidth::Number(1.0)
  }
}

impl IsCompatible for BorderImageSideWidth {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      BorderImageSideWidth::LengthPercentage(l) => l.is_compatible(browsers),
      _ => true,
    }
  }
}

/// A value for the [border-image-slice](https://www.w3.org/TR/css-backgrounds-3/#border-image-slice) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct BorderImageSlice {
  /// The offsets from the edges of the image.
  pub offsets: Rect<NumberOrPercentage>,
  /// Whether the middle of the border image should be preserved.
  pub fill: bool,
}

impl Default for BorderImageSlice {
  fn default() -> BorderImageSlice {
    BorderImageSlice {
      offsets: Rect::all(NumberOrPercentage::Percentage(Percentage(1.0))),
      fill: false,
    }
  }
}

impl<'i> Parse<'i> for BorderImageSlice {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut fill = input.try_parse(|i| i.expect_ident_matching("fill")).is_ok();
    let offsets = Rect::parse(input)?;
    if !fill {
      fill = input.try_parse(|i| i.expect_ident_matching("fill")).is_ok();
    }
    Ok(BorderImageSlice { offsets, fill })
  }
}

impl ToCss for BorderImageSlice {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.offsets.to_css(dest)?;
    if self.fill {
      dest.write_str(" fill")?;
    }
    Ok(())
  }
}

impl IsCompatible for BorderImageSlice {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

define_shorthand! {
  /// A value for the [border-image](https://www.w3.org/TR/css-backgrounds-3/#border-image) shorthand property.
  #[derive(Default)]
  pub struct BorderImage<'i>(VendorPrefix) {
    /// The border image.
    #[cfg_attr(feature = "serde", serde(borrow))]
    source: BorderImageSource(Image<'i>),
    /// The offsets that define where the image is sliced.
    slice: BorderImageSlice(BorderImageSlice),
    /// The width of the border image.
    width: BorderImageWidth(Rect<BorderImageSideWidth>),
    /// The amount that the image extends beyond the border box.
    outset: BorderImageOutset(Rect<LengthOrNumber>),
    /// How the border image is scaled and tiled.
    repeat: BorderImageRepeat(BorderImageRepeat),
  }
}

impl<'i> Parse<'i> for BorderImage<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    BorderImage::parse_with_callback(input, |_| false)
  }
}

impl<'i> BorderImage<'i> {
  pub(crate) fn parse_with_callback<'t, F>(
    input: &mut Parser<'i, 't>,
    mut callback: F,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>>
  where
    F: FnMut(&mut Parser<'i, 't>) -> bool,
  {
    let mut source: Option<Image> = None;
    let mut slice: Option<BorderImageSlice> = None;
    let mut width: Option<Rect<BorderImageSideWidth>> = None;
    let mut outset: Option<Rect<LengthOrNumber>> = None;
    let mut repeat: Option<BorderImageRepeat> = None;
    loop {
      if slice.is_none() {
        if let Ok(value) = input.try_parse(|input| BorderImageSlice::parse(input)) {
          slice = Some(value);
          // Parse border image width and outset, if applicable.
          let maybe_width_outset: Result<_, cssparser::ParseError<'_, ParserError<'i>>> =
            input.try_parse(|input| {
              input.expect_delim('/')?;

              // Parse border image width, if applicable.
              let w = input.try_parse(|input| Rect::parse(input)).ok();

              // Parse border image outset if applicable.
              let o = input
                .try_parse(|input| {
                  input.expect_delim('/')?;
                  Rect::parse(input)
                })
                .ok();
              if w.is_none() && o.is_none() {
                Err(input.new_custom_error(ParserError::InvalidDeclaration))
              } else {
                Ok((w, o))
              }
            });
          if let Ok((w, o)) = maybe_width_outset {
            width = w;
            outset = o;
          }
          continue;
        }
      }

      if source.is_none() {
        if let Ok(value) = input.try_parse(|input| Image::parse(input)) {
          source = Some(value);
          continue;
        }
      }

      if repeat.is_none() {
        if let Ok(value) = input.try_parse(|input| BorderImageRepeat::parse(input)) {
          repeat = Some(value);
          continue;
        }
      }

      if callback(input) {
        continue;
      }

      break;
    }

    if source.is_some() || slice.is_some() || width.is_some() || outset.is_some() || repeat.is_some() {
      Ok(BorderImage {
        source: source.unwrap_or_default(),
        slice: slice.unwrap_or_default(),
        width: width.unwrap_or(Rect::all(BorderImageSideWidth::default())),
        outset: outset.unwrap_or(Rect::all(LengthOrNumber::default())),
        repeat: repeat.unwrap_or_default(),
      })
    } else {
      Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }
  }

  pub(crate) fn to_css_internal<W>(
    source: &Image<'i>,
    slice: &BorderImageSlice,
    width: &Rect<BorderImageSideWidth>,
    outset: &Rect<LengthOrNumber>,
    repeat: &BorderImageRepeat,
    dest: &mut Printer<W>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if *source != Image::default() {
      source.to_css(dest)?;
    }
    let has_slice = *slice != BorderImageSlice::default();
    let has_width = *width != Rect::all(BorderImageSideWidth::default());
    let has_outset = *outset != Rect::all(LengthOrNumber::Number(0.0));
    if has_slice || has_width || has_outset {
      dest.write_str(" ")?;
      slice.to_css(dest)?;
      if has_width || has_outset {
        dest.delim('/', true)?;
      }
      if has_width {
        width.to_css(dest)?;
      }

      if has_outset {
        dest.delim('/', true)?;
        outset.to_css(dest)?;
      }
    }

    if *repeat != BorderImageRepeat::default() {
      dest.write_str(" ")?;
      repeat.to_css(dest)?;
    }

    Ok(())
  }
}

impl<'i> ToCss for BorderImage<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    BorderImage::to_css_internal(&self.source, &self.slice, &self.width, &self.outset, &self.repeat, dest)
  }
}

impl<'i> FallbackValues for BorderImage<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    self
      .source
      .get_fallbacks(targets)
      .into_iter()
      .map(|source| BorderImage { source, ..self.clone() })
      .collect()
  }
}

property_bitflags! {
  #[derive(Default, Debug)]
  struct BorderImageProperty: u16 {
    const BorderImageSource = 1 << 0;
    const BorderImageSlice = 1 << 1;
    const BorderImageWidth = 1 << 2;
    const BorderImageOutset = 1 << 3;
    const BorderImageRepeat = 1 << 4;
    const BorderImage(_vp) = Self::BorderImageSource.bits() | Self::BorderImageSlice.bits() | Self::BorderImageWidth.bits() | Self::BorderImageOutset.bits() | Self::BorderImageRepeat.bits();
  }
}

#[derive(Debug)]
pub(crate) struct BorderImageHandler<'i> {
  source: Option<Image<'i>>,
  slice: Option<BorderImageSlice>,
  width: Option<Rect<BorderImageSideWidth>>,
  outset: Option<Rect<LengthOrNumber>>,
  repeat: Option<BorderImageRepeat>,
  vendor_prefix: VendorPrefix,
  flushed_properties: BorderImageProperty,
  has_any: bool,
}

impl<'i> Default for BorderImageHandler<'i> {
  fn default() -> Self {
    BorderImageHandler {
      vendor_prefix: VendorPrefix::empty(),
      source: None,
      slice: None,
      width: None,
      outset: None,
      repeat: None,
      flushed_properties: BorderImageProperty::empty(),
      has_any: false,
    }
  }
}

impl<'i> PropertyHandler<'i> for BorderImageHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;
    macro_rules! property {
      ($name: ident, $val: ident) => {{
        if self.vendor_prefix != VendorPrefix::None {
          self.flush(dest, context);
        }
        flush!($name, $val);
        self.vendor_prefix = VendorPrefix::None;
        self.$name = Some($val.clone());
        self.has_any = true;
      }};
    }

    macro_rules! flush {
      ($name: ident, $val: expr) => {{
        if self.$name.is_some() && self.$name.as_ref().unwrap() != $val && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets)) {
          self.flush(dest, context);
        }
      }};
    }

    match property {
      BorderImageSource(val) => property!(source, val),
      BorderImageSlice(val) => property!(slice, val),
      BorderImageWidth(val) => property!(width, val),
      BorderImageOutset(val) => property!(outset, val),
      BorderImageRepeat(val) => property!(repeat, val),
      BorderImage(val, vp) => {
        flush!(source, &val.source);
        flush!(slice, &val.slice);
        flush!(width, &val.width);
        flush!(outset, &val.outset);
        flush!(repeat, &val.repeat);
        self.source = Some(val.source.clone());
        self.slice = Some(val.slice.clone());
        self.width = Some(val.width.clone());
        self.outset = Some(val.outset.clone());
        self.repeat = Some(val.repeat.clone());
        self.vendor_prefix |= *vp;
        self.has_any = true;
      }
      Unparsed(val) if is_border_image_property(&val.property_id) => {
        self.flush(dest, context);

        // Even if we weren't able to parse the value (e.g. due to var() references),
        // we can still add vendor prefixes to the property itself.
        let mut unparsed = if matches!(val.property_id, PropertyId::BorderImage(_)) {
          val.get_prefixed(context.targets, Feature::BorderImage)
        } else {
          val.clone()
        };

        context.add_unparsed_fallbacks(&mut unparsed);
        self
          .flushed_properties
          .insert(BorderImageProperty::try_from(&unparsed.property_id).unwrap());
        dest.push(Property::Unparsed(unparsed));
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
    self.flushed_properties = BorderImageProperty::empty();
  }
}

impl<'i> BorderImageHandler<'i> {
  pub fn reset(&mut self) {
    self.source = None;
    self.slice = None;
    self.width = None;
    self.outset = None;
    self.repeat = None;
  }

  pub fn will_flush(&self, property: &Property<'i>) -> bool {
    use Property::*;
    match property {
      BorderImageSource(_) | BorderImageSlice(_) | BorderImageWidth(_) | BorderImageOutset(_)
      | BorderImageRepeat(_) => self.vendor_prefix != VendorPrefix::None,
      Unparsed(val) => is_border_image_property(&val.property_id),
      _ => false,
    }
  }

  fn flush(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    macro_rules! push {
      ($prop: ident, $val: expr) => {
        dest.push(Property::$prop($val));
        self.flushed_properties.insert(BorderImageProperty::$prop);
      };
    }

    let source = std::mem::take(&mut self.source);
    let slice = std::mem::take(&mut self.slice);
    let width = std::mem::take(&mut self.width);
    let outset = std::mem::take(&mut self.outset);
    let repeat = std::mem::take(&mut self.repeat);

    if source.is_some() && slice.is_some() && width.is_some() && outset.is_some() && repeat.is_some() {
      let mut border_image = BorderImage {
        source: source.unwrap(),
        slice: slice.unwrap(),
        width: width.unwrap(),
        outset: outset.unwrap(),
        repeat: repeat.unwrap(),
      };

      let mut prefix = self.vendor_prefix;
      if prefix.contains(VendorPrefix::None) && !border_image.slice.fill {
        prefix = context.targets.prefixes(self.vendor_prefix, Feature::BorderImage);
        if !self.flushed_properties.intersects(BorderImageProperty::BorderImage) {
          let fallbacks = border_image.get_fallbacks(context.targets);
          for fallback in fallbacks {
            // Match prefix of fallback. e.g. -webkit-linear-gradient
            // can only be used in -webkit-border-image, not -moz-border-image.
            // However, if border-image is unprefixed, gradients can still be.
            let mut p = fallback.source.get_vendor_prefix() & prefix;
            if p.is_empty() {
              p = prefix;
            }
            dest.push(Property::BorderImage(fallback, p));
          }
        }
      }

      let p = border_image.source.get_vendor_prefix() & prefix;
      if !p.is_empty() {
        prefix = p;
      }

      dest.push(Property::BorderImage(border_image, prefix));
      self.flushed_properties.insert(BorderImageProperty::BorderImage);
    } else {
      if let Some(mut source) = source {
        if !self.flushed_properties.contains(BorderImageProperty::BorderImageSource) {
          let fallbacks = source.get_fallbacks(context.targets);
          for fallback in fallbacks {
            dest.push(Property::BorderImageSource(fallback));
          }
        }

        push!(BorderImageSource, source);
      }

      if let Some(slice) = slice {
        push!(BorderImageSlice, slice);
      }

      if let Some(width) = width {
        push!(BorderImageWidth, width);
      }

      if let Some(outset) = outset {
        push!(BorderImageOutset, outset);
      }

      if let Some(repeat) = repeat {
        push!(BorderImageRepeat, repeat);
      }
    }

    self.vendor_prefix = VendorPrefix::empty();
  }
}

#[inline]
fn is_border_image_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BorderImageSource
    | PropertyId::BorderImageSlice
    | PropertyId::BorderImageWidth
    | PropertyId::BorderImageOutset
    | PropertyId::BorderImageRepeat
    | PropertyId::BorderImage(_) => true,
    _ => false,
  }
}
