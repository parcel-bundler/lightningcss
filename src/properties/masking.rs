//! CSS properties related to clipping and masking.

use super::background::{BackgroundRepeat, BackgroundSize};
use super::border_image::{BorderImage, BorderImageRepeat, BorderImageSideWidth, BorderImageSlice};
use super::PropertyId;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_list_shorthand, define_shorthand, enum_property, property_bitflags};
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::Property;
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::image::ImageFallback;
use crate::values::length::LengthOrNumber;
use crate::values::rect::Rect;
use crate::values::{image::Image, position::Position, shape::BasicShape, url::Url};
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use itertools::izip;
use smallvec::SmallVec;

enum_property! {
  /// A value for the [mask-type](https://www.w3.org/TR/css-masking-1/#the-mask-type) property.
  pub enum MaskType {
    /// The luminance values of the mask is used.
    Luminance,
    /// The alpha values of the mask is used.
    Alpha,
  }
}

enum_property! {
  /// A value for the [mask-mode](https://www.w3.org/TR/css-masking-1/#the-mask-mode) property.
  pub enum MaskMode {
    /// The luminance values of the mask image is used.
    Luminance,
    /// The alpha values of the mask image is used.
    Alpha,
    /// If an SVG source is used, the value matches the `mask-type` property. Otherwise, the alpha values are used.
    MatchSource,
  }
}

impl Default for MaskMode {
  fn default() -> MaskMode {
    MaskMode::MatchSource
  }
}

enum_property! {
  /// A value for the [-webkit-mask-source-type](https://github.com/WebKit/WebKit/blob/6eece09a1c31e47489811edd003d1e36910e9fd3/Source/WebCore/css/CSSProperties.json#L6578-L6587)
  /// property.
  ///
  /// See also [MaskMode](MaskMode).
  pub enum WebKitMaskSourceType {
    /// Equivalent to `match-source` in the standard `mask-mode` syntax.
    Auto,
    /// The luminance values of the mask image is used.
    Luminance,
    /// The alpha values of the mask image is used.
    Alpha,
  }
}

impl From<MaskMode> for WebKitMaskSourceType {
  fn from(mode: MaskMode) -> WebKitMaskSourceType {
    match mode {
      MaskMode::Luminance => WebKitMaskSourceType::Luminance,
      MaskMode::Alpha => WebKitMaskSourceType::Alpha,
      MaskMode::MatchSource => WebKitMaskSourceType::Auto,
    }
  }
}

enum_property! {
  /// A [`<geometry-box>`](https://www.w3.org/TR/css-masking-1/#typedef-geometry-box) value
  /// as used in the `mask-clip` and `clip-path` properties.
  pub enum GeometryBox {
    /// The painted content is clipped to the content box.
    BorderBox,
    /// The painted content is clipped to the padding box.
    PaddingBox,
    /// The painted content is clipped to the border box.
    ContentBox,
    /// The painted content is clipped to the margin box.
    MarginBox,
    /// The painted content is clipped to the object bounding box.
    FillBox,
    /// The painted content is clipped to the stroke bounding box.
    StrokeBox,
    /// Uses the nearest SVG viewport as reference box.
    ViewBox,
  }
}

impl Default for GeometryBox {
  fn default() -> GeometryBox {
    GeometryBox::BorderBox
  }
}

/// A value for the [mask-clip](https://www.w3.org/TR/css-masking-1/#the-mask-clip) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum MaskClip {
  /// A geometry box.
  GeometryBox(GeometryBox),
  /// The painted content is not clipped.
  NoClip,
}

impl IsCompatible for MaskClip {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      MaskClip::GeometryBox(g) => g.is_compatible(browsers),
      MaskClip::NoClip => true,
    }
  }
}

impl Into<MaskClip> for GeometryBox {
  fn into(self) -> MaskClip {
    MaskClip::GeometryBox(self.clone())
  }
}

impl IsCompatible for GeometryBox {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

enum_property! {
  /// A value for the [mask-composite](https://www.w3.org/TR/css-masking-1/#the-mask-composite) property.
  pub enum MaskComposite {
    /// The source is placed over the destination.
    Add,
    /// The source is placed, where it falls outside of the destination.
    Subtract,
    /// The parts of source that overlap the destination, replace the destination.
    Intersect,
    /// The non-overlapping regions of source and destination are combined.
    Exclude,
  }
}

impl Default for MaskComposite {
  fn default() -> MaskComposite {
    MaskComposite::Add
  }
}

enum_property! {
  /// A value for the [-webkit-mask-composite](https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-mask-composite)
  /// property.
  ///
  /// See also [MaskComposite](MaskComposite).
  #[allow(missing_docs)]
  pub enum WebKitMaskComposite {
    Clear,
    Copy,
    /// Equivalent to `add` in the standard `mask-composite` syntax.
    SourceOver,
    /// Equivalent to `intersect` in the standard `mask-composite` syntax.
    SourceIn,
    /// Equivalent to `subtract` in the standard `mask-composite` syntax.
    SourceOut,
    SourceAtop,
    DestinationOver,
    DestinationIn,
    DestinationOut,
    DestinationAtop,
    /// Equivalent to `exclude` in the standard `mask-composite` syntax.
    Xor,
  }
}

impl From<MaskComposite> for WebKitMaskComposite {
  fn from(composite: MaskComposite) -> WebKitMaskComposite {
    match composite {
      MaskComposite::Add => WebKitMaskComposite::SourceOver,
      MaskComposite::Subtract => WebKitMaskComposite::SourceOut,
      MaskComposite::Intersect => WebKitMaskComposite::SourceIn,
      MaskComposite::Exclude => WebKitMaskComposite::Xor,
    }
  }
}

define_list_shorthand! {
  /// A value for the [mask](https://www.w3.org/TR/css-masking-1/#the-mask) shorthand property.
  pub struct Mask<'i>(VendorPrefix) {
    /// The mask image.
    #[cfg_attr(feature = "serde", serde(borrow))]
    image: MaskImage(Image<'i>, VendorPrefix),
    /// The position of the mask.
    position: MaskPosition(Position, VendorPrefix),
    /// The size of the mask image.
    size: MaskSize(BackgroundSize, VendorPrefix),
    /// How the mask repeats.
    repeat: MaskRepeat(BackgroundRepeat, VendorPrefix),
    /// The box in which the mask is clipped.
    clip: MaskClip(MaskClip, VendorPrefix),
    /// The origin of the mask.
    origin: MaskOrigin(GeometryBox, VendorPrefix),
    /// How the mask is composited with the element.
    composite: MaskComposite(MaskComposite),
    /// How the mask image is interpreted.
    mode: MaskMode(MaskMode),
  }
}

impl<'i> Parse<'i> for Mask<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut image: Option<Image> = None;
    let mut position: Option<Position> = None;
    let mut size: Option<BackgroundSize> = None;
    let mut repeat: Option<BackgroundRepeat> = None;
    let mut clip: Option<MaskClip> = None;
    let mut origin: Option<GeometryBox> = None;
    let mut composite: Option<MaskComposite> = None;
    let mut mode: Option<MaskMode> = None;

    loop {
      if image.is_none() {
        if let Ok(value) = input.try_parse(Image::parse) {
          image = Some(value);
          continue;
        }
      }

      if position.is_none() {
        if let Ok(value) = input.try_parse(Position::parse) {
          position = Some(value);
          size = input
            .try_parse(|input| {
              input.expect_delim('/')?;
              BackgroundSize::parse(input)
            })
            .ok();
          continue;
        }
      }

      if repeat.is_none() {
        if let Ok(value) = input.try_parse(BackgroundRepeat::parse) {
          repeat = Some(value);
          continue;
        }
      }

      if origin.is_none() {
        if let Ok(value) = input.try_parse(GeometryBox::parse) {
          origin = Some(value);
          continue;
        }
      }

      if clip.is_none() {
        if let Ok(value) = input.try_parse(MaskClip::parse) {
          clip = Some(value);
          continue;
        }
      }

      if composite.is_none() {
        if let Ok(value) = input.try_parse(MaskComposite::parse) {
          composite = Some(value);
          continue;
        }
      }

      if mode.is_none() {
        if let Ok(value) = input.try_parse(MaskMode::parse) {
          mode = Some(value);
          continue;
        }
      }

      break;
    }

    if clip.is_none() {
      if let Some(origin) = origin {
        clip = Some(origin.into());
      }
    }

    Ok(Mask {
      image: image.unwrap_or_default(),
      position: position.unwrap_or_default(),
      repeat: repeat.unwrap_or_default(),
      size: size.unwrap_or_default(),
      origin: origin.unwrap_or(GeometryBox::BorderBox),
      clip: clip.unwrap_or(GeometryBox::BorderBox.into()),
      composite: composite.unwrap_or(MaskComposite::Add),
      mode: mode.unwrap_or(MaskMode::MatchSource),
    })
  }
}

impl<'i> ToCss for Mask<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.image.to_css(dest)?;

    if self.position != Position::default() || self.size != BackgroundSize::default() {
      dest.write_char(' ')?;
      self.position.to_css(dest)?;

      if self.size != BackgroundSize::default() {
        dest.delim('/', true)?;
        self.size.to_css(dest)?;
      }
    }

    if self.repeat != BackgroundRepeat::default() {
      dest.write_char(' ')?;
      self.repeat.to_css(dest)?;
    }

    if self.origin != GeometryBox::BorderBox || self.clip != GeometryBox::BorderBox.into() {
      dest.write_char(' ')?;
      self.origin.to_css(dest)?;

      if self.clip != self.origin.into() {
        dest.write_char(' ')?;
        self.clip.to_css(dest)?;
      }
    }

    if self.composite != MaskComposite::default() {
      dest.write_char(' ')?;
      self.composite.to_css(dest)?;
    }

    if self.mode != MaskMode::default() {
      dest.write_char(' ')?;
      self.mode.to_css(dest)?;
    }

    Ok(())
  }
}

// TODO: shorthand handler?
impl<'i> ImageFallback<'i> for Mask<'i> {
  #[inline]
  fn get_image(&self) -> &Image<'i> {
    &self.image
  }

  #[inline]
  fn with_image(&self, image: Image<'i>) -> Self {
    Mask { image, ..self.clone() }
  }
}

/// A value for the [clip-path](https://www.w3.org/TR/css-masking-1/#the-clip-path) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ClipPath<'i> {
  /// No clip path.
  None,
  /// A url reference to an SVG path element.
  #[cfg_attr(feature = "serde", serde(borrow, with = "crate::serialization::ValueWrapper::<Url>"))]
  Url(Url<'i>),
  /// A basic shape, positioned according to the reference box.
  #[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
  Shape {
    /// A basic shape.
    shape: Box<BasicShape>,
    /// A reference box that the shape is positioned according to.
    reference_box: GeometryBox,
  },
  /// A reference box.
  #[cfg_attr(feature = "serde", serde(with = "crate::serialization::ValueWrapper::<GeometryBox>"))]
  Box(GeometryBox),
}

impl<'i> Parse<'i> for ClipPath<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      return Ok(ClipPath::Url(url));
    }

    if let Ok(shape) = input.try_parse(BasicShape::parse) {
      let b = input.try_parse(GeometryBox::parse).unwrap_or_default();
      return Ok(ClipPath::Shape {
        shape: Box::new(shape),
        reference_box: b,
      });
    }

    if let Ok(b) = input.try_parse(GeometryBox::parse) {
      if let Ok(shape) = input.try_parse(BasicShape::parse) {
        return Ok(ClipPath::Shape {
          shape: Box::new(shape),
          reference_box: b,
        });
      }
      return Ok(ClipPath::Box(b));
    }

    input.expect_ident_matching("none")?;
    Ok(ClipPath::None)
  }
}

impl<'i> ToCss for ClipPath<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      ClipPath::None => dest.write_str("none"),
      ClipPath::Url(url) => url.to_css(dest),
      ClipPath::Shape {
        shape,
        reference_box: b,
      } => {
        shape.to_css(dest)?;
        if *b != GeometryBox::default() {
          dest.write_char(' ')?;
          b.to_css(dest)?;
        }
        Ok(())
      }
      ClipPath::Box(b) => b.to_css(dest),
    }
  }
}

enum_property! {
  /// A value for the [mask-border-mode](https://www.w3.org/TR/css-masking-1/#the-mask-border-mode) property.
  pub enum MaskBorderMode {
    /// The luminance values of the mask image is used.
    Luminance,
    /// The alpha values of the mask image is used.
    Alpha,
  }
}

impl Default for MaskBorderMode {
  fn default() -> MaskBorderMode {
    MaskBorderMode::Alpha
  }
}

define_shorthand! {
  /// A value for the [mask-border](https://www.w3.org/TR/css-masking-1/#the-mask-border) shorthand property.
  #[derive(Default)]
  pub struct MaskBorder<'i> {
    /// The mask image.
    #[cfg_attr(feature = "serde", serde(borrow))]
    source: MaskBorderSource(Image<'i>),
    /// The offsets that define where the image is sliced.
    slice: MaskBorderSlice(BorderImageSlice),
    /// The width of the mask image.
    width: MaskBorderWidth(Rect<BorderImageSideWidth>),
    /// The amount that the image extends beyond the border box.
    outset: MaskBorderOutset(Rect<LengthOrNumber>),
    /// How the mask image is scaled and tiled.
    repeat: MaskBorderRepeat(BorderImageRepeat),
    /// How the mask image is interpreted.
    mode: MaskBorderMode(MaskBorderMode),
  }
}

impl<'i> Parse<'i> for MaskBorder<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut mode: Option<MaskBorderMode> = None;
    let border_image = BorderImage::parse_with_callback(input, |input| {
      if mode.is_none() {
        if let Ok(value) = input.try_parse(MaskBorderMode::parse) {
          mode = Some(value);
          return true;
        }
      }
      false
    });

    if border_image.is_ok() || mode.is_some() {
      let border_image = border_image.unwrap_or_default();
      Ok(MaskBorder {
        source: border_image.source,
        slice: border_image.slice,
        width: border_image.width,
        outset: border_image.outset,
        repeat: border_image.repeat,
        mode: mode.unwrap_or_default(),
      })
    } else {
      Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }
  }
}

impl<'i> ToCss for MaskBorder<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    BorderImage::to_css_internal(&self.source, &self.slice, &self.width, &self.outset, &self.repeat, dest)?;
    if self.mode != MaskBorderMode::default() {
      dest.write_char(' ')?;
      self.mode.to_css(dest)?;
    }
    Ok(())
  }
}

impl<'i> FallbackValues for MaskBorder<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    self
      .source
      .get_fallbacks(targets)
      .into_iter()
      .map(|source| MaskBorder { source, ..self.clone() })
      .collect()
  }
}

impl<'i> Into<BorderImage<'i>> for MaskBorder<'i> {
  fn into(self) -> BorderImage<'i> {
    BorderImage {
      source: self.source,
      slice: self.slice,
      width: self.width,
      outset: self.outset,
      repeat: self.repeat,
    }
  }
}

property_bitflags! {
  #[derive(Default, Debug)]
  struct MaskProperty: u16 {
    const MaskImage(_vp) = 1 << 0;
    const MaskPosition(_vp) = 1 << 1;
    const MaskSize(_vp) = 1 << 2;
    const MaskRepeat(_vp) = 1 << 3;
    const MaskClip(_vp) = 1 << 4;
    const MaskOrigin(_vp) = 1 << 5;
    const MaskComposite = 1 << 6;
    const MaskMode = 1 << 7;
    const Mask(_vp) = Self::MaskImage.bits() | Self::MaskPosition.bits() | Self::MaskSize.bits() | Self::MaskRepeat.bits() | Self::MaskClip.bits() | Self::MaskOrigin.bits() | Self::MaskComposite.bits() | Self::MaskMode.bits();

    const MaskBorderSource = 1 << 7;
    const MaskBorderMode = 1 << 8;
    const MaskBorderSlice = 1 << 9;
    const MaskBorderWidth = 1 << 10;
    const MaskBorderOutset = 1 << 11;
    const MaskBorderRepeat = 1 << 12;
    const MaskBorder = Self::MaskBorderSource.bits() | Self::MaskBorderMode.bits() | Self::MaskBorderSlice.bits() | Self::MaskBorderWidth.bits() | Self::MaskBorderOutset.bits() | Self::MaskBorderRepeat.bits();
  }
}

#[derive(Default)]
pub(crate) struct MaskHandler<'i> {
  images: Option<(SmallVec<[Image<'i>; 1]>, VendorPrefix)>,
  positions: Option<(SmallVec<[Position; 1]>, VendorPrefix)>,
  sizes: Option<(SmallVec<[BackgroundSize; 1]>, VendorPrefix)>,
  repeats: Option<(SmallVec<[BackgroundRepeat; 1]>, VendorPrefix)>,
  clips: Option<(SmallVec<[MaskClip; 1]>, VendorPrefix)>,
  origins: Option<(SmallVec<[GeometryBox; 1]>, VendorPrefix)>,
  composites: Option<SmallVec<[MaskComposite; 1]>>,
  modes: Option<SmallVec<[MaskMode; 1]>>,
  border_source: Option<(Image<'i>, VendorPrefix)>,
  border_mode: Option<MaskBorderMode>,
  border_slice: Option<(BorderImageSlice, VendorPrefix)>,
  border_width: Option<(Rect<BorderImageSideWidth>, VendorPrefix)>,
  border_outset: Option<(Rect<LengthOrNumber>, VendorPrefix)>,
  border_repeat: Option<(BorderImageRepeat, VendorPrefix)>,
  flushed_properties: MaskProperty,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for MaskHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest, context);
          }
        }

        if self.$prop.is_some() && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets)) {
          self.flush(dest, context);
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: expr) => {{
        maybe_flush!($prop, $val, $vp);

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
        } else {
          self.$prop = Some(($val.clone(), *$vp));
          self.has_any = true;
        }
      }};
    }

    macro_rules! border_shorthand {
      ($val: expr, $vp: expr) => {
        let source = $val.source.clone();
        maybe_flush!(border_source, &source, &$vp);

        let slice = $val.slice.clone();
        maybe_flush!(border_slice, &slice, &$vp);

        let width = $val.width.clone();
        maybe_flush!(border_width, &width, &$vp);

        let outset = $val.outset.clone();
        maybe_flush!(border_outset, &outset, &$vp);

        let repeat = $val.repeat.clone();
        maybe_flush!(border_repeat, &repeat, &$vp);

        property!(border_source, &source, &$vp);
        property!(border_slice, &slice, &$vp);
        property!(border_width, &width, &$vp);
        property!(border_outset, &outset, &$vp);
        property!(border_repeat, &repeat, &$vp);
      };
    }

    match property {
      Property::MaskImage(val, vp) => property!(images, val, vp),
      Property::MaskPosition(val, vp) => property!(positions, val, vp),
      Property::MaskSize(val, vp) => property!(sizes, val, vp),
      Property::MaskRepeat(val, vp) => property!(repeats, val, vp),
      Property::MaskClip(val, vp) => property!(clips, val, vp),
      Property::MaskOrigin(val, vp) => property!(origins, val, vp),
      Property::MaskComposite(val) => self.composites = Some(val.clone()),
      Property::MaskMode(val) => self.modes = Some(val.clone()),
      Property::Mask(val, prefix) => {
        let images = val.iter().map(|b| b.image.clone()).collect();
        maybe_flush!(images, &images, prefix);

        let positions = val.iter().map(|b| b.position.clone()).collect();
        maybe_flush!(positions, &positions, prefix);

        let sizes = val.iter().map(|b| b.size.clone()).collect();
        maybe_flush!(sizes, &sizes, prefix);

        let repeats = val.iter().map(|b| b.repeat.clone()).collect();
        maybe_flush!(repeats, &repeats, prefix);

        let clips = val.iter().map(|b| b.clip.clone()).collect();
        maybe_flush!(clips, &clips, prefix);

        let origins = val.iter().map(|b| b.origin.clone()).collect();
        maybe_flush!(origins, &origins, prefix);

        self.composites = Some(val.iter().map(|b| b.composite.clone()).collect());
        self.modes = Some(val.iter().map(|b| b.mode.clone()).collect());

        property!(images, &images, prefix);
        property!(positions, &positions, prefix);
        property!(sizes, &sizes, prefix);
        property!(repeats, &repeats, prefix);
        property!(clips, &clips, prefix);
        property!(origins, &origins, prefix);
      }
      Property::Unparsed(val) if is_mask_property(&val.property_id) => {
        self.flush(dest, context);
        let mut unparsed = val.get_prefixed(context.targets, Feature::Mask);
        context.add_unparsed_fallbacks(&mut unparsed);
        self
          .flushed_properties
          .insert(MaskProperty::try_from(&val.property_id).unwrap());
        dest.push(Property::Unparsed(unparsed));
      }
      Property::MaskBorderSource(val) => property!(border_source, val, &VendorPrefix::None),
      Property::WebKitMaskBoxImageSource(val, _) => property!(border_source, val, &VendorPrefix::WebKit),
      Property::MaskBorderMode(val) => self.border_mode = Some(val.clone()),
      Property::MaskBorderSlice(val) => property!(border_slice, val, &VendorPrefix::None),
      Property::WebKitMaskBoxImageSlice(val, _) => property!(border_slice, val, &VendorPrefix::WebKit),
      Property::MaskBorderWidth(val) => property!(border_width, val, &VendorPrefix::None),
      Property::WebKitMaskBoxImageWidth(val, _) => property!(border_width, val, &VendorPrefix::WebKit),
      Property::MaskBorderOutset(val) => property!(border_outset, val, &VendorPrefix::None),
      Property::WebKitMaskBoxImageOutset(val, _) => property!(border_outset, val, &VendorPrefix::WebKit),
      Property::MaskBorderRepeat(val) => property!(border_repeat, val, &VendorPrefix::None),
      Property::WebKitMaskBoxImageRepeat(val, _) => property!(border_repeat, val, &VendorPrefix::WebKit),
      Property::MaskBorder(val) => {
        border_shorthand!(val, VendorPrefix::None);
        self.border_mode = Some(val.mode.clone());
      }
      Property::WebKitMaskBoxImage(val, _) => {
        border_shorthand!(val, VendorPrefix::WebKit);
      }
      Property::Unparsed(val) if is_mask_border_property(&val.property_id) => {
        self.flush(dest, context);
        // Add vendor prefixes and expand color fallbacks.
        let mut val = val.clone();
        let prefix = context
          .targets
          .prefixes(val.property_id.prefix().or_none(), Feature::MaskBorder);
        if prefix.contains(VendorPrefix::WebKit) {
          if let Some(property_id) = get_webkit_mask_property(&val.property_id) {
            let mut clone = val.clone();
            clone.property_id = property_id;
            context.add_unparsed_fallbacks(&mut clone);
            dest.push(Property::Unparsed(clone));
          }
        }

        context.add_unparsed_fallbacks(&mut val);
        self
          .flushed_properties
          .insert(MaskProperty::try_from(&val.property_id).unwrap());
        dest.push(Property::Unparsed(val));
      }
      _ => return false,
    }

    self.has_any = true;
    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
    self.flushed_properties = MaskProperty::empty();
  }
}

impl<'i> MaskHandler<'i> {
  fn flush(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    self.flush_mask(dest, context);
    self.flush_mask_border(dest, context);
  }

  fn flush_mask(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    let mut images = std::mem::take(&mut self.images);
    let mut positions = std::mem::take(&mut self.positions);
    let mut sizes = std::mem::take(&mut self.sizes);
    let mut repeats = std::mem::take(&mut self.repeats);
    let mut clips = std::mem::take(&mut self.clips);
    let mut origins = std::mem::take(&mut self.origins);
    let mut composites = std::mem::take(&mut self.composites);
    let mut modes = std::mem::take(&mut self.modes);

    if let (
      Some((images, images_vp)),
      Some((positions, positions_vp)),
      Some((sizes, sizes_vp)),
      Some((repeats, repeats_vp)),
      Some((clips, clips_vp)),
      Some((origins, origins_vp)),
      Some(composites_val),
      Some(mode_vals),
    ) = (
      &mut images,
      &mut positions,
      &mut sizes,
      &mut repeats,
      &mut clips,
      &mut origins,
      &mut composites,
      &mut modes,
    ) {
      // Only use shorthand syntax if the number of masks matches on all properties.
      let len = images.len();
      let intersection = *images_vp & *positions_vp & *sizes_vp & *repeats_vp & *clips_vp & *origins_vp;
      if !intersection.is_empty()
        && positions.len() == len
        && sizes.len() == len
        && repeats.len() == len
        && clips.len() == len
        && origins.len() == len
        && composites_val.len() == len
        && mode_vals.len() == len
      {
        let mut masks: SmallVec<[Mask<'i>; 1]> = izip!(
          images.drain(..),
          positions.drain(..),
          sizes.drain(..),
          repeats.drain(..),
          clips.drain(..),
          origins.drain(..),
          composites_val.drain(..),
          mode_vals.drain(..)
        )
        .map(|(image, position, size, repeat, clip, origin, composite, mode)| Mask {
          image,
          position,
          size,
          repeat,
          clip,
          origin,
          composite,
          mode,
        })
        .collect();

        let mut prefix = context.targets.prefixes(intersection, Feature::Mask);
        if !self.flushed_properties.intersects(MaskProperty::Mask) {
          for fallback in masks.get_fallbacks(context.targets) {
            // Match prefix of fallback. e.g. -webkit-linear-gradient
            // can only be used in -webkit-mask-image.
            // However, if mask-image is unprefixed, gradients can still be.
            let mut p = fallback
              .iter()
              .fold(VendorPrefix::empty(), |p, mask| p | mask.image.get_vendor_prefix())
              - VendorPrefix::None
              & prefix;
            if p.is_empty() {
              p = prefix;
            }
            self.flush_mask_shorthand(fallback, p, dest);
          }

          let p = masks
            .iter()
            .fold(VendorPrefix::empty(), |p, mask| p | mask.image.get_vendor_prefix())
            - VendorPrefix::None
            & prefix;
          if !p.is_empty() {
            prefix = p;
          }
        }

        self.flush_mask_shorthand(masks, prefix, dest);
        self.flushed_properties.insert(MaskProperty::Mask);

        images_vp.remove(intersection);
        positions_vp.remove(intersection);
        sizes_vp.remove(intersection);
        repeats_vp.remove(intersection);
        clips_vp.remove(intersection);
        origins_vp.remove(intersection);
        composites = None;
        modes = None;
      }
    }

    macro_rules! prop {
      ($var: ident, $property: ident) => {
        if let Some((val, vp)) = $var {
          if !vp.is_empty() {
            let prefix = context.targets.prefixes(vp, Feature::$property);
            dest.push(Property::$property(val, prefix));
            self.flushed_properties.insert(MaskProperty::$property);
          }
        }
      };
    }

    if let Some((mut images, vp)) = images {
      if !vp.is_empty() {
        let mut prefix = vp;
        if !self.flushed_properties.contains(MaskProperty::MaskImage) {
          prefix = context.targets.prefixes(prefix, Feature::MaskImage);
          for fallback in images.get_fallbacks(context.targets) {
            // Match prefix of fallback. e.g. -webkit-linear-gradient
            // can only be used in -webkit-mask-image.
            // However, if mask-image is unprefixed, gradients can still be.
            let mut p = fallback
              .iter()
              .fold(VendorPrefix::empty(), |p, image| p | image.get_vendor_prefix())
              - VendorPrefix::None
              & prefix;
            if p.is_empty() {
              p = prefix;
            }
            dest.push(Property::MaskImage(fallback, p))
          }

          let p = images
            .iter()
            .fold(VendorPrefix::empty(), |p, image| p | image.get_vendor_prefix())
            - VendorPrefix::None
            & prefix;
          if !p.is_empty() {
            prefix = p;
          }
        }

        dest.push(Property::MaskImage(images, prefix));
        self.flushed_properties.insert(MaskProperty::MaskImage);
      }
    }

    prop!(positions, MaskPosition);
    prop!(sizes, MaskSize);
    prop!(repeats, MaskRepeat);
    prop!(clips, MaskClip);
    prop!(origins, MaskOrigin);

    if let Some(composites) = composites {
      let prefix = context.targets.prefixes(VendorPrefix::None, Feature::MaskComposite);
      if prefix.contains(VendorPrefix::WebKit) {
        dest.push(Property::WebKitMaskComposite(
          composites.iter().map(|c| (*c).into()).collect(),
        ));
      }

      dest.push(Property::MaskComposite(composites));
      self.flushed_properties.insert(MaskProperty::MaskComposite);
    }

    if let Some(modes) = modes {
      let prefix = context.targets.prefixes(VendorPrefix::None, Feature::Mask);
      if prefix.contains(VendorPrefix::WebKit) {
        dest.push(Property::WebKitMaskSourceType(
          modes.iter().map(|c| (*c).into()).collect(),
          VendorPrefix::WebKit,
        ));
      }

      dest.push(Property::MaskMode(modes));
      self.flushed_properties.insert(MaskProperty::MaskMode);
    }
  }

  fn flush_mask_shorthand(
    &self,
    masks: SmallVec<[Mask<'i>; 1]>,
    prefix: VendorPrefix,
    dest: &mut DeclarationList<'i>,
  ) {
    if prefix.contains(VendorPrefix::WebKit)
      && masks
        .iter()
        .any(|mask| mask.composite != MaskComposite::default() || mask.mode != MaskMode::default())
    {
      // Prefixed shorthand syntax did not support mask-composite or mask-mode. These map to different webkit-specific properties.
      // -webkit-mask-composite uses a different syntax than mask-composite.
      // -webkit-mask-source-type is equivalent to mask-mode, but only supported in Safari, not Chrome.
      let mut webkit = masks.clone();
      let mut composites: SmallVec<[WebKitMaskComposite; 1]> = SmallVec::new();
      let mut modes: SmallVec<[WebKitMaskSourceType; 1]> = SmallVec::new();
      let mut needs_composites = false;
      let mut needs_modes = false;
      for mask in &mut webkit {
        let composite = std::mem::take(&mut mask.composite);
        if composite != MaskComposite::default() {
          needs_composites = true;
        }
        composites.push(composite.into());

        let mode = std::mem::take(&mut mask.mode);
        if mode != MaskMode::default() {
          needs_modes = true;
        }
        modes.push(mode.into());
      }

      dest.push(Property::Mask(webkit, VendorPrefix::WebKit));
      if needs_composites {
        dest.push(Property::WebKitMaskComposite(composites));
      }
      if needs_modes {
        dest.push(Property::WebKitMaskSourceType(modes, VendorPrefix::WebKit));
      }

      let prefix = prefix - VendorPrefix::WebKit;
      if !prefix.is_empty() {
        dest.push(Property::Mask(masks, prefix));
      }
    } else {
      dest.push(Property::Mask(masks, prefix));
    }
  }

  fn flush_mask_border(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    let mut source = std::mem::take(&mut self.border_source);
    let mut slice = std::mem::take(&mut self.border_slice);
    let mut width = std::mem::take(&mut self.border_width);
    let mut outset = std::mem::take(&mut self.border_outset);
    let mut repeat = std::mem::take(&mut self.border_repeat);
    let mut mode = std::mem::take(&mut self.border_mode);

    if let (
      Some((source, source_vp)),
      Some((slice, slice_vp)),
      Some((width, width_vp)),
      Some((outset, outset_vp)),
      Some((repeat, repeat_vp)),
    ) = (&mut source, &mut slice, &mut width, &mut outset, &mut repeat)
    {
      let intersection = *source_vp & *slice_vp & *width_vp & *outset_vp & *repeat_vp;
      if !intersection.is_empty() && (!intersection.contains(VendorPrefix::None) || mode.is_some()) {
        let mut mask_border = MaskBorder {
          source: source.clone(),
          slice: slice.clone(),
          width: width.clone(),
          outset: outset.clone(),
          repeat: repeat.clone(),
          mode: mode.unwrap_or_default(),
        };

        let mut prefix = context.targets.prefixes(intersection, Feature::MaskBorder);
        if !self.flushed_properties.intersects(MaskProperty::MaskBorder) {
          // Get vendor prefix and color fallbacks.
          let fallbacks = mask_border.get_fallbacks(context.targets);
          for fallback in fallbacks {
            let mut p = fallback.source.get_vendor_prefix() - VendorPrefix::None & prefix;
            if p.is_empty() {
              p = prefix;
            }

            if p.contains(VendorPrefix::WebKit) {
              dest.push(Property::WebKitMaskBoxImage(
                fallback.clone().into(),
                VendorPrefix::WebKit,
              ));
            }

            if p.contains(VendorPrefix::None) {
              dest.push(Property::MaskBorder(fallback));
            }
          }
        }

        let p = mask_border.source.get_vendor_prefix() - VendorPrefix::None & prefix;
        if !p.is_empty() {
          prefix = p;
        }

        if prefix.contains(VendorPrefix::WebKit) {
          dest.push(Property::WebKitMaskBoxImage(
            mask_border.clone().into(),
            VendorPrefix::WebKit,
          ));
        }

        if prefix.contains(VendorPrefix::None) {
          dest.push(Property::MaskBorder(mask_border));
          self.flushed_properties.insert(MaskProperty::MaskBorder);
          mode = None;
        }

        source_vp.remove(intersection);
        slice_vp.remove(intersection);
        width_vp.remove(intersection);
        outset_vp.remove(intersection);
        repeat_vp.remove(intersection);
      }
    }

    if let Some((mut source, mut prefix)) = source {
      prefix = context.targets.prefixes(prefix, Feature::MaskBorderSource);

      if !self.flushed_properties.contains(MaskProperty::MaskBorderSource) {
        // Get vendor prefix and color fallbacks.
        let fallbacks = source.get_fallbacks(context.targets);
        for fallback in fallbacks {
          if prefix.contains(VendorPrefix::WebKit) {
            dest.push(Property::WebKitMaskBoxImageSource(
              fallback.clone(),
              VendorPrefix::WebKit,
            ));
          }

          if prefix.contains(VendorPrefix::None) {
            dest.push(Property::MaskBorderSource(fallback));
          }
        }
      }

      if prefix.contains(VendorPrefix::WebKit) {
        dest.push(Property::WebKitMaskBoxImageSource(source.clone(), VendorPrefix::WebKit));
      }

      if prefix.contains(VendorPrefix::None) {
        dest.push(Property::MaskBorderSource(source));
        self.flushed_properties.insert(MaskProperty::MaskBorderSource);
      }
    }

    macro_rules! prop {
      ($val: expr, $prop: ident, $webkit: ident) => {
        if let Some((val, mut prefix)) = $val {
          prefix = context.targets.prefixes(prefix, Feature::$prop);
          if prefix.contains(VendorPrefix::WebKit) {
            dest.push(Property::$webkit(val.clone(), VendorPrefix::WebKit));
          }

          if prefix.contains(VendorPrefix::None) {
            dest.push(Property::$prop(val));
          }
          self.flushed_properties.insert(MaskProperty::$prop);
        }
      };
    }

    prop!(slice, MaskBorderSlice, WebKitMaskBoxImageSlice);
    prop!(width, MaskBorderWidth, WebKitMaskBoxImageWidth);
    prop!(outset, MaskBorderOutset, WebKitMaskBoxImageOutset);
    prop!(repeat, MaskBorderRepeat, WebKitMaskBoxImageRepeat);

    if let Some(mode) = mode {
      dest.push(Property::MaskBorderMode(mode));
      self.flushed_properties.insert(MaskProperty::MaskBorderMode);
    }
  }
}

#[inline]
fn is_mask_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::MaskImage(_)
    | PropertyId::MaskPosition(_)
    | PropertyId::MaskSize(_)
    | PropertyId::MaskRepeat(_)
    | PropertyId::MaskClip(_)
    | PropertyId::MaskOrigin(_)
    | PropertyId::MaskComposite
    | PropertyId::MaskMode
    | PropertyId::Mask(_) => true,
    _ => false,
  }
}

#[inline]
fn is_mask_border_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::MaskBorderSource
    | PropertyId::MaskBorderSlice
    | PropertyId::MaskBorderWidth
    | PropertyId::MaskBorderOutset
    | PropertyId::MaskBorderRepeat
    | PropertyId::MaskBorderMode
    | PropertyId::MaskBorder => true,
    _ => false,
  }
}

#[inline]
pub(crate) fn get_webkit_mask_property(property_id: &PropertyId) -> Option<PropertyId<'static>> {
  Some(match property_id {
    PropertyId::MaskBorderSource => PropertyId::WebKitMaskBoxImageSource(VendorPrefix::WebKit),
    PropertyId::MaskBorderSlice => PropertyId::WebKitMaskBoxImageSlice(VendorPrefix::WebKit),
    PropertyId::MaskBorderWidth => PropertyId::WebKitMaskBoxImageWidth(VendorPrefix::WebKit),
    PropertyId::MaskBorderOutset => PropertyId::WebKitMaskBoxImageOutset(VendorPrefix::WebKit),
    PropertyId::MaskBorderRepeat => PropertyId::WebKitMaskBoxImageRepeat(VendorPrefix::WebKit),
    PropertyId::MaskBorder => PropertyId::WebKitMaskBoxImage(VendorPrefix::WebKit),
    PropertyId::MaskComposite => PropertyId::WebKitMaskComposite,
    PropertyId::MaskMode => PropertyId::WebKitMaskSourceType(VendorPrefix::WebKit),
    _ => return None,
  })
}
