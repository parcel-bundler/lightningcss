//! CSS properties related to backgrounds.

use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::*;
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::color::ColorFallbackKind;
use crate::values::image::ImageFallback;
use crate::values::{color::CssColor, image::Image, length::LengthPercentageOrAuto, position::*};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use itertools::izip;
use smallvec::SmallVec;

/// A value for the [background-size](https://www.w3.org/TR/css-backgrounds-3/#background-size) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum BackgroundSize {
  /// An explicit background size.
  Explicit {
    /// The width of the background.
    width: LengthPercentageOrAuto,
    /// The height of the background.
    height: LengthPercentageOrAuto,
  },
  /// The `cover` keyword. Scales the background image to cover both the width and height of the element.
  Cover,
  /// The `contain` keyword. Scales the background image so that it fits within the element.
  Contain,
}

impl Default for BackgroundSize {
  fn default() -> BackgroundSize {
    BackgroundSize::Explicit {
      width: LengthPercentageOrAuto::Auto,
      height: LengthPercentageOrAuto::Auto,
    }
  }
}

impl<'i> Parse<'i> for BackgroundSize {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(width) = input.try_parse(LengthPercentageOrAuto::parse) {
      let height = input
        .try_parse(LengthPercentageOrAuto::parse)
        .unwrap_or(LengthPercentageOrAuto::Auto);
      return Ok(BackgroundSize::Explicit { width, height });
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    Ok(match_ignore_ascii_case! { ident,
      "cover" => BackgroundSize::Cover,
      "contain" => BackgroundSize::Contain,
      _ => return Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    })
  }
}

impl ToCss for BackgroundSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use BackgroundSize::*;

    match &self {
      Cover => dest.write_str("cover"),
      Contain => dest.write_str("contain"),
      Explicit { width, height } => {
        width.to_css(dest)?;
        if *height != LengthPercentageOrAuto::Auto {
          dest.write_str(" ")?;
          height.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl IsCompatible for BackgroundSize {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      BackgroundSize::Explicit { width, height } => {
        width.is_compatible(browsers) && height.is_compatible(browsers)
      }
      BackgroundSize::Cover | BackgroundSize::Contain => true,
    }
  }
}

enum_property! {
  /// A [`<repeat-style>`](https://www.w3.org/TR/css-backgrounds-3/#typedef-repeat-style) value,
  /// used within the `background-repeat` property to represent how a background image is repeated
  /// in a single direction.
  ///
  /// See [BackgroundRepeat](BackgroundRepeat).
  pub enum BackgroundRepeatKeyword {
    /// The image is repeated in this direction.
    "repeat": Repeat,
    /// The image is repeated so that it fits, and then spaced apart evenly.
    "space": Space,
    /// The image is scaled so that it repeats an even number of times.
    "round": Round,
    /// The image is placed once and not repeated in this direction.
    "no-repeat": NoRepeat,
  }
}

/// A value for the [background-repeat](https://www.w3.org/TR/css-backgrounds-3/#background-repeat) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct BackgroundRepeat {
  /// A repeat style for the x direction.
  pub x: BackgroundRepeatKeyword,
  /// A repeat style for the y direction.
  pub y: BackgroundRepeatKeyword,
}

impl Default for BackgroundRepeat {
  fn default() -> BackgroundRepeat {
    BackgroundRepeat {
      x: BackgroundRepeatKeyword::Repeat,
      y: BackgroundRepeatKeyword::Repeat,
    }
  }
}

impl<'i> Parse<'i> for BackgroundRepeat {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    use BackgroundRepeatKeyword::*;
    let state = input.state();
    let ident = input.expect_ident()?;

    match_ignore_ascii_case! { ident,
      "repeat-x" => return Ok(BackgroundRepeat { x: Repeat, y: NoRepeat }),
      "repeat-y" => return Ok(BackgroundRepeat { x: NoRepeat, y: Repeat }),
      _ => {}
    }

    input.reset(&state);

    let x = BackgroundRepeatKeyword::parse(input)?;
    let y = input.try_parse(BackgroundRepeatKeyword::parse).unwrap_or(x.clone());
    Ok(BackgroundRepeat { x, y })
  }
}

impl ToCss for BackgroundRepeat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use BackgroundRepeatKeyword::*;
    match (&self.x, &self.y) {
      (Repeat, NoRepeat) => dest.write_str("repeat-x"),
      (NoRepeat, Repeat) => dest.write_str("repeat-y"),
      (x, y) => {
        x.to_css(dest)?;
        if y != x {
          dest.write_str(" ")?;
          y.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl IsCompatible for BackgroundRepeat {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

enum_property! {
  /// A value for the [background-attachment](https://www.w3.org/TR/css-backgrounds-3/#background-attachment) property.
  pub enum BackgroundAttachment {
    /// The background scrolls with the container.
    Scroll,
    /// The background is fixed to the viewport.
    Fixed,
    /// The background is fixed with regard to the element’s contents.
    Local,
  }
}

impl Default for BackgroundAttachment {
  fn default() -> BackgroundAttachment {
    BackgroundAttachment::Scroll
  }
}

enum_property! {
  /// A value for the [background-origin](https://www.w3.org/TR/css-backgrounds-3/#background-origin) property.
  pub enum BackgroundOrigin {
    /// The position is relative to the border box.
    "border-box": BorderBox,
    /// The position is relative to the padding box.
    "padding-box": PaddingBox,
    /// The position is relative to the content box.
    "content-box": ContentBox,
  }
}

enum_property! {
  /// A value for the [background-clip](https://drafts.csswg.org/css-backgrounds-4/#background-clip) property.
  pub enum BackgroundClip {
    /// The background is clipped to the border box.
    "border-box": BorderBox,
    /// The background is clipped to the padding box.
    "padding-box": PaddingBox,
    /// The background is clipped to the content box.
    "content-box": ContentBox,
    /// The background is clipped to the area painted by the border.
    "border": Border,
    /// The background is clipped to the text content of the element.
    "text": Text,
  }
}

impl PartialEq<BackgroundOrigin> for BackgroundClip {
  fn eq(&self, other: &BackgroundOrigin) -> bool {
    match (self, other) {
      (BackgroundClip::BorderBox, BackgroundOrigin::BorderBox)
      | (BackgroundClip::PaddingBox, BackgroundOrigin::PaddingBox)
      | (BackgroundClip::ContentBox, BackgroundOrigin::ContentBox) => true,
      _ => false,
    }
  }
}

impl Into<BackgroundClip> for BackgroundOrigin {
  fn into(self) -> BackgroundClip {
    match self {
      BackgroundOrigin::BorderBox => BackgroundClip::BorderBox,
      BackgroundOrigin::PaddingBox => BackgroundClip::PaddingBox,
      BackgroundOrigin::ContentBox => BackgroundClip::ContentBox,
    }
  }
}

impl Default for BackgroundClip {
  fn default() -> BackgroundClip {
    BackgroundClip::BorderBox
  }
}

impl BackgroundClip {
  fn is_background_box(&self) -> bool {
    matches!(
      self,
      BackgroundClip::BorderBox | BackgroundClip::PaddingBox | BackgroundClip::ContentBox
    )
  }
}

define_list_shorthand! {
  /// A value for the [background-position](https://drafts.csswg.org/css-backgrounds/#background-position) shorthand property.
  pub struct BackgroundPosition {
    /// The x-position.
    x: BackgroundPositionX(HorizontalPosition),
    /// The y-position.
    y: BackgroundPositionY(VerticalPosition),
  }
}

impl From<Position> for BackgroundPosition {
  fn from(pos: Position) -> Self {
    BackgroundPosition { x: pos.x, y: pos.y }
  }
}

impl Into<Position> for &BackgroundPosition {
  fn into(self) -> Position {
    Position {
      x: self.x.clone(),
      y: self.y.clone(),
    }
  }
}

impl Default for BackgroundPosition {
  fn default() -> Self {
    Position::default().into()
  }
}

impl<'i> Parse<'i> for BackgroundPosition {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let pos = Position::parse(input)?;
    Ok(pos.into())
  }
}

impl ToCss for BackgroundPosition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let pos: Position = self.into();
    pos.to_css(dest)
  }
}

/// A value for the [background](https://www.w3.org/TR/css-backgrounds-3/#background) shorthand property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Background<'i> {
  /// The background image.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub image: Image<'i>,
  /// The background color.
  pub color: CssColor,
  /// The background position.
  pub position: BackgroundPosition,
  /// How the background image should repeat.
  pub repeat: BackgroundRepeat,
  /// The size of the background image.
  pub size: BackgroundSize,
  /// The background attachment.
  pub attachment: BackgroundAttachment,
  /// The background origin.
  pub origin: BackgroundOrigin,
  /// How the background should be clipped.
  pub clip: BackgroundClip,
}

impl<'i> Parse<'i> for Background<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color: Option<CssColor> = None;
    let mut position: Option<BackgroundPosition> = None;
    let mut size: Option<BackgroundSize> = None;
    let mut image: Option<Image> = None;
    let mut repeat: Option<BackgroundRepeat> = None;
    let mut attachment: Option<BackgroundAttachment> = None;
    let mut origin: Option<BackgroundOrigin> = None;
    let mut clip: Option<BackgroundClip> = None;

    loop {
      // TODO: only allowed on the last background.
      if color.is_none() {
        if let Ok(value) = input.try_parse(CssColor::parse) {
          color = Some(value);
          continue;
        }
      }

      if position.is_none() {
        if let Ok(value) = input.try_parse(BackgroundPosition::parse) {
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

      if image.is_none() {
        if let Ok(value) = input.try_parse(Image::parse) {
          image = Some(value);
          continue;
        }
      }

      if repeat.is_none() {
        if let Ok(value) = input.try_parse(BackgroundRepeat::parse) {
          repeat = Some(value);
          continue;
        }
      }

      if attachment.is_none() {
        if let Ok(value) = input.try_parse(BackgroundAttachment::parse) {
          attachment = Some(value);
          continue;
        }
      }

      if origin.is_none() {
        if let Ok(value) = input.try_parse(BackgroundOrigin::parse) {
          origin = Some(value);
          continue;
        }
      }

      if clip.is_none() {
        if let Ok(value) = input.try_parse(BackgroundClip::parse) {
          clip = Some(value);
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

    Ok(Background {
      image: image.unwrap_or_default(),
      color: color.unwrap_or_default(),
      position: position.unwrap_or_default(),
      repeat: repeat.unwrap_or_default(),
      size: size.unwrap_or_default(),
      attachment: attachment.unwrap_or_default(),
      origin: origin.unwrap_or(BackgroundOrigin::PaddingBox),
      clip: clip.unwrap_or(BackgroundClip::BorderBox),
    })
  }
}

impl<'i> ToCss for Background<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut has_output = false;

    if self.color != CssColor::default() {
      self.color.to_css(dest)?;
      has_output = true;
    }

    if self.image != Image::default() {
      if has_output {
        dest.write_str(" ")?;
      }
      self.image.to_css(dest)?;
      has_output = true;
    }

    let position: Position = (&self.position).into();
    if !position.is_zero() || self.size != BackgroundSize::default() {
      if has_output {
        dest.write_str(" ")?;
      }
      position.to_css(dest)?;

      if self.size != BackgroundSize::default() {
        dest.delim('/', true)?;
        self.size.to_css(dest)?;
      }

      has_output = true;
    }

    if self.repeat != BackgroundRepeat::default() {
      if has_output {
        dest.write_str(" ")?;
      }

      self.repeat.to_css(dest)?;
      has_output = true;
    }

    if self.attachment != BackgroundAttachment::default() {
      if has_output {
        dest.write_str(" ")?;
      }

      self.attachment.to_css(dest)?;
      has_output = true;
    }

    let output_padding_box = self.origin != BackgroundOrigin::PaddingBox
      || (self.clip != BackgroundOrigin::BorderBox && self.clip.is_background_box());
    if output_padding_box {
      if has_output {
        dest.write_str(" ")?;
      }

      self.origin.to_css(dest)?;
      has_output = true;
    }

    if (output_padding_box && self.clip != self.origin) || self.clip != BackgroundOrigin::BorderBox {
      if has_output {
        dest.write_str(" ")?;
      }

      self.clip.to_css(dest)?;
      has_output = true;
    }

    // If nothing was output, then this is the initial value, e.g. background: transparent
    if !has_output {
      if dest.minify {
        // `0 0` is the shortest valid background value
        self.position.to_css(dest)?;
      } else {
        dest.write_str("none")?;
      }
    }

    Ok(())
  }
}

impl<'i> ImageFallback<'i> for Background<'i> {
  #[inline]
  fn get_image(&self) -> &Image<'i> {
    &self.image
  }

  #[inline]
  fn with_image(&self, image: Image<'i>) -> Self {
    Background { image, ..self.clone() }
  }

  #[inline]
  fn get_necessary_fallbacks(&self, targets: Targets) -> ColorFallbackKind {
    self.color.get_necessary_fallbacks(targets) | self.get_image().get_necessary_fallbacks(targets)
  }

  #[inline]
  fn get_fallback(&self, kind: ColorFallbackKind) -> Self {
    Background {
      color: self.color.get_fallback(kind),
      image: self.image.get_fallback(kind),
      ..self.clone()
    }
  }
}

impl<'i> Shorthand<'i> for SmallVec<[Background<'i>; 1]> {
  fn from_longhands(decls: &DeclarationBlock<'i>, vendor_prefix: VendorPrefix) -> Option<(Self, bool)> {
    let mut color = None;
    let mut images = None;
    let mut x_positions = None;
    let mut y_positions = None;
    let mut repeats = None;
    let mut sizes = None;
    let mut attachments = None;
    let mut origins = None;
    let mut clips = None;

    let mut count = 0;
    let mut important_count = 0;
    let mut length = None;
    for (property, important) in decls.iter() {
      let len = match property {
        Property::BackgroundColor(value) => {
          color = Some(value.clone());
          count += 1;
          if important {
            important_count += 1;
          }
          continue;
        }
        Property::BackgroundImage(value) => {
          images = Some(value.clone());
          value.len()
        }
        Property::BackgroundPosition(value) => {
          x_positions = Some(value.iter().map(|v| v.x.clone()).collect());
          y_positions = Some(value.iter().map(|v| v.y.clone()).collect());
          value.len()
        }
        Property::BackgroundPositionX(value) => {
          x_positions = Some(value.clone());
          value.len()
        }
        Property::BackgroundPositionY(value) => {
          y_positions = Some(value.clone());
          value.len()
        }
        Property::BackgroundRepeat(value) => {
          repeats = Some(value.clone());
          value.len()
        }
        Property::BackgroundSize(value) => {
          sizes = Some(value.clone());
          value.len()
        }
        Property::BackgroundAttachment(value) => {
          attachments = Some(value.clone());
          value.len()
        }
        Property::BackgroundOrigin(value) => {
          origins = Some(value.clone());
          value.len()
        }
        Property::BackgroundClip(value, vp) => {
          if *vp != vendor_prefix {
            return None;
          }
          clips = Some(value.clone());
          value.len()
        }
        Property::Background(val) => {
          color = Some(val.last().unwrap().color.clone());
          images = Some(val.iter().map(|b| b.image.clone()).collect());
          x_positions = Some(val.iter().map(|b| b.position.x.clone()).collect());
          y_positions = Some(val.iter().map(|b| b.position.y.clone()).collect());
          repeats = Some(val.iter().map(|b| b.repeat.clone()).collect());
          sizes = Some(val.iter().map(|b| b.size.clone()).collect());
          attachments = Some(val.iter().map(|b| b.attachment.clone()).collect());
          origins = Some(val.iter().map(|b| b.origin.clone()).collect());
          clips = Some(val.iter().map(|b| b.clip.clone()).collect());
          val.len()
        }
        _ => continue,
      };

      // Lengths must be equal.
      if length.is_none() {
        length = Some(len);
      } else if length.unwrap() != len {
        return None;
      }

      count += 1;
      if important {
        important_count += 1;
      }
    }

    // !important flags must match to produce a shorthand.
    if important_count > 0 && important_count != count {
      return None;
    }

    if color.is_some()
      && images.is_some()
      && x_positions.is_some()
      && y_positions.is_some()
      && repeats.is_some()
      && sizes.is_some()
      && attachments.is_some()
      && origins.is_some()
      && clips.is_some()
    {
      let length = length.unwrap();
      let values = izip!(
        images.unwrap().drain(..),
        x_positions.unwrap().drain(..),
        y_positions.unwrap().drain(..),
        repeats.unwrap().drain(..),
        sizes.unwrap().drain(..),
        attachments.unwrap().drain(..),
        origins.unwrap().drain(..),
        clips.unwrap().drain(..),
      )
      .enumerate()
      .map(
        |(i, (image, x_position, y_position, repeat, size, attachment, origin, clip))| Background {
          color: if i == length - 1 {
            color.clone().unwrap()
          } else {
            CssColor::default()
          },
          image,
          position: BackgroundPosition {
            x: x_position,
            y: y_position,
          },
          repeat,
          size,
          attachment,
          origin,
          clip: clip,
        },
      )
      .collect();
      return Some((values, important_count > 0));
    }

    None
  }

  fn longhands(vendor_prefix: VendorPrefix) -> Vec<PropertyId<'static>> {
    vec![
      PropertyId::BackgroundColor,
      PropertyId::BackgroundImage,
      PropertyId::BackgroundPositionX,
      PropertyId::BackgroundPositionY,
      PropertyId::BackgroundRepeat,
      PropertyId::BackgroundSize,
      PropertyId::BackgroundAttachment,
      PropertyId::BackgroundOrigin,
      PropertyId::BackgroundClip(vendor_prefix),
    ]
  }

  fn longhand(&self, property_id: &PropertyId) -> Option<Property<'i>> {
    match property_id {
      PropertyId::BackgroundColor => Some(Property::BackgroundColor(self.last().unwrap().color.clone())),
      PropertyId::BackgroundImage => Some(Property::BackgroundImage(
        self.iter().map(|v| v.image.clone()).collect(),
      )),
      PropertyId::BackgroundPositionX => Some(Property::BackgroundPositionX(
        self.iter().map(|v| v.position.x.clone()).collect(),
      )),
      PropertyId::BackgroundPositionY => Some(Property::BackgroundPositionY(
        self.iter().map(|v| v.position.y.clone()).collect(),
      )),
      PropertyId::BackgroundRepeat => Some(Property::BackgroundRepeat(
        self.iter().map(|v| v.repeat.clone()).collect(),
      )),
      PropertyId::BackgroundSize => Some(Property::BackgroundSize(self.iter().map(|v| v.size.clone()).collect())),
      PropertyId::BackgroundAttachment => Some(Property::BackgroundAttachment(
        self.iter().map(|v| v.attachment.clone()).collect(),
      )),
      PropertyId::BackgroundOrigin => Some(Property::BackgroundOrigin(
        self.iter().map(|v| v.origin.clone()).collect(),
      )),
      PropertyId::BackgroundClip(vp) => Some(Property::BackgroundClip(
        self.iter().map(|v| v.clip.clone()).collect(),
        *vp,
      )),
      _ => None,
    }
  }

  fn set_longhand(&mut self, property: &Property<'i>) -> Result<(), ()> {
    macro_rules! longhand {
      ($value: ident, $key: ident $(.$k: ident)*) => {{
        if $value.len() != self.len() {
          return Err(());
        }
        for (i, item) in self.iter_mut().enumerate() {
          item.$key$(.$k)* = $value[i].clone();
        }
      }};
    }

    match property {
      Property::BackgroundColor(value) => self.last_mut().unwrap().color = value.clone(),
      Property::BackgroundImage(value) => longhand!(value, image),
      Property::BackgroundPositionX(value) => longhand!(value, position.x),
      Property::BackgroundPositionY(value) => longhand!(value, position.y),
      Property::BackgroundPosition(value) => longhand!(value, position),
      Property::BackgroundRepeat(value) => longhand!(value, repeat),
      Property::BackgroundSize(value) => longhand!(value, size),
      Property::BackgroundAttachment(value) => longhand!(value, attachment),
      Property::BackgroundOrigin(value) => longhand!(value, origin),
      Property::BackgroundClip(value, _vp) => longhand!(value, clip),
      _ => return Err(()),
    }

    Ok(())
  }
}

property_bitflags! {
  #[derive(Default)]
  struct BackgroundProperty: u16 {
    const BackgroundColor = 1 << 0;
    const BackgroundImage = 1 << 1;
    const BackgroundPositionX = 1 << 2;
    const BackgroundPositionY = 1 << 3;
    const BackgroundPosition = Self::BackgroundPositionX.bits() | Self::BackgroundPositionY.bits();
    const BackgroundRepeat = 1 << 4;
    const BackgroundSize = 1 << 5;
    const BackgroundAttachment = 1 << 6;
    const BackgroundOrigin = 1 << 7;
    const BackgroundClip(_vp) = 1 << 8;
    const Background = Self::BackgroundColor.bits() | Self::BackgroundImage.bits() | Self::BackgroundPosition.bits() | Self::BackgroundRepeat.bits() | Self::BackgroundSize.bits() | Self::BackgroundAttachment.bits() | Self::BackgroundOrigin.bits() | Self::BackgroundClip.bits();
  }
}

#[derive(Default)]
pub(crate) struct BackgroundHandler<'i> {
  color: Option<CssColor>,
  images: Option<SmallVec<[Image<'i>; 1]>>,
  has_prefix: bool,
  x_positions: Option<SmallVec<[HorizontalPosition; 1]>>,
  y_positions: Option<SmallVec<[VerticalPosition; 1]>>,
  repeats: Option<SmallVec<[BackgroundRepeat; 1]>>,
  sizes: Option<SmallVec<[BackgroundSize; 1]>>,
  attachments: Option<SmallVec<[BackgroundAttachment; 1]>>,
  origins: Option<SmallVec<[BackgroundOrigin; 1]>>,
  clips: Option<SmallVec<[BackgroundClip; 1]>>,
  decls: Vec<Property<'i>>,
  flushed_properties: BackgroundProperty,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for BackgroundHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    macro_rules! background_image {
      ($val: ident) => {
        flush!(images, $val);

        // Store prefixed properties. Clear if we hit an unprefixed property and we have
        // targets. In this case, the necessary prefixes will be generated.
        self.has_prefix = $val.iter().any(|x| x.has_vendor_prefix());
        if self.has_prefix {
          self.decls.push(property.clone())
        } else if context.targets.browsers.is_some() {
          self.decls.clear();
        }
      };
    }

    macro_rules! flush {
      ($key: ident, $val: expr) => {{
        if self.$key.is_some() && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets)) {
          self.flush(dest, context);
        }
      }};
    }

    match &property {
      Property::BackgroundColor(val) => {
        flush!(color, val);
        self.color = Some(val.clone());
      }
      Property::BackgroundImage(val) => {
        background_image!(val);
        self.images = Some(val.clone())
      }
      Property::BackgroundPosition(val) => {
        self.x_positions = Some(val.iter().map(|p| p.x.clone()).collect());
        self.y_positions = Some(val.iter().map(|p| p.y.clone()).collect());
      }
      Property::BackgroundPositionX(val) => self.x_positions = Some(val.clone()),
      Property::BackgroundPositionY(val) => self.y_positions = Some(val.clone()),
      Property::BackgroundRepeat(val) => self.repeats = Some(val.clone()),
      Property::BackgroundSize(val) => self.sizes = Some(val.clone()),
      Property::BackgroundAttachment(val) => self.attachments = Some(val.clone()),
      Property::BackgroundOrigin(val) => self.origins = Some(val.clone()),
      Property::BackgroundClip(val, vendor_prefix) => {
        if *vendor_prefix == VendorPrefix::None {
          self.clips = Some(val.clone());
        } else {
          self.flush(dest, context);
          dest.push(property.clone())
        }
      }
      Property::Background(val) => {
        let images: SmallVec<[Image; 1]> = val.iter().map(|b| b.image.clone()).collect();
        background_image!(images);
        let color = val.last().unwrap().color.clone();
        flush!(color, color);
        self.color = Some(color);
        self.images = Some(images);
        self.x_positions = Some(val.iter().map(|b| b.position.x.clone()).collect());
        self.y_positions = Some(val.iter().map(|b| b.position.y.clone()).collect());
        self.repeats = Some(val.iter().map(|b| b.repeat.clone()).collect());
        self.sizes = Some(val.iter().map(|b| b.size.clone()).collect());
        self.attachments = Some(val.iter().map(|b| b.attachment.clone()).collect());
        self.origins = Some(val.iter().map(|b| b.origin.clone()).collect());
        self.clips = Some(val.iter().map(|b| b.clip.clone()).collect());
      }
      Property::Unparsed(val) if is_background_property(&val.property_id) => {
        self.flush(dest, context);
        let mut unparsed = val.clone();
        context.add_unparsed_fallbacks(&mut unparsed);
        self
          .flushed_properties
          .insert(BackgroundProperty::try_from(&unparsed.property_id).unwrap());
        dest.push(Property::Unparsed(unparsed));
      }
      _ => return false,
    }

    self.has_any = true;
    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    // If the last declaration is prefixed, pop the last value
    // so it isn't duplicated when we flush.
    if self.has_prefix {
      self.decls.pop();
    }

    dest.extend(self.decls.drain(..));
    self.flush(dest, context);
    self.flushed_properties = BackgroundProperty::empty();
  }
}

impl<'i> BackgroundHandler<'i> {
  fn flush(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    macro_rules! push {
      ($prop: ident, $val: expr) => {
        dest.push(Property::$prop($val));
        self.flushed_properties.insert(BackgroundProperty::$prop);
      };
    }

    let color = std::mem::take(&mut self.color);
    let mut images = std::mem::take(&mut self.images);
    let mut x_positions = std::mem::take(&mut self.x_positions);
    let mut y_positions = std::mem::take(&mut self.y_positions);
    let mut repeats = std::mem::take(&mut self.repeats);
    let mut sizes = std::mem::take(&mut self.sizes);
    let mut attachments = std::mem::take(&mut self.attachments);
    let mut origins = std::mem::take(&mut self.origins);
    let mut clips = std::mem::take(&mut self.clips);

    if let (
      Some(color),
      Some(images),
      Some(x_positions),
      Some(y_positions),
      Some(repeats),
      Some(sizes),
      Some(attachments),
      Some(origins),
      Some(clips),
    ) = (
      &color,
      &mut images,
      &mut x_positions,
      &mut y_positions,
      &mut repeats,
      &mut sizes,
      &mut attachments,
      &mut origins,
      &mut clips,
    ) {
      // Only use shorthand syntax if the number of layers matches on all properties.
      let len = images.len();
      if x_positions.len() == len
        && y_positions.len() == len
        && repeats.len() == len
        && sizes.len() == len
        && attachments.len() == len
        && origins.len() == len
        && clips.len() == len
      {
        let clip_prefixes = if clips.iter().any(|clip| *clip == BackgroundClip::Text) {
          context.targets.prefixes(VendorPrefix::None, Feature::BackgroundClip)
        } else {
          VendorPrefix::None
        };

        let clip_property = if clip_prefixes != VendorPrefix::None {
          Some(Property::BackgroundClip(clips.clone(), clip_prefixes))
        } else {
          None
        };

        let mut backgrounds: SmallVec<[Background<'i>; 1]> = izip!(
          images.drain(..),
          x_positions.drain(..),
          y_positions.drain(..),
          repeats.drain(..),
          sizes.drain(..),
          attachments.drain(..),
          origins.drain(..),
          clips.drain(..)
        )
        .enumerate()
        .map(
          |(i, (image, x_position, y_position, repeat, size, attachment, origin, clip))| Background {
            color: if i == len - 1 {
              color.clone()
            } else {
              CssColor::default()
            },
            image,
            position: BackgroundPosition {
              x: x_position,
              y: y_position,
            },
            repeat,
            size,
            attachment,
            origin,
            clip: if clip_prefixes == VendorPrefix::None {
              clip
            } else {
              BackgroundClip::default()
            },
          },
        )
        .collect();

        if !self.flushed_properties.intersects(BackgroundProperty::Background) {
          for fallback in backgrounds.get_fallbacks(context.targets) {
            push!(Background, fallback);
          }
        }

        push!(Background, backgrounds);

        if let Some(clip) = clip_property {
          dest.push(clip);
          self.flushed_properties.insert(BackgroundProperty::BackgroundClip);
        }

        self.reset();
        return;
      }
    }

    if let Some(mut color) = color {
      if !self.flushed_properties.contains(BackgroundProperty::BackgroundColor) {
        for fallback in color.get_fallbacks(context.targets) {
          push!(BackgroundColor, fallback);
        }
      }

      push!(BackgroundColor, color);
    }

    if let Some(mut images) = images {
      if !self.flushed_properties.contains(BackgroundProperty::BackgroundImage) {
        for fallback in images.get_fallbacks(context.targets) {
          push!(BackgroundImage, fallback);
        }
      }

      push!(BackgroundImage, images);
    }

    match (&mut x_positions, &mut y_positions) {
      (Some(x_positions), Some(y_positions)) if x_positions.len() == y_positions.len() => {
        let positions = izip!(x_positions.drain(..), y_positions.drain(..))
          .map(|(x, y)| BackgroundPosition { x, y })
          .collect();
        push!(BackgroundPosition, positions);
      }
      _ => {
        if let Some(x_positions) = x_positions {
          push!(BackgroundPositionX, x_positions);
        }

        if let Some(y_positions) = y_positions {
          push!(BackgroundPositionY, y_positions);
        }
      }
    }

    if let Some(repeats) = repeats {
      push!(BackgroundRepeat, repeats);
    }

    if let Some(sizes) = sizes {
      push!(BackgroundSize, sizes);
    }

    if let Some(attachments) = attachments {
      push!(BackgroundAttachment, attachments);
    }

    if let Some(origins) = origins {
      push!(BackgroundOrigin, origins);
    }

    if let Some(clips) = clips {
      let prefixes = if clips.iter().any(|clip| *clip == BackgroundClip::Text) {
        context.targets.prefixes(VendorPrefix::None, Feature::BackgroundClip)
      } else {
        VendorPrefix::None
      };
      dest.push(Property::BackgroundClip(clips, prefixes));
      self.flushed_properties.insert(BackgroundProperty::BackgroundClip);
    }

    self.reset();
  }

  fn reset(&mut self) {
    self.color = None;
    self.images = None;
    self.x_positions = None;
    self.y_positions = None;
    self.repeats = None;
    self.sizes = None;
    self.attachments = None;
    self.origins = None;
    self.clips = None
  }
}

#[inline]
fn is_background_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BackgroundColor
    | PropertyId::BackgroundImage
    | PropertyId::BackgroundPosition
    | PropertyId::BackgroundPositionX
    | PropertyId::BackgroundPositionY
    | PropertyId::BackgroundRepeat
    | PropertyId::BackgroundSize
    | PropertyId::BackgroundAttachment
    | PropertyId::BackgroundOrigin
    | PropertyId::BackgroundClip(_)
    | PropertyId::Background => true,
    _ => false,
  }
}
