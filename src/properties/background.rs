//! CSS properties related to backgrounds.

use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::*;
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::targets::Browsers;
use crate::traits::{FallbackValues, Parse, PropertyHandler, ToCss};
use crate::values::color::ColorFallbackKind;
use crate::values::image::ImageFallback;
use crate::values::{color::CssColor, image::Image, length::LengthPercentageOrAuto, position::*};
use cssparser::*;
use itertools::izip;
use smallvec::SmallVec;

/// A value for the [background-size](https://www.w3.org/TR/css-backgrounds-3/#background-size) property.
#[derive(Debug, Clone, PartialEq)]
pub enum BackgroundSize {
  /// An explicit background size.
  Explicit {
    width: LengthPercentageOrAuto,
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

/// A value for the [background](https://www.w3.org/TR/css-backgrounds-3/#background) shorthand property.
#[derive(Debug, Clone, PartialEq)]
pub struct Background<'i> {
  /// The background image.
  pub image: Image<'i>,
  /// The background color.
  pub color: CssColor,
  /// The background position.
  pub position: Position,
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
    let mut position: Option<Position> = None;
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

    if !self.position.is_zero() || self.size != BackgroundSize::default() {
      if has_output {
        dest.write_str(" ")?;
      }
      self.position.to_css(dest)?;

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
  fn get_necessary_fallbacks(&self, targets: Browsers) -> ColorFallbackKind {
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

#[derive(Default)]
pub(crate) struct BackgroundHandler<'i> {
  targets: Option<Browsers>,
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
  has_any: bool,
}

impl<'i> BackgroundHandler<'i> {
  pub fn new(targets: Option<Browsers>) -> Self {
    BackgroundHandler {
      targets,
      ..BackgroundHandler::default()
    }
  }
}

impl<'i> PropertyHandler<'i> for BackgroundHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i>,
  ) -> bool {
    macro_rules! background_image {
      ($val: ident) => {
        // Store prefixed properties. Clear if we hit an unprefixed property and we have
        // targets. In this case, the necessary prefixes will be generated.
        self.has_prefix = $val.iter().any(|x| x.has_vendor_prefix());
        if self.has_prefix {
          self.decls.push(property.clone())
        } else if self.targets.is_some() {
          self.decls.clear();
        }
      };
    }

    match &property {
      Property::BackgroundColor(val) => self.color = Some(val.clone()),
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
          self.flush(dest);
          dest.push(property.clone())
        }
      }
      Property::Background(val) => {
        let images: SmallVec<[Image; 1]> = val.iter().map(|b| b.image.clone()).collect();
        background_image!(images);
        self.color = Some(val.last().unwrap().color.clone());
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
        self.flush(dest);
        let mut unparsed = val.clone();
        context.add_unparsed_fallbacks(&mut unparsed);
        dest.push(Property::Unparsed(unparsed))
      }
      _ => return false,
    }

    self.has_any = true;
    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, _: &mut PropertyHandlerContext<'i>) {
    // If the last declaration is prefixed, pop the last value
    // so it isn't duplicated when we flush.
    if self.has_prefix {
      self.decls.pop();
    }

    dest.extend(self.decls.drain(..));
    self.flush(dest);
  }
}

impl<'i> BackgroundHandler<'i> {
  fn flush(&mut self, dest: &mut DeclarationList<'i>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

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
        let clip_prefixes = if let Some(targets) = self.targets {
          if clips.iter().any(|clip| *clip == BackgroundClip::Text) {
            Feature::BackgroundClip.prefixes_for(targets)
          } else {
            VendorPrefix::None
          }
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
            position: Position {
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

        if let Some(targets) = self.targets {
          for fallback in backgrounds.get_fallbacks(targets) {
            dest.push(Property::Background(fallback));
          }
        }

        dest.push(Property::Background(backgrounds));

        if let Some(clip) = clip_property {
          dest.push(clip)
        }

        self.reset();
        return;
      }
    }

    if let Some(mut color) = color {
      if let Some(targets) = self.targets {
        for fallback in color.get_fallbacks(targets) {
          dest.push(Property::BackgroundColor(fallback))
        }
      }

      dest.push(Property::BackgroundColor(color))
    }

    if let Some(mut images) = images {
      if let Some(targets) = self.targets {
        for fallback in images.get_fallbacks(targets) {
          dest.push(Property::BackgroundImage(fallback));
        }
      }

      dest.push(Property::BackgroundImage(images))
    }

    match (&mut x_positions, &mut y_positions) {
      (Some(x_positions), Some(y_positions)) if x_positions.len() == y_positions.len() => {
        let positions = izip!(x_positions.drain(..), y_positions.drain(..))
          .map(|(x, y)| Position { x, y })
          .collect();
        dest.push(Property::BackgroundPosition(positions))
      }
      _ => {
        if let Some(x_positions) = x_positions {
          dest.push(Property::BackgroundPositionX(x_positions))
        }

        if let Some(y_positions) = y_positions {
          dest.push(Property::BackgroundPositionY(y_positions))
        }
      }
    }

    if let Some(repeats) = repeats {
      dest.push(Property::BackgroundRepeat(repeats))
    }

    if let Some(sizes) = sizes {
      dest.push(Property::BackgroundSize(sizes))
    }

    if let Some(attachments) = attachments {
      dest.push(Property::BackgroundAttachment(attachments))
    }

    if let Some(origins) = origins {
      dest.push(Property::BackgroundOrigin(origins))
    }

    if let Some(clips) = clips {
      let prefixes = if let Some(targets) = self.targets {
        if clips.iter().any(|clip| *clip == BackgroundClip::Text) {
          Feature::BackgroundClip.prefixes_for(targets)
        } else {
          VendorPrefix::None
        }
      } else {
        VendorPrefix::None
      };
      dest.push(Property::BackgroundClip(clips, prefixes))
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
