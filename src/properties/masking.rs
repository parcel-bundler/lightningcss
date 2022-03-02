use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::macros::enum_property;
use crate::error::{ParserError, PrinterError};
use crate::values::image::ImageFallback;
use crate::values::{
  image::Image,
  position::Position,
  url::Url,
  shape::BasicShape,
};
use super::background::{BackgroundSize, BackgroundRepeat};
use super::border_image::BorderImage;

enum_property! {
  /// https://www.w3.org/TR/css-masking-1/#the-mask-type
  pub enum MaskType {
    Luminance,
    Alpha,
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-masking-1/#the-mask-mode
  pub enum MaskMode {
    "luminance": Luminance,
    "alpha": Alpha,
    "match-source": MatchSource,
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-masking-1/#typedef-geometry-box
  pub enum GeometryBox {
    "border-box": BorderBox,
    "padding-box": PaddingBox,
    "content-box": ContentBox,
    "margin-box": MarginBox,
    "fill-box": FillBox,
    "stroke-box": StrokeBox,
    "view-box": ViewBox,
  }
}

impl Default for GeometryBox {
  fn default() -> GeometryBox {
    GeometryBox::BorderBox
  }
}

/// https://www.w3.org/TR/css-masking-1/#the-mask-clip
#[derive(Debug, Clone, PartialEq)]
pub enum MaskClip {
  GeometryBox(GeometryBox),
  NoClip
}

impl<'i> Parse<'i> for MaskClip {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(b) = input.try_parse(GeometryBox::parse) {
      return Ok(MaskClip::GeometryBox(b))
    }

    input.expect_ident_matching("no-clip")?;
    Ok(MaskClip::NoClip)
  }
}

impl ToCss for MaskClip {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      MaskClip::GeometryBox(b) => b.to_css(dest),
      MaskClip::NoClip => dest.write_str("no-clip")
    }
  }
}

impl Into<MaskClip> for GeometryBox {
  fn into(self) -> MaskClip {
    MaskClip::GeometryBox(self.clone())
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-masking-1/#the-mask-composite
  pub enum MaskComposite {
    Add,
    Subtract,
    Intersect,
    Exclude,
  }
}

/// https://www.w3.org/TR/css-masking-1/#the-mask
#[derive(Debug, Clone, PartialEq)]
pub struct Mask<'i> {
  pub image: Image<'i>,
  pub position: Position,
  pub size: BackgroundSize,
  pub repeat: BackgroundRepeat,
  pub clip: MaskClip,
  pub origin: GeometryBox,
  pub composite: MaskComposite,
  pub mode: MaskMode
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
          size = input.try_parse(|input| {
            input.expect_delim('/')?;
            BackgroundSize::parse(input)
          }).ok();
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

      break
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
      mode: mode.unwrap_or(MaskMode::MatchSource)
    })
  }
}

impl<'i> ToCss for Mask<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

    if self.composite != MaskComposite::Add {
      dest.write_char(' ')?;
      self.composite.to_css(dest)?;
    }

    if self.mode != MaskMode::MatchSource {
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
    Mask {
      image,
      ..self.clone()
    }
  }
}

/// https://www.w3.org/TR/css-masking-1/#the-clip-path
#[derive(Debug, Clone, PartialEq)]
pub enum ClipPath<'i> {
  None,
  Url(Url<'i>),
  Shape(Box<BasicShape>, GeometryBox),
  Box(GeometryBox)
}

impl<'i> Parse<'i> for ClipPath<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      return Ok(ClipPath::Url(url))
    }

    if let Ok(shape) = input.try_parse(BasicShape::parse) {
      let b = input.try_parse(GeometryBox::parse).unwrap_or_default();
      return Ok(ClipPath::Shape(Box::new(shape), b))
    }

    if let Ok(b) = input.try_parse(GeometryBox::parse) {
      if let Ok(shape) = input.try_parse(BasicShape::parse) {
        return Ok(ClipPath::Shape(Box::new(shape), b))
      }
      return Ok(ClipPath::Box(b))
    }

    input.expect_ident_matching("none")?;
    Ok(ClipPath::None)
  }
}

impl<'i> ToCss for ClipPath<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      ClipPath::None => dest.write_str("none"),
      ClipPath::Url(url) => url.to_css(dest),
      ClipPath::Shape(shape, b) => {
        shape.to_css(dest)?;
        if *b != GeometryBox::default() {
          dest.write_char(' ')?;
          b.to_css(dest)?;
        }
        Ok(())
      }
      ClipPath::Box(b) => b.to_css(dest)
    }
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-masking-1/#the-mask-border-mode
  pub enum MaskBorderMode {
    "luminance": Luminance,
    "alpha": Alpha,
  }
}

impl Default for MaskBorderMode {
  fn default() -> MaskBorderMode {
    MaskBorderMode::Alpha
  }
}

/// https://www.w3.org/TR/css-masking-1/#the-mask-border
#[derive(Debug, Clone, PartialEq)]
pub struct MaskBorder<'i> {
  pub border_image: BorderImage<'i>,
  pub mode: MaskBorderMode
}

impl<'i> Parse<'i> for MaskBorder<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut mode: Option<MaskBorderMode> = None;
    let border_image = BorderImage::parse_with_callback(input, |input| {
      if mode.is_none() {
        if let Ok(value) = input.try_parse(MaskBorderMode::parse) {
          mode = Some(value);
          return true
        }
      }
      false
    });

    if border_image.is_ok() || mode.is_some() {
      Ok(MaskBorder {
        border_image: border_image.unwrap_or_default(),
        mode: mode.unwrap_or_default()
      })
    } else {
      Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }
  }
}

impl<'i> ToCss for MaskBorder<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.border_image.to_css(dest)?;
    if self.mode != MaskBorderMode::default() {
      dest.write_char(' ')?;
      self.mode.to_css(dest)?;
    }
    Ok(())
  }
}
