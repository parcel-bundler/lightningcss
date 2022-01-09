use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::macros::enum_property;
use crate::error::{ParserError, PrinterError};
use crate::values::{
  image::Image,
  position::Position,
};
use super::background::{BackgroundSize, BackgroundRepeat};

// https://www.w3.org/TR/css-masking-1/#the-mask-type
enum_property!(MaskType,
  Luminance,
  Alpha
);

// https://www.w3.org/TR/css-masking-1/#the-mask-mode
enum_property!(MaskMode,
  ("luminance", Luminance),
  ("alpha", Alpha),
  ("match-source", MatchSource)
);

// https://www.w3.org/TR/css-masking-1/#the-mask-clip
enum_property!(MaskClip,
  ("border-box", BorderBox),
  ("padding-box", PaddingBox),
  ("content-box", ContentBox),
  ("fill-box", FillBox),
  ("stroke-box", StrokeBox),
  ("view-box", ViewBox),
  ("no-clip", NoClip)
);

// https://www.w3.org/TR/css-masking-1/#the-mask-origin
enum_property!(MaskOrigin,
  ("border-box", BorderBox),
  ("padding-box", PaddingBox),
  ("content-box", ContentBox),
  ("fill-box", FillBox),
  ("stroke-box", StrokeBox),
  ("view-box", ViewBox)
);

impl Into<MaskClip> for MaskOrigin {
  fn into(self) -> MaskClip {
    match self {
      MaskOrigin::BorderBox => MaskClip::BorderBox,
      MaskOrigin::PaddingBox => MaskClip::PaddingBox,
      MaskOrigin::ContentBox => MaskClip::ContentBox,
      MaskOrigin::FillBox => MaskClip::FillBox,
      MaskOrigin::StrokeBox => MaskClip::StrokeBox,
      MaskOrigin::ViewBox => MaskClip::ViewBox
    }
  }
}

// https://www.w3.org/TR/css-masking-1/#the-mask-composite
enum_property!(MaskComposite,
  Add,
  Subtract,
  Intersect,
  Exclude
);

/// https://www.w3.org/TR/css-masking-1/#the-mask
#[derive(Debug, Clone, PartialEq)]
pub struct Mask {
  pub image: Image,
  pub position: Position,
  pub size: BackgroundSize,
  pub repeat: BackgroundRepeat,
  pub clip: MaskClip,
  pub origin: MaskOrigin,
  pub composite: MaskComposite,
  pub mode: MaskMode
}

impl Parse for Mask {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut image: Option<Image> = None;
    let mut position: Option<Position> = None;
    let mut size: Option<BackgroundSize> = None;
    let mut repeat: Option<BackgroundRepeat> = None;
    let mut clip: Option<MaskClip> = None;
    let mut origin: Option<MaskOrigin> = None;
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
        if let Ok(value) = input.try_parse(MaskOrigin::parse) {
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
      origin: origin.unwrap_or(MaskOrigin::BorderBox),
      clip: clip.unwrap_or(MaskClip::BorderBox),
      composite: composite.unwrap_or(MaskComposite::Add),
      mode: mode.unwrap_or(MaskMode::MatchSource)
    })
  }
}

impl ToCss for Mask {
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

    if self.origin != MaskOrigin::BorderBox || self.clip != MaskClip::BorderBox {
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
