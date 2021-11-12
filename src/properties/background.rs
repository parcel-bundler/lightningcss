use cssparser::*;
use crate::values::{
  length::LengthPercentageOrAuto,
  position::*,
  color::CssColor,
  image::Image
};
use super::prefixes::{Feature, Browsers, is_webkit_gradient};
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::macros::*;
use crate::properties::{Property, VendorPrefix};
use itertools::izip;
use crate::printer::Printer;
use smallvec::SmallVec;

/// https://www.w3.org/TR/css-backgrounds-3/#background-size
#[derive(Debug, Clone, PartialEq)]
pub enum BackgroundSize {
  Explicit {
    width: LengthPercentageOrAuto,
    height: LengthPercentageOrAuto
  },
  Cover,
  Contain
}

impl Default for BackgroundSize {
  fn default() -> BackgroundSize {
    BackgroundSize::Explicit {
      width: LengthPercentageOrAuto::Auto,
      height: LengthPercentageOrAuto::Auto
    }
  }
}

impl Parse for BackgroundSize {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(width) = input.try_parse(LengthPercentageOrAuto::parse) {
      let height = input.try_parse(LengthPercentageOrAuto::parse).unwrap_or(LengthPercentageOrAuto::Auto);
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use BackgroundSize::*;

    match &self {
      Cover => dest.write_str("cover"),
      Contain => dest.write_str("contain"),
      Explicit {width, height} => {
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

// https://www.w3.org/TR/css-backgrounds-3/#typedef-repeat-style
enum_property!(BackgroundRepeatKeyword,
  ("repeat", Repeat),
  ("space", Space),
  ("round", Round),
  ("no-repeat", NoRepeat)
);

/// https://www.w3.org/TR/css-backgrounds-3/#background-repeat
#[derive(Debug, Clone, PartialEq)]
pub struct BackgroundRepeat {
  x: BackgroundRepeatKeyword,
  y: BackgroundRepeatKeyword
}

impl Default for BackgroundRepeat {
  fn default() -> BackgroundRepeat {
    BackgroundRepeat {
      x: BackgroundRepeatKeyword::Repeat,
      y: BackgroundRepeatKeyword::Repeat
    }
  }
}

impl Parse for BackgroundRepeat {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
    let y = BackgroundRepeatKeyword::parse(input).ok().unwrap_or(x.clone());
    Ok(BackgroundRepeat { x, y })
  }
}

impl ToCss for BackgroundRepeat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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

// https://www.w3.org/TR/css-backgrounds-3/#background-attachment
enum_property!(BackgroundAttachment,
  Scroll,
  Fixed,
  Local
);

impl Default for BackgroundAttachment {
  fn default() -> BackgroundAttachment {
    BackgroundAttachment::Scroll
  }
}

// https://www.w3.org/TR/css-backgrounds-3/#typedef-box
enum_property!(BackgroundBox,
  ("border-box", BorderBox),
  ("padding-box", PaddingBox),
  ("content-box", ContentBox)
);

// https://drafts.csswg.org/css-backgrounds-4/#background-clip
enum_property!(BackgroundClip,
  ("border-box", BorderBox),
  ("padding-box", PaddingBox),
  ("content-box", ContentBox),
  ("border", Border),
  ("text", Text)
);

impl PartialEq<BackgroundBox> for BackgroundClip {
  fn eq(&self, other: &BackgroundBox) -> bool {
    match (self, other) {
      (BackgroundClip::BorderBox, BackgroundBox::BorderBox) |
      (BackgroundClip::PaddingBox, BackgroundBox::PaddingBox) |
      (BackgroundClip::ContentBox, BackgroundBox::ContentBox) => true,
      _ => false
    }
  }
}

impl Into<BackgroundClip> for BackgroundBox {
  fn into(self) -> BackgroundClip {
    match self {
      BackgroundBox::BorderBox => BackgroundClip::BorderBox,
      BackgroundBox::PaddingBox => BackgroundClip::PaddingBox,
      BackgroundBox::ContentBox => BackgroundClip::ContentBox
    }
  }
}

// https://www.w3.org/TR/css-backgrounds-3/#background
#[derive(Debug, Clone, PartialEq)]
pub struct Background {
  image: Image,
  color: CssColor,
  position: Position,
  repeat: BackgroundRepeat,
  size: BackgroundSize,
  attachment: BackgroundAttachment,
  origin: BackgroundBox,
  clip: BackgroundClip
}

impl Parse for Background {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut color: Option<CssColor> = None;
    let mut position: Option<Position> = None;
    let mut size: Option<BackgroundSize> = None;
    let mut image: Option<Image> = None;
    let mut repeat: Option<BackgroundRepeat> = None;
    let mut attachment: Option<BackgroundAttachment> = None;
    let mut origin: Option<BackgroundBox> = None;
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

          size = input.try_parse(|input| {
            input.expect_delim('/')?;
            BackgroundSize::parse(input)
          }).ok();

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
        if let Ok(value) = input.try_parse(BackgroundBox::parse) {
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

      break
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
      origin: origin.unwrap_or(BackgroundBox::PaddingBox),
      clip: clip.unwrap_or(BackgroundClip::BorderBox)
    })
  }
}

impl ToCss for Background {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut needs_space = false;

    if self.color != CssColor::default() {
      self.color.to_css(dest)?;
      needs_space = true;
    }

    if self.image != Image::default() {
      if needs_space {
        dest.write_str(" ")?;
      }
      self.image.to_css(dest)?;
      needs_space = true;
    }

    if self.position != Position::default() || self.size != BackgroundSize::default() {
      if needs_space {
        dest.write_str(" ")?;
      }
      self.position.to_css(dest)?;

      if self.size != BackgroundSize::default() {
        dest.delim('/', true)?;
        self.size.to_css(dest)?;
      }

      needs_space = true;
    }

    if self.repeat != BackgroundRepeat::default() {
      if needs_space {
        dest.write_str(" ")?;
      }

      self.repeat.to_css(dest)?;
      needs_space = true;
    }

    if self.attachment != BackgroundAttachment::default() {
      if needs_space {
        dest.write_str(" ")?;
      }

      self.attachment.to_css(dest)?;
      needs_space = true;
    }

    if self.origin != BackgroundBox::PaddingBox || self.clip != BackgroundBox::BorderBox {
      if needs_space {
        dest.write_str(" ")?;
      }

      self.origin.to_css(dest)?;

      if self.clip != self.origin {
        dest.write_str(" ")?;
        self.clip.to_css(dest)?;
      }
    }

    Ok(())
  }
}

#[derive(Default)]
pub struct BackgroundHandler {
  targets: Option<Browsers>,
  color: Option<CssColor>,
  images: Option<SmallVec<[Image; 1]>>,
  x_positions: Option<SmallVec<[HorizontalPosition; 1]>>,
  y_positions: Option<SmallVec<[VerticalPosition; 1]>>,
  repeats: Option<SmallVec<[BackgroundRepeat; 1]>>,
  sizes: Option<SmallVec<[BackgroundSize; 1]>>,
  attachments: Option<SmallVec<[BackgroundAttachment; 1]>>,
  origins: Option<SmallVec<[BackgroundBox; 1]>>,
  clips: Option<SmallVec<[BackgroundClip; 1]>>,
  decls: Vec<Property>
}

impl BackgroundHandler {
  pub fn new(targets: Option<Browsers>) -> BackgroundHandler {
    BackgroundHandler {
      targets,
      ..BackgroundHandler::default()
    }
  }
}

impl PropertyHandler for BackgroundHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    macro_rules! background_image {
      ($val: ident) => {
        if let Some(images) = &self.images {
          if self.targets.is_some() && $val.iter().all(|x| !x.has_vendor_prefix()) {
            self.decls.clear();
          } else if (self.targets.is_none() && images.iter().any(|x| matches!(x, Image::Gradient(_)))) || images.iter().any(|x| x.has_vendor_prefix()) {
            self.flush();
          }
        }
      };
    }

    match &property {
      Property::BackgroundColor(val) => self.color = Some(val.clone()),
      Property::BackgroundImage(val) => {
        background_image!(val);
        self.images = Some(val.clone())
      },
      Property::BackgroundPosition(val) => {
        self.x_positions = Some(val.iter().map(|p| p.x.clone()).collect());
        self.y_positions = Some(val.iter().map(|p| p.y.clone()).collect());
      },
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
          self.flush();
          self.decls.push(property.clone())
        }
      },
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
      _ => return false
    }

    true
  }

  fn finalize(&mut self) -> Vec<Property> {
    self.flush();
    std::mem::take(&mut self.decls)
  }
}

impl BackgroundHandler {
  fn flush(&mut self) {    
    let color = std::mem::take(&mut self.color);
    let mut images = std::mem::take(&mut self.images);
    let mut x_positions = std::mem::take(&mut self.x_positions);
    let mut y_positions = std::mem::take(&mut self.y_positions);
    let mut repeats = std::mem::take(&mut self.repeats);
    let mut sizes = std::mem::take(&mut self.sizes);
    let mut attachments = std::mem::take(&mut self.attachments);
    let mut origins = std::mem::take(&mut self.origins);
    let mut clips = std::mem::take(&mut self.clips);

    if let (Some(color), Some(images), Some(x_positions), Some(y_positions), Some(repeats), Some(sizes), Some(attachments), Some(origins), Some(clips)) = (&color, &mut images, &mut x_positions, &mut y_positions, &mut repeats, &mut sizes, &mut attachments, &mut origins, &mut clips) {
      // Only use shorthand syntax if the number of layers matches on all properties.
      let len = images.len();
      if x_positions.len() == len && y_positions.len() == len && repeats.len() == len && sizes.len() == len && attachments.len() == len && origins.len() == len && clips.len() == len {
        let backgrounds = izip!(images.drain(..), x_positions.drain(..), y_positions.drain(..), repeats.drain(..), sizes.drain(..), attachments.drain(..), origins.drain(..), clips.drain(..)).enumerate().map(|(i, (image, x_position, y_position, repeat, size, attachment, origin, clip))| {
          Background {
            color: if i == len - 1 {
              color.clone()
            } else {
              CssColor::default()
            },
            image,
            position: Position {
              x: x_position,
              y: y_position
            },
            repeat,
            size,
            attachment,
            origin,
            clip
          }
        }).collect();
        self.decls.push(Property::Background(backgrounds));
        self.reset();
        return
      }
    }

    if let Some(color) = color {
      self.decls.push(Property::BackgroundColor(color))
    }

    if let Some(images) = images {
      if let Some(targets) = self.targets {
        let prefixes = images.iter().fold(VendorPrefix::empty(), |acc, image| acc | image.get_necessary_prefixes(targets));
        if prefixes.contains(VendorPrefix::WebKit) {
          if is_webkit_gradient(targets) {
            let images: SmallVec<[Image; 1]> = images.iter().map(|image| image.get_legacy_webkit()).flatten().collect();
            if !images.is_empty() {
              self.decls.push(Property::BackgroundImage(images))
            }
          }
          let images = images.iter().map(|image| image.get_prefixed(VendorPrefix::WebKit)).collect();
          self.decls.push(Property::BackgroundImage(images))
        }

        if prefixes.contains(VendorPrefix::None) {
          let images = images.iter().map(|image| image.get_prefixed(VendorPrefix::None)).collect();
          self.decls.push(Property::BackgroundImage(images))
        }
      } else {
        self.decls.push(Property::BackgroundImage(images))
      }
    }

    match (&mut x_positions, &mut y_positions) {
      (Some(x_positions), Some(y_positions)) if x_positions.len() == y_positions.len() => {
        let positions = izip!(x_positions.drain(..), y_positions.drain(..)).map(|(x, y)| Position {x, y}).collect();
        self.decls.push(Property::BackgroundPosition(positions))
      }
      _ => {
        if let Some(x_positions) = x_positions {
          self.decls.push(Property::BackgroundPositionX(x_positions))
        }
  
        if let Some(y_positions) = y_positions {
          self.decls.push(Property::BackgroundPositionY(y_positions))
        }
      }
    }

    if let Some(repeats) = repeats {
      self.decls.push(Property::BackgroundRepeat(repeats))
    }

    if let Some(sizes) = sizes {
      self.decls.push(Property::BackgroundSize(sizes))
    }

    if let Some(attachments) = attachments {
      self.decls.push(Property::BackgroundAttachment(attachments))
    }

    if let Some(origins) = origins {
      self.decls.push(Property::BackgroundOrigin(origins))
    }

    if let Some(clips) = clips {
      self.decls.push(Property::BackgroundClip(clips, VendorPrefix::None))
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
