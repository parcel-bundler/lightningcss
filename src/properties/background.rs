use cssparser::*;
use crate::values::{
  length::{LengthPercentage, LengthPercentageOrAuto},
  position::*,
  percentage::Percentage,
  color::CssColor,
  image::Image
};
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::macros::*;
use crate::properties::Property;
use itertools::izip;
use crate::printer::Printer;
use smallvec::SmallVec;

/// https://www.w3.org/TR/css-backgrounds-3/#background-position
#[derive(Debug, Clone, PartialEq)]
pub struct BackgroundPosition {
  x: HorizontalPosition,
  y: VerticalPosition
}

impl Default for BackgroundPosition {
  fn default() -> BackgroundPosition {
    BackgroundPosition {
      x: HorizontalPosition::Length(LengthPercentage::Percentage(Percentage(0.0))),
      y: VerticalPosition::Length(LengthPercentage::Percentage(Percentage(0.0)))
    }
  }
}

impl Parse for BackgroundPosition {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    match input.try_parse(HorizontalPosition::parse) {
      Ok(HorizontalPosition::Center) => {
        // Try parsing a vertical position next.
        if let Ok(y) = input.try_parse(VerticalPosition::parse) {
          return Ok(BackgroundPosition {
            x: HorizontalPosition::Center,
            y
          })
        }

        // If it didn't work, assume the first actually represents a y position,
        // and the next is an x position. e.g. `center left` rather than `left center`.
        let x = input
          .try_parse(HorizontalPosition::parse)
          .unwrap_or(HorizontalPosition::Center);
        let y = VerticalPosition::Center;
        return Ok(BackgroundPosition { x, y })
      },
      Ok(x @ HorizontalPosition::Length(_)) => {
        // If we got a length as the first component, then the second must 
        // be a keyword or length (not a side offset).
        if let Ok(y_keyword) = input.try_parse(VerticalPositionKeyword::parse) {
          let y = VerticalPosition::Side(y_keyword, None);
          return Ok(BackgroundPosition { x, y });
        }
        if let Ok(y_lp) = input.try_parse(LengthPercentage::parse) {
            let y = VerticalPosition::Length(y_lp);
            return Ok(BackgroundPosition { x, y });
        }
        let y = VerticalPosition::Center;
        let _ = input.try_parse(|i| i.expect_ident_matching("center"));
        return Ok(BackgroundPosition { x, y });
      }
      Ok(HorizontalPosition::Side(x_keyword, lp)) => {
        // If we got a horizontal side keyword (and optional offset), expect another for the vertical side.
        // e.g. `left center` or `left 20px center`
        if input.try_parse(|i| i.expect_ident_matching("center")).is_ok() {
          let x = HorizontalPosition::Side(x_keyword, lp);
          let y = VerticalPosition::Center;
          return Ok(BackgroundPosition { x, y });
        }

        // e.g. `left top`, `left top 20px`, `left 20px top`, or `left 20px top 20px`
        if let Ok(y_keyword) = input.try_parse(VerticalPositionKeyword::parse) {
          let y_lp = input.try_parse(LengthPercentage::parse).ok();
          let x = HorizontalPosition::Side(x_keyword, lp);
          let y = VerticalPosition::Side(y_keyword, y_lp);
          return Ok(BackgroundPosition { x, y });
        }

        // If we didn't get a vertical side keyword (e.g. `left 20px`), then apply the offset to the vertical side.
        let x = HorizontalPosition::Side(x_keyword, None);
        let y = lp.map_or(VerticalPosition::Center, VerticalPosition::Length);
        return Ok(BackgroundPosition { x, y });
      }
      _ => {}
    }

    // If the horizontal position didn't parse, then it must be out of order. Try vertical position keyword.
    let y_keyword = VerticalPositionKeyword::parse(input)?;
    let lp_and_x_pos: Result<_, ParseError<()>> = input.try_parse(|i| {
      let y_lp = i.try_parse(LengthPercentage::parse).ok();
      if let Ok(x_keyword) = i.try_parse(HorizontalPositionKeyword::parse) {
        let x_lp = i.try_parse(LengthPercentage::parse).ok();
        let x_pos = HorizontalPosition::Side(x_keyword, x_lp);
        return Ok((y_lp, x_pos));
      }
      i.expect_ident_matching("center")?;
      let x_pos = HorizontalPosition::Center;
      Ok((y_lp, x_pos))
    });

    if let Ok((y_lp, x)) = lp_and_x_pos {
      let y = VerticalPosition::Side(y_keyword, y_lp);
      return Ok(BackgroundPosition { x, y });
    }

    let x = HorizontalPosition::Center;
    let y = VerticalPosition::Side(y_keyword, None);
    Ok(BackgroundPosition { x, y })
  }
}

impl ToCss for BackgroundPosition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match (&self.x, &self.y) {
      (
        x_pos @ &HorizontalPosition::Side(_, Some(_)),
        &VerticalPosition::Length(ref y_lp),
      ) => {
        x_pos.to_css(dest)?;
        dest.write_str(" top ")?;
        y_lp.to_css(dest)
      },
      (
        &HorizontalPosition::Length(ref x_lp),
        y_pos @ &VerticalPosition::Side(_, Some(_)),
      ) => {
        dest.write_str("left ")?;
        x_lp.to_css(dest)?;
        dest.write_str(" ")?;
        y_pos.to_css(dest)
      },
      (
        &HorizontalPosition::Length(ref x_lp),
        &VerticalPosition::Center
      ) => {
        // `center` is assumed if omitted.
        x_lp.to_css(dest)
      },
      (
        &HorizontalPosition::Length(ref x_lp),
        &VerticalPosition::Length(LengthPercentage::Percentage(Percentage(y_lp)))
      ) if y_lp == 0.5 => {
        // 50% is equivalent to `center`, which may be omitted.
        x_lp.to_css(dest)
      },
      (x_pos, y_pos) => {
        x_pos.to_css(dest)?;
        dest.write_str(" ")?;
        y_pos.to_css(dest)
      },
    }
  }
}

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

// https://www.w3.org/TR/css-backgrounds-3/#background
#[derive(Debug, Clone, PartialEq)]
pub struct Background {
  image: Image,
  color: CssColor,
  position: BackgroundPosition,
  repeat: BackgroundRepeat,
  size: BackgroundSize,
  attachment: BackgroundAttachment,
  origin: BackgroundBox,
  clip: BackgroundBox
}

impl Parse for Background {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut color: Option<CssColor> = None;
    let mut position: Option<BackgroundPosition> = None;
    let mut size: Option<BackgroundSize> = None;
    let mut image: Option<Image> = None;
    let mut repeat: Option<BackgroundRepeat> = None;
    let mut attachment: Option<BackgroundAttachment> = None;
    let mut origin: Option<BackgroundBox> = None;
    let mut clip: Option<BackgroundBox> = None;

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
        if let Ok(value) = input.try_parse(BackgroundBox::parse) {
          clip = Some(value);
          continue;
        }
      }

      break
    }

    if clip.is_none() {
      if let Some(origin) = origin {
        clip = Some(origin.clone());
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
      clip: clip.unwrap_or(BackgroundBox::BorderBox)
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

    if self.position != BackgroundPosition::default() || self.size != BackgroundSize::default() {
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
  color: Option<CssColor>,
  images: Option<SmallVec<[Image; 1]>>,
  x_positions: Option<SmallVec<[HorizontalPosition; 1]>>,
  y_positions: Option<SmallVec<[VerticalPosition; 1]>>,
  repeats: Option<SmallVec<[BackgroundRepeat; 1]>>,
  sizes: Option<SmallVec<[BackgroundSize; 1]>>,
  attachments: Option<SmallVec<[BackgroundAttachment; 1]>>,
  origins: Option<SmallVec<[BackgroundBox; 1]>>,
  clips: Option<SmallVec<[BackgroundBox; 1]>>
}

impl PropertyHandler for BackgroundHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    match &property {
      Property::BackgroundColor(val) => self.color = Some(val.clone()),
      Property::BackgroundImage(val) => self.images = Some(val.clone()),
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
      Property::BackgroundClip(val) => self.clips = Some(val.clone()),
      Property::Background(val) => {
        self.color = Some(val.last().unwrap().color.clone());
        self.images = Some(val.iter().map(|b| b.image.clone()).collect());
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
    let mut decls = vec![];
    
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
            position: BackgroundPosition {
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
        decls.push(Property::Background(backgrounds));
        return decls
      }
    }

    if let Some(color) = color {
      decls.push(Property::BackgroundColor(color))
    }

    if let Some(images) = images {
      decls.push(Property::BackgroundImage(images))
    }

    match (&mut x_positions, &mut y_positions) {
      (Some(x_positions), Some(y_positions)) if x_positions.len() == y_positions.len() => {
        let positions = izip!(x_positions.drain(..), y_positions.drain(..)).map(|(x, y)| BackgroundPosition {x, y}).collect();
        decls.push(Property::BackgroundPosition(positions))
      }
      _ => {
        if let Some(x_positions) = x_positions {
          decls.push(Property::BackgroundPositionX(x_positions))
        }
  
        if let Some(y_positions) = y_positions {
          decls.push(Property::BackgroundPositionY(y_positions))
        }
      }
    }

    if let Some(repeats) = repeats {
      decls.push(Property::BackgroundRepeat(repeats))
    }

    if let Some(sizes) = sizes {
      decls.push(Property::BackgroundSize(sizes))
    }

    if let Some(attachments) = attachments {
      decls.push(Property::BackgroundAttachment(attachments))
    }

    if let Some(origins) = origins {
      decls.push(Property::BackgroundOrigin(origins))
    }

    if let Some(clips) = clips {
      decls.push(Property::BackgroundClip(clips))
    }

    decls
  }
}
