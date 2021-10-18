use super::length::{*, NumberOrPercentage};
use cssparser::*;
use super::traits::Parse;
use crate::properties::Property;
use super::rect::Rect;
use super::image::Image;
use super::macros::*;

// https://www.w3.org/TR/css-backgrounds-3/#border-image-repeat
enum_property!(BorderImageRepeatKeyword,
  Stretch,
  Repeat,
  Round,
  Space
);

#[derive(Debug, Clone, PartialEq)]
pub struct BorderImageRepeat(pub BorderImageRepeatKeyword, pub BorderImageRepeatKeyword);

impl Default for BorderImageRepeat {
  fn default() -> BorderImageRepeat {
    BorderImageRepeat(BorderImageRepeatKeyword::Stretch, BorderImageRepeatKeyword::Stretch)
  }
}

impl Parse for BorderImageRepeat {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let horizontal = BorderImageRepeatKeyword::parse(input)?;
    let vertical = input.try_parse(BorderImageRepeatKeyword::parse).ok();
    Ok(BorderImageRepeat(horizontal, vertical.unwrap_or(horizontal)))
  }
}

impl ToCss for BorderImageRepeat {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    self.0.to_css(dest)?;
    if self.0 != self.1 {
      dest.write_str(" ")?;
      self.1.to_css(dest)?;
    }
    Ok(())
  }
}

/// https://www.w3.org/TR/css-backgrounds-3/#border-image-width
#[derive(Debug, Clone, PartialEq)]
pub enum BorderImageSideWidth {
  Number(f32),
  LengthPercentage(LengthPercentage),
  Auto
}

impl Default for BorderImageSideWidth {
  fn default() -> BorderImageSideWidth {
    BorderImageSideWidth::Number(1.0)
  }
}

impl Parse for BorderImageSideWidth {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("auto")).is_ok() {
      return Ok(BorderImageSideWidth::Auto);
    }

    if let Ok(number) = input.try_parse(|input| input.expect_number()) {
      return Ok(BorderImageSideWidth::Number(number))
    }

    if let Ok(percent) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(BorderImageSideWidth::LengthPercentage(percent))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for BorderImageSideWidth {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    use BorderImageSideWidth::*;
    match self {
      Auto => dest.write_str("auto"),
      LengthPercentage(l) => l.to_css(dest),
      Number(n) => serialize_number(*n, dest)
    }
  }
}

/// https://www.w3.org/TR/css-backgrounds-3/#border-image-slice
#[derive(Debug, Clone, PartialEq)]
pub struct BorderImageSlice {
  pub offsets: Rect<NumberOrPercentage>,
  pub fill: bool,
}

impl Default for BorderImageSlice {
  fn default() -> BorderImageSlice {
    BorderImageSlice {
      offsets: Rect::all(NumberOrPercentage::Percentage(Percentage(100.0))),
      fill: false
    }
  }
}

impl Parse for BorderImageSlice {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut fill = input.try_parse(|i| i.expect_ident_matching("fill")).is_ok();
    let offsets = Rect::parse(input)?;
    if !fill {
      fill = input.try_parse(|i| i.expect_ident_matching("fill")).is_ok();
    }
    Ok(BorderImageSlice {
      offsets,
      fill
    })
  }
}

impl ToCss for BorderImageSlice {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    self.offsets.to_css(dest)?;
    if self.fill {
      dest.write_str(" fill")?;
    }
    Ok(())
  }
}

/// https://www.w3.org/TR/css-backgrounds-3/#border-image
#[derive(Debug, Clone, PartialEq)]
pub struct BorderImage {
  source: Image,
  slice: BorderImageSlice,
  width: Rect<BorderImageSideWidth>,
  outset: Rect<LengthOrNumber>,
  repeat: BorderImageRepeat
}

impl Parse for BorderImage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
          let maybe_width_outset: Result<_, cssparser::ParseError<'_, ()>> = input.try_parse(|input| {
            input.expect_delim('/')?;

            // Parse border image width, if applicable.
            let w = input.try_parse(|input| Rect::parse(input)).ok();

            // Parse border image outset if applicable.
            let o = input.try_parse(|input| {
              input.expect_delim('/')?;
              Rect::parse(input)
            }).ok();
            if w.is_none() && o.is_none() {
              Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
            }
            else {
              Ok((w, o))
            }
          });
          if let Ok((w, o)) = maybe_width_outset {
            width = w;
            outset = o;
          }
          continue
        }
      }

      if source.is_none() {
        if let Ok(value) = input.try_parse(|input| Image::parse(input)) {
          source = Some(value);
          continue
        }
      }

      if repeat.is_none() {
        if let Ok(value) = input.try_parse(|input| BorderImageRepeat::parse(input)) {
          repeat = Some(value);
          continue
        }
      }

      break
    }

    if source.is_some() || slice.is_some() || width.is_some() || outset.is_some() || repeat.is_some() {
      Ok(BorderImage {
        source: source.unwrap_or_default(),
        slice: slice.unwrap_or_default(),
        width: width.unwrap_or(Rect::all(BorderImageSideWidth::default())),
        outset: outset.unwrap_or(Rect::all(LengthOrNumber::Number(0.0))),
        repeat: repeat.unwrap_or_default()
      })
    } else {
      Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }
}

impl ToCss for BorderImage {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    if self.source != Image::default() {
      self.source.to_css(dest)?;
    }
    let has_slice = self.slice != BorderImageSlice::default();
    let has_width = self.width != Rect::all(BorderImageSideWidth::default());
    let has_outset = self.outset != Rect::all(LengthOrNumber::Number(0.0));
    if has_slice || has_width || has_outset {
      dest.write_str(" ")?;
      self.slice.to_css(dest)?;
      if has_width || has_outset {
        dest.write_str(" / ")?;
      }
      if has_width {
        self.width.to_css(dest)?;
      }

      if has_outset {
        dest.write_str(" / ")?;
        self.outset.to_css(dest)?;
      }
    }
    
    if self.repeat != BorderImageRepeat::default() {
      dest.write_str(" ")?;
      self.repeat.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Default, Debug)]
pub struct BorderImageHandler {
  source: Option<Image>,
  slice: Option<BorderImageSlice>,
  width: Option<Rect<BorderImageSideWidth>>,
  outset: Option<Rect<LengthOrNumber>>,
  repeat: Option<BorderImageRepeat>
}

impl BorderImageHandler {
  pub fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;
    match property {
      BorderImageSource(val) => self.source = Some(val.clone()),
      BorderImageSlice(val) => self.slice = Some(val.clone()),
      BorderImageWidth(val) => self.width = Some(val.clone()),
      BorderImageOutset(val) => self.outset = Some(val.clone()),
      BorderImageRepeat(val) => self.repeat = Some(val.clone()),
      BorderImage(val) => self.set_border_image(val),
      _ => return false
    }

    true
  }

  pub fn reset(&mut self) {
    self.source = None;
    self.slice = None;
    self.width = None;
    self.outset = None;
    self.repeat = None;
  }

  pub fn set_border_image(&mut self, border_image: &BorderImage) {
    self.source = Some(border_image.source.clone());
    self.slice = Some(border_image.slice.clone());
    self.width = Some(border_image.width.clone());
    self.outset = Some(border_image.outset.clone());
    self.repeat = Some(border_image.repeat.clone());
  }

  pub fn finalize(&mut self) -> Vec<Property> {
    let mut decls = vec![];
    if self.source.is_some() && self.slice.is_some() && self.width.is_some() && self.outset.is_some() && self.repeat.is_some() {
      decls.push(Property::BorderImage(BorderImage {
        source: self.source.clone().unwrap(),
        slice: self.slice.clone().unwrap(),
        width: self.width.clone().unwrap(),
        outset: self.outset.clone().unwrap(),
        repeat: self.repeat.clone().unwrap()
      }))
    } else {
      if let Some(source) = &self.source {
        decls.push(Property::BorderImageSource(source.clone()))
      }

      if let Some(slice) = &self.slice {
        decls.push(Property::BorderImageSlice(slice.clone()))
      }

      if let Some(width) = &self.width {
        decls.push(Property::BorderImageWidth(width.clone()))
      }

      if let Some(outset) = &self.outset {
        decls.push(Property::BorderImageOutset(outset.clone()))
      }

      if let Some(repeat) = &self.repeat {
        decls.push(Property::BorderImageRepeat(repeat.clone()))
      }
    }

    decls
  }
}
