use crate::values::{length::*, percentage::{Percentage, NumberOrPercentage}, number::serialize_number};
use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::declaration::DeclarationList;
use crate::targets::Browsers;
use crate::prefixes::Feature;
use crate::values::rect::Rect;
use crate::values::image::Image;
use crate::macros::*;
use crate::printer::Printer;

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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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

    if let Ok(number) = input.try_parse(f32::parse) {
      return Ok(BorderImageSideWidth::Number(number))
    }

    if let Ok(percent) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(BorderImageSideWidth::LengthPercentage(percent))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for BorderImageSideWidth {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
      offsets: Rect::all(NumberOrPercentage::Percentage(Percentage(1.0))),
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
  pub source: Image,
  pub slice: BorderImageSlice,
  pub width: Rect<BorderImageSideWidth>,
  pub outset: Rect<LengthOrNumber>,
  pub repeat: BorderImageRepeat
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
        dest.delim('/', true)?;
      }
      if has_width {
        self.width.to_css(dest)?;
      }

      if has_outset {
        dest.delim('/', true)?;
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
pub(crate) struct BorderImageHandler {
  targets: Option<Browsers>,
  source: Option<Image>,
  slice: Option<BorderImageSlice>,
  width: Option<Rect<BorderImageSideWidth>>,
  outset: Option<Rect<LengthOrNumber>>,
  repeat: Option<BorderImageRepeat>,
  vendor_prefix: VendorPrefix,
  has_any: bool
}

impl BorderImageHandler {
  pub fn new(targets: Option<Browsers>) -> BorderImageHandler {
    BorderImageHandler {
      targets,
      vendor_prefix: VendorPrefix::empty(),
      ..BorderImageHandler::default()
    }
  }
}

impl PropertyHandler for BorderImageHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;
    macro_rules! property {
      ($name: ident, $val: ident) => {{
        if self.vendor_prefix != VendorPrefix::None {
          self.flush(dest);
        }
        self.vendor_prefix = VendorPrefix::None;
        self.$name = Some($val.clone());
        self.has_any = true;
      }};
    }

    match property {
      BorderImageSource(val) => property!(source, val),
      BorderImageSlice(val) => property!(slice, val),
      BorderImageWidth(val) => property!(width, val),
      BorderImageOutset(val) => property!(outset, val),
      BorderImageRepeat(val) => property!(repeat, val),
      BorderImage(val, vp) => {
        self.set_border_image(val);
        self.vendor_prefix |= *vp;
        self.has_any = true;
      },
      Unparsed(val) if is_border_image_property(&val.property_id) => {
        self.flush(dest);

        // Even if we weren't able to parse the value (e.g. due to var() references),
        // we can still add vendor prefixes to the property itself.
        let prop = if matches!(val.property_id, PropertyId::BorderImage(_)) {
          Property::Unparsed(val.get_prefixed(self.targets, Feature::BorderImage))
        } else {
          property.clone()
        };

        dest.push(prop);
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    self.flush(dest);
  }
}

impl BorderImageHandler {
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

  fn flush(&mut self, dest: &mut DeclarationList) {
    if !self.has_any {
      return
    }

    self.has_any = false;

    let source = std::mem::take(&mut self.source);
    let slice = std::mem::take(&mut self.slice);
    let width = std::mem::take(&mut self.width);
    let outset = std::mem::take(&mut self.outset);
    let repeat = std::mem::take(&mut self.repeat);

    if source.is_some() && slice.is_some() && width.is_some() && outset.is_some() && repeat.is_some() {
      let slice = slice.unwrap();
      let mut prefix = self.vendor_prefix;
      if prefix.contains(VendorPrefix::None) && !slice.fill {
        if let Some(targets) = self.targets {
          prefix = Feature::BorderImage.prefixes_for(targets)
        }
      }

      dest.push(Property::BorderImage(BorderImage {
        source: source.unwrap(),
        slice,
        width: width.unwrap(),
        outset: outset.unwrap(),
        repeat: repeat.unwrap()
      }, prefix))
    } else {
      if let Some(source) = source {
        dest.push(Property::BorderImageSource(source))
      }

      if let Some(slice) = slice {
        dest.push(Property::BorderImageSlice(slice))
      }

      if let Some(width) = width {
        dest.push(Property::BorderImageWidth(width))
      }

      if let Some(outset) = outset {
        dest.push(Property::BorderImageOutset(outset))
      }

      if let Some(repeat) = repeat {
        dest.push(Property::BorderImageRepeat(repeat))
      }
    }

    self.vendor_prefix = VendorPrefix::empty();
  }
}

#[inline]
fn is_border_image_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BorderImageSource |
    PropertyId::BorderImageSlice |
    PropertyId::BorderImageWidth |
    PropertyId::BorderImageOutset |
    PropertyId::BorderImageRepeat |
    PropertyId::BorderImage(_) => true,
    _ => false
  }
}
