use crate::{values::{length::*, percentage::{Percentage, NumberOrPercentage}, number::serialize_number}, traits::FallbackValues};
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
use crate::error::{ParserError, PrinterError};
use crate::logical::LogicalProperties;

enum_property! {
  /// https://www.w3.org/TR/css-backgrounds-3/#border-image-repeat
  pub enum BorderImageRepeatKeyword {
    Stretch,
    Repeat,
    Round,
    Space,
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BorderImageRepeat(pub BorderImageRepeatKeyword, pub BorderImageRepeatKeyword);

impl Default for BorderImageRepeat {
  fn default() -> BorderImageRepeat {
    BorderImageRepeat(BorderImageRepeatKeyword::Stretch, BorderImageRepeatKeyword::Stretch)
  }
}

impl<'i> Parse<'i> for BorderImageRepeat {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let horizontal = BorderImageRepeatKeyword::parse(input)?;
    let vertical = input.try_parse(BorderImageRepeatKeyword::parse).ok();
    Ok(BorderImageRepeat(horizontal, vertical.unwrap_or(horizontal)))
  }
}

impl ToCss for BorderImageRepeat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

impl<'i> Parse<'i> for BorderImageSideWidth {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

impl<'i> Parse<'i> for BorderImageSlice {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.offsets.to_css(dest)?;
    if self.fill {
      dest.write_str(" fill")?;
    }
    Ok(())
  }
}

/// https://www.w3.org/TR/css-backgrounds-3/#border-image
#[derive(Debug, Clone, PartialEq, Default)]
pub struct BorderImage<'i> {
  pub source: Image<'i>,
  pub slice: BorderImageSlice,
  pub width: Rect<BorderImageSideWidth>,
  pub outset: Rect<LengthOrNumber>,
  pub repeat: BorderImageRepeat
}

impl<'i> Parse<'i> for BorderImage<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    BorderImage::parse_with_callback(input, |_| false)
  }
}

impl<'i> BorderImage<'i> {
  pub(crate) fn parse_with_callback<'t, F>(input: &mut Parser<'i, 't>, mut callback: F) -> Result<Self, ParseError<'i, ParserError<'i>>>
  where
    F: FnMut(&mut Parser<'i, 't>) -> bool
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
          let maybe_width_outset: Result<_, cssparser::ParseError<'_, ParserError<'i>>> = input.try_parse(|input| {
            input.expect_delim('/')?;

            // Parse border image width, if applicable.
            let w = input.try_parse(|input| Rect::parse(input)).ok();

            // Parse border image outset if applicable.
            let o = input.try_parse(|input| {
              input.expect_delim('/')?;
              Rect::parse(input)
            }).ok();
            if w.is_none() && o.is_none() {
              Err(input.new_custom_error(ParserError::InvalidDeclaration))
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

      if callback(input) {
        continue
      }

      break
    }

    if source.is_some() || slice.is_some() || width.is_some() || outset.is_some() || repeat.is_some() {
      Ok(BorderImage {
        source: source.unwrap_or_default(),
        slice: slice.unwrap_or_default(),
        width: width.unwrap_or(Rect::all(BorderImageSideWidth::default())),
        outset: outset.unwrap_or(Rect::all(LengthOrNumber::default())),
        repeat: repeat.unwrap_or_default()
      })
    } else {
      Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }
  }
}

impl<'i> ToCss for BorderImage<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

impl<'i> FallbackValues for BorderImage<'i> {
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<Self> {
    self.source.get_fallbacks(targets)
      .into_iter()
      .map(|source| BorderImage {
        source,
        ..self.clone()
      })
      .collect()
  }
}

#[derive(Default, Debug)]
pub(crate) struct BorderImageHandler<'i> {
  targets: Option<Browsers>,
  source: Option<Image<'i>>,
  slice: Option<BorderImageSlice>,
  width: Option<Rect<BorderImageSideWidth>>,
  outset: Option<Rect<LengthOrNumber>>,
  repeat: Option<BorderImageRepeat>,
  vendor_prefix: VendorPrefix,
  has_any: bool
}

impl<'i> BorderImageHandler<'i> {
  pub fn new(targets: Option<Browsers>) -> BorderImageHandler<'i> {
    BorderImageHandler {
      targets,
      vendor_prefix: VendorPrefix::empty(),
      ..BorderImageHandler::default()
    }
  }
}

impl<'i> PropertyHandler<'i> for BorderImageHandler<'i> {
  fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, logical: &mut LogicalProperties<'i>) -> bool {
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
        let mut unparsed = if matches!(val.property_id, PropertyId::BorderImage(_)) {
          val.get_prefixed(self.targets, Feature::BorderImage)
        } else {
          val.clone()
        };

        logical.add_unparsed_fallbacks(&mut unparsed);
        dest.push(Property::Unparsed(unparsed));
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, _: &mut LogicalProperties<'i>) {
    self.flush(dest);
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

  pub fn set_border_image(&mut self, border_image: &BorderImage<'i>) {
    self.source = Some(border_image.source.clone());
    self.slice = Some(border_image.slice.clone());
    self.width = Some(border_image.width.clone());
    self.outset = Some(border_image.outset.clone());
    self.repeat = Some(border_image.repeat.clone());
  }

  fn flush(&mut self, dest: &mut DeclarationList<'i>) {
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
      let mut border_image = BorderImage {
        source: source.unwrap(),
        slice: slice.unwrap(),
        width: width.unwrap(),
        outset: outset.unwrap(),
        repeat: repeat.unwrap()
      };

      let mut prefix = self.vendor_prefix;
      if prefix.contains(VendorPrefix::None) && !border_image.slice.fill {
        if let Some(targets) = self.targets {
          prefix = Feature::BorderImage.prefixes_for(targets);

          let fallbacks = border_image.get_fallbacks(targets);
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

      dest.push(Property::BorderImage(border_image, prefix))
    } else {
      if let Some(mut source) = source {
        if let Some(targets) = self.targets {
          let fallbacks = source.get_fallbacks(targets);
          for fallback in fallbacks {
            dest.push(Property::BorderImageSource(fallback));
          }
        }

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
