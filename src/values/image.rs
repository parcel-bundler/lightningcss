use cssparser::*;
use crate::properties::VendorPrefix;
use crate::properties::prefixes::{Browsers};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::gradient::*;
use super::resolution::Resolution;
use std::fmt::Write;

/// https://www.w3.org/TR/css-images-3/#typedef-image
#[derive(Debug, Clone, PartialEq)]
pub enum Image {
  None,
  Url(String),
  Gradient(Gradient),
  ImageSet(ImageSet)
}

impl Default for Image {
  fn default() -> Image {
    Image::None
  }
}

impl Image {
  pub fn has_vendor_prefix(&self) -> bool {
    match self {
      Image::Gradient(a) => a.has_vendor_prefix(),
      _ => false
    }
  }

  pub fn get_necessary_prefixes(&self, targets: Browsers) -> VendorPrefix {
    match self {
      Image::Gradient(grad) => grad.get_necessary_prefixes(targets),
      _ => VendorPrefix::None
    }
  }

  pub fn get_prefixed(&self, prefix: VendorPrefix) -> Image {
    match self {
      Image::Gradient(grad) => Image::Gradient(grad.get_prefixed(prefix)),
      _ => self.clone()
    }
  }

  pub fn get_legacy_webkit(&self) -> Result<Image, ()> {
    match self {
      Image::Gradient(grad) => Ok(Image::Gradient(grad.get_legacy_webkit()?)),
      _ => Ok(self.clone())
    }
  }
}

impl Parse for Image {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Image::None)
    }
    
    if let Ok(url) = input.try_parse(|input| input.expect_url()) {
      return Ok(Image::Url(url.as_ref().into()))
    }

    if let Ok(grad) = input.try_parse(Gradient::parse) {
      return Ok(Image::Gradient(grad))
    }

    if let Ok(image_set) = input.try_parse(ImageSet::parse) {
      return Ok(Image::ImageSet(image_set))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for Image {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use Image::*;
    use cssparser::ToCss;
    match self {
      None => dest.write_str("none"),
      Url(url) => {
        Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(dest)
      }
      Gradient(grad) => grad.to_css(dest),
      ImageSet(image_set) => image_set.to_css(dest)
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImageSet(Vec<ImageSetOption>);

impl Parse for ImageSet {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    input.expect_function_matching("image-set")?;
    let options = input.parse_nested_block(|input| {
      input.parse_comma_separated(ImageSetOption::parse)
    })?;
    Ok(ImageSet(options))
  }
}

impl ToCss for ImageSet {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("image-set(")?;
    let mut first = true;
    for option in &self.0 {
      if first {
        first = false;
      } else {
        dest.delim(',', false)?;
      }
      option.to_css(dest)?;
    }
    dest.write_char(')')
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImageSetOption {
  image: Image,
  resolution: Resolution,
  file_type: Option<String>
}

impl Parse for ImageSetOption {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let image = if let Ok(url) = input.try_parse(|input| input.expect_url_or_string()) {
      Image::Url(url.as_ref().into())
    } else {
      Image::parse(input)?
    };

    let (resolution, file_type) = if let Ok(res) = input.try_parse(Resolution::parse) {
      let file_type = input.try_parse(parse_file_type).ok();
      (res, file_type)
    } else {
      let file_type = input.try_parse(parse_file_type).ok();
      let resolution = input.try_parse(Resolution::parse).unwrap_or(Resolution::Dppx(1.0));
      (resolution, file_type)
    };

    Ok(ImageSetOption { image, resolution, file_type })
  }
}

impl ToCss for ImageSetOption {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if let Image::Url(url) = &self.image {
      serialize_string(&url, dest)?;
    } else {
      self.image.to_css(dest)?;
    }

    if self.resolution != Resolution::Dppx(1.0) {
      dest.write_char(' ')?;
      self.resolution.to_css(dest)?;
    }

    if let Some(file_type) = &self.file_type {
      dest.write_str(" type(")?;
      serialize_string(&file_type, dest)?;
      dest.write_char(')')?;
    }

    Ok(())
  }
}

fn parse_file_type<'i, 't>(input: &mut Parser<'i, 't>) -> Result<String, ParseError<'i, ()>> {
  input.expect_function_matching("type")?;
  input.parse_nested_block(|input| Ok(input.expect_string()?.as_ref().to_owned()))
}
