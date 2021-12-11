use cssparser::*;
use crate::vendor_prefix::VendorPrefix;
use crate::prefixes::Feature;
use crate::targets::Browsers;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::gradient::*;
use super::resolution::Resolution;

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
      Image::ImageSet(a) => a.has_vendor_prefix(),
      _ => false
    }
  }

  pub fn get_necessary_prefixes(&self, targets: Browsers) -> VendorPrefix {
    match self {
      Image::Gradient(grad) => grad.get_necessary_prefixes(targets),
      Image::ImageSet(image_set) => image_set.get_necessary_prefixes(targets),
      _ => VendorPrefix::None
    }
  }

  pub fn get_prefixed(&self, prefix: VendorPrefix) -> Image {
    match self {
      Image::Gradient(grad) => Image::Gradient(grad.get_prefixed(prefix)),
      Image::ImageSet(image_set) => Image::ImageSet(image_set.get_prefixed(prefix)),
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
        if dest.minify {
          let mut buf = String::new();
          Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(&mut buf)?;

          // If the unquoted url is longer than it would be quoted (e.g. `url("...")`)
          // then serialize as a string and choose the shorter version.
          if buf.len() > url.len() + 7 {
            let mut buf2 = String::new();
            serialize_string(url, &mut buf2)?;
            if buf2.len() + 5 < buf.len() {
              dest.write_str("url(")?;
              dest.write_str(&buf2)?;
              return dest.write_char(')')
            }
          }

          dest.write_str(&buf)
        } else {
          Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(dest)
        }
      }
      Gradient(grad) => grad.to_css(dest),
      ImageSet(image_set) => image_set.to_css(dest)
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImageSet {
  pub options: Vec<ImageSetOption>,
  pub vendor_prefix: VendorPrefix
}

impl ImageSet {
  pub fn has_vendor_prefix(&self) -> bool {
    self.vendor_prefix != VendorPrefix::None
  }

  pub fn get_necessary_prefixes(&self, targets: Browsers) -> VendorPrefix {
    Feature::ImageSet.prefixes_for(targets)
  }

  pub fn get_prefixed(&self, prefix: VendorPrefix) -> ImageSet {
    ImageSet {
      options: self.options.clone(),
      vendor_prefix: prefix
    }
  }
}

impl Parse for ImageSet {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    let f = input.expect_function()?;
    let vendor_prefix = match_ignore_ascii_case! { &*f,
      "image-set" => VendorPrefix::None,
      "-webkit-image-set" => VendorPrefix::WebKit,
      _ => return Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(f.clone())
      ))
    };

    let options = input.parse_nested_block(|input| {
      input.parse_comma_separated(ImageSetOption::parse)
    })?;
    Ok(ImageSet {
      options,
      vendor_prefix
    })
  }
}

impl ToCss for ImageSet {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.vendor_prefix.to_css(dest)?;
    dest.write_str("image-set(")?;
    let mut first = true;
    for option in &self.options {
      if first {
        first = false;
      } else {
        dest.delim(',', false)?;
      }
      option.to_css(dest, self.vendor_prefix != VendorPrefix::None)?;
    }
    dest.write_char(')')
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImageSetOption {
  pub image: Image,
  pub resolution: Resolution,
  pub file_type: Option<String>
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

impl ImageSetOption {
  fn to_css<W>(&self, dest: &mut Printer<W>, is_prefixed: bool) -> std::fmt::Result where W: std::fmt::Write {
    match &self.image {
      // Prefixed syntax didn't allow strings, only url()
      Image::Url(url) if !is_prefixed => serialize_string(&url, dest)?,
      _ => self.image.to_css(dest)?
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
