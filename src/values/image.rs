use cssparser::*;
use crate::properties::VendorPrefix;
use crate::properties::prefixes::{Browsers};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::gradient::*;

/// https://www.w3.org/TR/css-images-3/#typedef-image
#[derive(Debug, Clone, PartialEq)]
pub enum Image {
  None,
  Url(String),
  Gradient(Gradient)
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
      Gradient(grad) => grad.to_css(dest)
    }
  }
}

