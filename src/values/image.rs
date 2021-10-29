use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;

/// https://www.w3.org/TR/css-images-3/#typedef-image
#[derive(Debug, Clone, PartialEq)]
pub enum Image {
  None,
  Url(String)
}

impl Default for Image {
  fn default() -> Image {
    Image::None
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

    // TODO: gradients

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
    }
  }
}
