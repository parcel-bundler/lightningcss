use cssparser::*;
use super::traits::Parse;

#[derive(Debug, Clone)]
pub enum Image {
  None,
  Url(String)
}

impl Parse for Image {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Image::None)
    }
    
    if let Ok(url) = input.try_parse(|input| input.expect_url()) {
      return Ok(Image::Url(url.as_ref().into()))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for Image {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    use Image::*;
    match self {
      None => dest.write_str("none"),
      Url(url) => {
        Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(dest)
      }
    }
  }
}
