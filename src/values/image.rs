use cssparser::*;

#[derive(Debug)]
pub enum Image {
  None,
  Url(String)
}

impl Image {
  pub fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Image::None)
    }
    
    if let Ok(url) = input.try_parse(|input| input.expect_url()) {
      return Ok(Image::Url(url.as_ref().into()))
    }

    Err(input.new_error_for_next_token())
  }
}
