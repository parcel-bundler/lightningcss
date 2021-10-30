use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;

/// https://www.w3.org/TR/css-values-4/#custom-idents
#[derive(Debug, Clone, PartialEq)]
pub struct CustomIdent(pub String);

impl Parse for CustomIdent {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    let valid = match_ignore_ascii_case! { ident,
      "initial" | "inherit" | "unset" | "default" | "revert" => false,
      _ => true
    };

    if !valid {
      return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())))
    }

    Ok(CustomIdent(ident.as_ref().into()))
  }
}

impl ToCss for CustomIdent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    serialize_identifier(&self.0, dest)
  }
}
