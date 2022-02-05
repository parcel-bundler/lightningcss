use cssparser::*;
use smallvec::SmallVec;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};

/// https://www.w3.org/TR/css-values-4/#custom-idents
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CustomIdent<'i>(pub CowRcStr<'i>);

impl<'i> Parse<'i> for CustomIdent<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident_cloned()?;
    let valid = match_ignore_ascii_case! { &ident,
      "initial" | "inherit" | "unset" | "default" | "revert" => false,
      _ => true
    };

    if !valid {
      return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())))
    }

    Ok(CustomIdent(ident))
  }
}

impl<'i> ToCss for CustomIdent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.write_ident(&self.0)
  }
}

pub type CustomIdentList<'i> = SmallVec<[CustomIdent<'i>; 1]>;
