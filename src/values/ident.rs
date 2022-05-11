//! CSS identifiers.

use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
use cssparser::*;
use smallvec::SmallVec;

/// A CSS [`<custom-ident>`](https://www.w3.org/TR/css-values-4/#custom-idents).
///
/// Custom idents are author defined, and allow any valid identifier except the
/// [CSS-wide keywords](https://www.w3.org/TR/css-values-4/#css-wide-keywords).
/// They may be renamed to include a hash when compiled as part of a CSS module.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CustomIdent<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CowArcStr<'i>);

impl<'i> Parse<'i> for CustomIdent<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    let valid = match_ignore_ascii_case! { &ident,
      "initial" | "inherit" | "unset" | "default" | "revert" | "revert-layer" => false,
      _ => true
    };

    if !valid {
      return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())));
    }

    Ok(CustomIdent(ident.into()))
  }
}

impl<'i> ToCss for CustomIdent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_ident(&self.0)
  }
}

/// A list of CSS [`<custom-ident>`](https://www.w3.org/TR/css-values-4/#custom-idents) values.
pub type CustomIdentList<'i> = SmallVec<[CustomIdent<'i>; 1]>;

/// A CSS [`<dashed-ident>`](https://www.w3.org/TR/css-values-4/#dashed-idents).
///
/// Dashed idents are used in cases where an identifier can be either author defined _or_ CSS-defined.
/// Author defined idents must start with two dash characters ("--") or parsing will fail.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DashedIdent<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CowArcStr<'i>);

impl<'i> Parse<'i> for DashedIdent<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    if !ident.starts_with("--") {
      return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())));
    }

    Ok(DashedIdent(ident.into()))
  }
}

impl<'i> ToCss for DashedIdent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_ident(&self.0)
  }
}
