use super::length::serialize_dimension;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use cssparser::*;

/// https://drafts.csswg.org/css-values-4/#resolution-value
#[derive(Debug, Clone, PartialEq)]
pub enum Resolution {
  Dpi(f32),
  Dpcm(f32),
  Dppx(f32),
}

impl<'i> Parse<'i> for Resolution {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    match *input.next()? {
      Token::Dimension { value, ref unit, .. } => {
        match_ignore_ascii_case! { unit,
          "dpi" => Ok(Resolution::Dpi(value)),
          "dpcm" => Ok(Resolution::Dpcm(value)),
          "dppx" | "x" => Ok(Resolution::Dppx(value)),
          _ => Err(location.new_unexpected_token_error(Token::Ident(unit.clone())))
        }
      }
      ref t => Err(location.new_unexpected_token_error(t.clone())),
    }
  }
}

impl ToCss for Resolution {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let (value, unit) = match self {
      Resolution::Dpi(dpi) => (*dpi, "dpi"),
      Resolution::Dpcm(dpcm) => (*dpcm, "dpcm"),
      Resolution::Dppx(dppx) => (*dppx, "x"),
    };

    serialize_dimension(value, unit, dest)
  }
}

impl std::ops::Add<f32> for Resolution {
  type Output = Self;

  fn add(self, other: f32) -> Resolution {
    match self {
      Resolution::Dpi(dpi) => Resolution::Dpi(dpi + other),
      Resolution::Dpcm(dpcm) => Resolution::Dpcm(dpcm + other),
      Resolution::Dppx(dppx) => Resolution::Dppx(dppx + other),
    }
  }
}
