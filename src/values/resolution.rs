//! CSS resolution values.

use super::length::serialize_dimension;
use super::number::CSSNumber;
use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use cssparser::*;

/// A CSS [`<resolution>`](https://www.w3.org/TR/css-values-4/#resolution) value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Resolution {
  /// A resolution in dots per inch.
  Dpi(CSSNumber),
  /// A resolution in dots per centimeter.
  Dpcm(CSSNumber),
  /// A resolution in dots per px.
  Dppx(CSSNumber),
}

impl<'i> Parse<'i> for Resolution {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // TODO: calc?
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
      Resolution::Dppx(dppx) => {
        if let Some(targets) = dest.targets {
          if Feature::XResolutionUnit.is_compatible(targets) {
            (*dppx, "x")
          } else {
            (*dppx, "dppx")
          }
        } else {
          (*dppx, "x")
        }
      }
    };

    serialize_dimension(value, unit, dest)
  }
}

impl std::ops::Add<CSSNumber> for Resolution {
  type Output = Self;

  fn add(self, other: CSSNumber) -> Resolution {
    match self {
      Resolution::Dpi(dpi) => Resolution::Dpi(dpi + other),
      Resolution::Dpcm(dpcm) => Resolution::Dpcm(dpcm + other),
      Resolution::Dppx(dppx) => Resolution::Dppx(dppx + other),
    }
  }
}
