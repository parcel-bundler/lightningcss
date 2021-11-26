use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::length::serialize_dimension;

/// https://drafts.csswg.org/css-values-4/#resolution-value
#[derive(Debug, Clone, PartialEq)]
pub enum Resolution {
  Dpi(f32),
  Dpcm(f32),
  Dppx(f32)
}

impl Parse for Resolution {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    match *input.next()? {
      Token::Dimension { value, ref unit, .. } => {
        match_ignore_ascii_case! { unit,
          "dpi" => Ok(Resolution::Dpi(value)),
          "dpcm" => Ok(Resolution::Dpcm(value)),
          "dppx" | "x" => Ok(Resolution::Dppx(value)),
          _ => Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }
      }
      ref t => Err(location.new_unexpected_token_error(t.clone())),
    }
  }
}

impl ToCss for Resolution {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let (value, unit) = match self {
      Resolution::Dpi(dpi) => (*dpi, "dpi"),
      Resolution::Dpcm(dpcm) => (*dpcm, "dpcm"),
      Resolution::Dppx(dppx) => (*dppx, "x")
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
      Resolution::Dppx(dppx) => Resolution::Dppx(dppx + other)
    }
  }
}
