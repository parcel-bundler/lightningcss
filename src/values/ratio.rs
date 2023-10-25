//! CSS ratio values.

use super::number::CSSNumber;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A CSS [`<ratio>`](https://www.w3.org/TR/css-values-4/#ratios) value,
/// representing the ratio of two numeric values.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "visitor", visit(visit_ratio, RATIOS))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct Ratio(pub CSSNumber, pub CSSNumber);

impl<'i> Parse<'i> for Ratio {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let first = CSSNumber::parse(input)?;
    let second = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      CSSNumber::parse(input)?
    } else {
      1.0
    };

    Ok(Ratio(first, second))
  }
}

impl Ratio {
  /// Parses a ratio where both operands are required.
  pub fn parse_required<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let first = CSSNumber::parse(input)?;
    input.expect_delim('/')?;
    let second = CSSNumber::parse(input)?;
    Ok(Ratio(first, second))
  }
}

impl ToCss for Ratio {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.0.to_css(dest)?;
    if self.1 != 1.0 {
      dest.delim('/', true)?;
      self.1.to_css(dest)?;
    }
    Ok(())
  }
}

impl std::ops::Add<CSSNumber> for Ratio {
  type Output = Self;

  fn add(self, other: CSSNumber) -> Ratio {
    Ratio(self.0 + other, self.1)
  }
}
