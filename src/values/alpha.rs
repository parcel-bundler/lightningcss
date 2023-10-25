//! CSS alpha values, used to represent opacity.

use super::percentage::NumberOrPercentage;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A CSS [`<alpha-value>`](https://www.w3.org/TR/css-color-4/#typedef-alpha-value),
/// used to represent opacity.
///
/// Parses either a `<number>` or `<percentage>`, but is always stored and serialized as a number.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct AlphaValue(pub f32);

impl<'i> Parse<'i> for AlphaValue {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match NumberOrPercentage::parse(input)? {
      NumberOrPercentage::Percentage(percent) => Ok(AlphaValue(percent.0)),
      NumberOrPercentage::Number(number) => Ok(AlphaValue(number)),
    }
  }
}

impl ToCss for AlphaValue {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.0.to_css(dest)
  }
}
