use super::percentage::NumberOrPercentage;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use cssparser::*;

/// https://www.w3.org/TR/2021/WD-css-color-4-20210601/#typedef-alpha-value
#[derive(Debug, Clone, PartialEq)]
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
