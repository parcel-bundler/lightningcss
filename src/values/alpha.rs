use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::percentage::NumberOrPercentage;

/// https://www.w3.org/TR/2021/WD-css-color-4-20210601/#typedef-alpha-value
#[derive(Debug, Clone, PartialEq)]
pub struct AlphaValue(f32);

impl Parse for AlphaValue {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    match NumberOrPercentage::parse(input)? {
      NumberOrPercentage::Percentage(percent) => Ok(AlphaValue(percent.0)),
      NumberOrPercentage::Number(number) => Ok(AlphaValue(number))
    }
  }
}

impl ToCss for AlphaValue {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.0.to_css(dest)
  }
}
