use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};

/// https://drafts.csswg.org/css-values-4/#ratios
#[derive(Debug, Clone, PartialEq)]
pub struct Ratio(pub f32, pub f32);

impl Parse for Ratio {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let first = f32::parse(input)?;
    let second = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      f32::parse(input)?
    } else {
      1.0
    };

    Ok(Ratio(first, second))
  }
}

impl Ratio {
  pub fn parse_required<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let first = f32::parse(input)?;
    input.expect_delim('/')?;
    let second = f32::parse(input)?;
    Ok(Ratio(first, second))
  }
}

impl ToCss for Ratio {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.0.to_css(dest)?;
    if self.1 != 1.0 {
      dest.delim('/', true)?;
      self.1.to_css(dest)?;
    }
    Ok(())
  }
}

impl std::ops::Add<f32> for Ratio {
  type Output = Self;
  
  fn add(self, other: f32) -> Ratio {
    Ratio(self.0 + other, self.1)
  }
}
