use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};

#[derive(Debug, Clone, PartialEq)]
pub struct Size2D<T>(pub T, pub T);

impl<'i, T> Parse<'i> for Size2D<T> where T: Parse<'i> + Clone {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let first = T::parse(input)?;
    let second = input.try_parse(T::parse).unwrap_or_else(|_| first.clone());
    Ok(Size2D(first, second))
  }
}

impl<T> ToCss for Size2D<T> where T: ToCss + PartialEq {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.0.to_css(dest)?;
    if self.1 != self.0 {
      dest.write_str(" ")?;
      self.1.to_css(dest)?;
    }
    Ok(())
  }
}
