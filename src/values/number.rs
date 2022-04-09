//! CSS number values.

use super::calc::Calc;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use cssparser::*;

/// A CSS [`<number>`](https://www.w3.org/TR/css-values-4/#numbers) value.
///
/// Numbers may be explicit or computed by `calc()`, but are always stored and serialized
/// as their computed value.
pub type CSSNumber = f32;

impl<'i> Parse<'i> for CSSNumber {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      Ok(Calc::Number(n)) => return Ok(n),
      // Numbers are always compatible, so they will always compute to a value.
      Ok(_) => unreachable!(),
      _ => {}
    }

    let number = input.expect_number()?;
    Ok(number)
  }
}

impl ToCss for CSSNumber {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use cssparser::ToCss;
    let number = *self;
    let int_value = if number.fract() == 0.0 {
      Some(number as i32)
    } else {
      None
    };
    let tok = Token::Number {
      has_sign: number < 0.0,
      value: number,
      int_value,
    };
    if number != 0.0 && number.abs() < 1.0 {
      let mut s = String::new();
      tok.to_css(&mut s)?;
      if number < 0.0 {
        dest.write_char('-')?;
        dest.write_str(s.trim_start_matches("-0"))
      } else {
        dest.write_str(s.trim_start_matches('0'))
      }
    } else {
      tok.to_css(dest)?;
      Ok(())
    }
  }
}

impl std::convert::Into<Calc<CSSNumber>> for CSSNumber {
  fn into(self) -> Calc<CSSNumber> {
    Calc::Value(Box::new(self))
  }
}

impl std::convert::From<Calc<CSSNumber>> for CSSNumber {
  fn from(calc: Calc<CSSNumber>) -> CSSNumber {
    match calc {
      Calc::Value(v) => *v,
      _ => unreachable!(),
    }
  }
}

/// A CSS [`<integer>`](https://www.w3.org/TR/css-values-4/#integers) value.
pub type CSSInteger = i32;

impl<'i> Parse<'i> for CSSInteger {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // TODO: calc??
    let integer = input.expect_integer()?;
    Ok(integer)
  }
}

impl ToCss for CSSInteger {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use cssparser::ToCss;
    let integer = *self;
    let tok = Token::Number {
      has_sign: integer < 0,
      value: integer as f32,
      int_value: Some(integer),
    };
    tok.to_css(dest)?;
    Ok(())
  }
}
