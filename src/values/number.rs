use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::calc::Calc;

impl Parse for f32 {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      Ok(Calc::Number(n)) => return Ok(n),
      // Numbers are always compatible, so they will always compute to a value.
      Ok(_) => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid)),
      _ => {}
    }

    if let Ok(number) = input.try_parse(|input| input.expect_number()) {
      return Ok(number)
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for f32 {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    serialize_number(*self, dest)
  }
}

impl std::convert::Into<Calc<f32>> for f32 {
  fn into(self) -> Calc<f32> {
    Calc::Value(Box::new(self))
  }
}

impl std::convert::From<Calc<f32>> for f32 {
  fn from(calc: Calc<f32>) -> f32 {
    match calc {
      Calc::Value(v) => *v,
      _ => unreachable!()
    }
  }
}

pub fn serialize_number<W>(number: f32, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
  use cssparser::ToCss;
  let int_value = if number.fract() == 0.0 {
    Some(number as i32)
  } else {
    None
  };
  let tok = Token::Number {
    has_sign: number < 0.0,
    value: number,
    int_value
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
    tok.to_css(dest)
  }
}
