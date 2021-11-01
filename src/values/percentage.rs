use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;
use super::calc::Calc;
use super::number::serialize_number;

/// https://drafts.csswg.org/css-values-4/#percentages
#[derive(Debug, Clone, PartialEq)]
pub struct Percentage(pub f32);

impl Parse for Percentage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      // Percentages are always compatible, so they will always compute to a value.
      Ok(_) => unreachable!(),
      _ => {}
    }

    let percent = input.expect_percentage()?;
    Ok(Percentage(percent))
  }
}

impl ToCss for Percentage {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use cssparser::ToCss;
    let int_value = if (self.0 * 100.0).fract() == 0.0 {
      Some(self.0 as i32)
    } else {
      None
    };
    let percent = Token::Percentage {
      has_sign: self.0 < 0.0,
      unit_value: self.0,
      int_value
    };
    if self.0 != 0.0 && self.0.abs() < 0.01 {
      let mut s = String::new();
      percent.to_css(&mut s)?;
      if self.0 < 0.0 {
        dest.write_char('-')?;
        dest.write_str(s.trim_start_matches("-0"))
      } else {
        dest.write_str(s.trim_start_matches('0'))
      }
    } else {
      percent.to_css(dest)
    }
  }
}

impl std::convert::Into<Calc<Percentage>> for Percentage {
  fn into(self) -> Calc<Percentage> {
    Calc::Value(Box::new(self))
  }
}

impl std::convert::From<Calc<Percentage>> for Percentage {
  fn from(calc: Calc<Percentage>) -> Percentage {
    match calc {
      Calc::Value(v) => *v,
      _ => unreachable!()
    }
  }
}

impl std::ops::Mul<f32> for Percentage {
  type Output = Self;

  fn mul(self, other: f32) -> Percentage {
    Percentage(self.0 * other)
  }
}

impl std::ops::Add<Percentage> for Percentage {
  type Output = Self;

  fn add(self, other: Percentage) -> Percentage {
    Percentage(self.0 + other.0)
  }
}

impl std::cmp::PartialEq<f32> for Percentage {
  fn eq(&self, other: &f32) -> bool {
    self.0 == *other
  }
}

impl std::cmp::PartialOrd<f32> for Percentage {
  fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
    self.0.partial_cmp(other)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberOrPercentage {
  Percentage(Percentage),
  Number(f32),
}

impl Parse for NumberOrPercentage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(percent) = input.try_parse(|input| Percentage::parse(input)) {
      return Ok(NumberOrPercentage::Percentage(percent))
    }

    if let Ok(number) = input.try_parse(|input| input.expect_number()) {
      return Ok(NumberOrPercentage::Number(number))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for NumberOrPercentage {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      NumberOrPercentage::Percentage(percent) => percent.to_css(dest),
      NumberOrPercentage::Number(number) => serialize_number(*number, dest)
    }
  }
}

impl std::cmp::PartialEq<f32> for NumberOrPercentage {
  fn eq(&self, other: &f32) -> bool {
    match self {
      NumberOrPercentage::Number(a) => *a == *other,
      NumberOrPercentage::Percentage(a) => a.0 == *other,
    }
  }
}

impl std::convert::Into<f32> for &NumberOrPercentage {
  fn into(self) -> f32 {
    match self {
      NumberOrPercentage::Number(a) => *a,
      NumberOrPercentage::Percentage(a) => a.0
    }
  }
}
