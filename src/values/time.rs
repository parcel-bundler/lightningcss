use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;
use super::calc::Calc;

/// https://www.w3.org/TR/css3-values/#time-value
#[derive(Debug, Clone, PartialEq)]
pub enum Time {
  Seconds(f32),
  Milliseconds(f32),
  Calc(Calc<Time>)
}

impl Parse for Time {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    match *input.next()? {
      Token::Dimension { value, ref unit, .. } => {
        match_ignore_ascii_case! { unit,
          "s" => Ok(Time::Seconds(value)),
          "ms" => Ok(Time::Milliseconds(value)),
          _ => Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }
      }
      Token::Function(ref name) => {
        match_ignore_ascii_case! { name,
          "calc" => {
            let calc = Calc::parse(input)?;
            Ok(Time::Calc(calc))
          },
          _ => Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }
      }
      ref t => Err(location.new_unexpected_token_error(t.clone())),
    }
  }
}

impl ToCss for Time {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    // 0.1s is shorter than 100ms
    // anything smaller is longer
    match self {
      Time::Seconds(s) => {
        if *s > 0.0 && *s < 0.1 {
          (*s * 1000.0).to_css(dest)?;
          dest.write_str("ms")
        } else {
          s.to_css(dest)?;
          dest.write_str("s")
        }
      }
      Time::Milliseconds(ms) => {
        if *ms == 0.0 || *ms >= 100.0 {
          (*ms / 1000.0).to_css(dest)?;
          dest.write_str("s")
        } else {
          ms.to_css(dest)?;
          dest.write_str("ms")
        }
      }
      Time::Calc(calc) => {
        if let Calc::Value(v) = calc {
          v.to_css(dest)
        } else {
          dest.write_str("calc(")?;
          calc.to_css(dest)?;
          dest.write_char(')')
        }
      }
    }
  }
}

impl std::ops::Mul<f32> for Time {
  type Output = Self;

  fn mul(self, other: f32) -> Time {
    match self {
      Time::Seconds(t) => Time::Seconds(t * other),
      Time::Milliseconds(t) => Time::Milliseconds(t * other),
      Time::Calc(c) => Time::Calc(c * other)
    }
  }
}

impl std::ops::Add<Time> for Time {
  type Output = Self;

  fn add(self, other: Time) -> Time {
    match (self, other) {
      (Time::Seconds(a), Time::Seconds(b)) => Time::Seconds(a + b),
      (Time::Milliseconds(a), Time::Milliseconds(b)) => Time::Milliseconds(a + b),
      (Time::Seconds(a), Time::Milliseconds(b)) => Time::Seconds(a + b / 1000.0),
      (Time::Milliseconds(a), Time::Seconds(b)) => Time::Seconds(a + b * 1000.0),
      (Time::Calc(a), Time::Calc(b)) => Time::Calc(a + b),
      (Time::Calc(a), b) => Time::Calc(a + Calc::Value(Box::new(b))),
      (a, Time::Calc(b)) => Time::Calc(Calc::Value(Box::new(a)) + b),
    }
  }
}

impl std::cmp::PartialEq<f32> for Time {
  fn eq(&self, other: &f32) -> bool {
    match self {
      Time::Seconds(a) | Time::Milliseconds(a) => a == other,
      Time::Calc(_) => false
    }
  }
}

impl std::cmp::PartialOrd<f32> for Time {
  fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
    match self {
      Time::Seconds(a) | Time::Milliseconds(a) => a.partial_cmp(other),
      Time::Calc(_) => None
    }
  }
}
