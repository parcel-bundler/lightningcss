//! CSS time values.

use super::calc::Calc;
use super::number::CSSNumber;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use cssparser::*;

/// A CSS [`<time>`](https://www.w3.org/TR/css-values-4/#time) value, in either
/// seconds or milliseconds.
///
/// Time values may be explicit or computed by `calc()`, but are always stored and serialized
/// as their computed value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Time {
  /// A time in seconds.
  Seconds(CSSNumber),
  /// A time in milliseconds.
  Milliseconds(CSSNumber),
}

impl Time {
  /// Returns the time in milliseconds.
  pub fn to_ms(&self) -> CSSNumber {
    match self {
      Time::Seconds(s) => s * 1000.0,
      Time::Milliseconds(ms) => *ms,
    }
  }
}

impl<'i> Parse<'i> for Time {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      // Time is always compatible, so they will always compute to a value.
      Ok(_) => unreachable!(),
      _ => {}
    }

    let location = input.current_source_location();
    match *input.next()? {
      Token::Dimension { value, ref unit, .. } => {
        match_ignore_ascii_case! { unit,
          "s" => Ok(Time::Seconds(value)),
          "ms" => Ok(Time::Milliseconds(value)),
          _ => Err(location.new_unexpected_token_error(Token::Ident(unit.clone())))
        }
      }
      ref t => Err(location.new_unexpected_token_error(t.clone())),
    }
  }
}

impl ToCss for Time {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
    }
  }
}

impl std::convert::Into<Calc<Time>> for Time {
  fn into(self) -> Calc<Time> {
    Calc::Value(Box::new(self))
  }
}

impl std::convert::From<Calc<Time>> for Time {
  fn from(calc: Calc<Time>) -> Time {
    match calc {
      Calc::Value(v) => *v,
      _ => unreachable!(),
    }
  }
}

impl std::ops::Mul<f32> for Time {
  type Output = Self;

  fn mul(self, other: f32) -> Time {
    match self {
      Time::Seconds(t) => Time::Seconds(t * other),
      Time::Milliseconds(t) => Time::Milliseconds(t * other),
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
    }
  }
}

impl std::cmp::PartialEq<f32> for Time {
  fn eq(&self, other: &f32) -> bool {
    match self {
      Time::Seconds(a) | Time::Milliseconds(a) => a == other,
    }
  }
}

impl std::cmp::PartialOrd<f32> for Time {
  fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
    match self {
      Time::Seconds(a) | Time::Milliseconds(a) => a.partial_cmp(other),
    }
  }
}

impl std::cmp::PartialOrd<Time> for Time {
  fn partial_cmp(&self, other: &Time) -> Option<std::cmp::Ordering> {
    self.to_ms().partial_cmp(&other.to_ms())
  }
}
