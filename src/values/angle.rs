//! CSS angle values.

use super::calc::{Calc, Round, RoundingStrategy};
use super::length::serialize_dimension;
use super::number::CSSNumber;
use super::percentage::DimensionPercentage;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{
  private::{AddInternal, TryAdd},
  Parse, ToCss,
};
use cssparser::*;
use std::f32::consts::PI;

/// A CSS [`<angle>`](https://www.w3.org/TR/css-values-4/#angles) value.
///
/// Angles may be explicit or computed by `calc()`, but are always stored and serialized
/// as their computed value.
#[derive(Debug, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Angle {
  /// An angle in degrees. There are 360 degrees in a full circle.
  Deg(CSSNumber),
  /// An angle in radians. There are 2π radians in a full circle.
  Rad(CSSNumber),
  /// An angle in gradians. There are 400 gradians in a full circle.
  Grad(CSSNumber),
  /// An angle in turns. There is 1 turn in a full circle.
  Turn(CSSNumber),
}

impl<'i> Parse<'i> for Angle {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      // Angles are always compatible, so they will always compute to a value.
      Ok(_) => unreachable!(),
      _ => {}
    }

    let location = input.current_source_location();
    let token = input.next()?;
    match *token {
      Token::Dimension { value, ref unit, .. } => {
        match_ignore_ascii_case! { unit,
          "deg" => Ok(Angle::Deg(value)),
          "grad" => Ok(Angle::Grad(value)),
          "turn" => Ok(Angle::Turn(value)),
          "rad" => Ok(Angle::Rad(value)),
          _ => return Err(location.new_unexpected_token_error(token.clone())),
        }
      }
      ref token => return Err(location.new_unexpected_token_error(token.clone())),
    }
  }
}

impl ToCss for Angle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let (value, unit) = match self {
      Angle::Deg(val) => (*val, "deg"),
      Angle::Grad(val) => (*val, "grad"),
      Angle::Rad(val) => {
        let deg = self.to_degrees();
        // We print 5 digits of precision by default.
        // Switch to degrees if there are an even number of them.
        if (deg * 100000.0).round().fract() == 0.0 {
          (deg, "deg")
        } else {
          (*val, "rad")
        }
      }
      Angle::Turn(val) => (*val, "turn"),
    };

    serialize_dimension(value, unit, dest)
  }
}

impl Angle {
  /// Returns whether the angle is zero.
  pub fn is_zero(&self) -> bool {
    use Angle::*;
    match self {
      Deg(v) | Rad(v) | Grad(v) | Turn(v) => *v == 0.0,
    }
  }

  /// Returns the angle in radians.
  pub fn to_radians(&self) -> CSSNumber {
    const RAD_PER_DEG: f32 = PI / 180.0;
    match self {
      Angle::Deg(deg) => deg * RAD_PER_DEG,
      Angle::Rad(rad) => *rad,
      Angle::Grad(grad) => grad * 180.0 / 200.0 * RAD_PER_DEG,
      Angle::Turn(turn) => turn * 360.0 * RAD_PER_DEG,
    }
  }

  /// Returns the angle in degrees.
  pub fn to_degrees(&self) -> CSSNumber {
    const DEG_PER_RAD: f32 = 180.0 / PI;
    match self {
      Angle::Deg(deg) => *deg,
      Angle::Rad(rad) => rad * DEG_PER_RAD,
      Angle::Grad(grad) => grad * 180.0 / 200.0,
      Angle::Turn(turn) => turn * 360.0,
    }
  }
}

impl std::convert::Into<Calc<Angle>> for Angle {
  fn into(self) -> Calc<Angle> {
    Calc::Value(Box::new(self))
  }
}

impl std::convert::From<Calc<Angle>> for Angle {
  fn from(calc: Calc<Angle>) -> Angle {
    match calc {
      Calc::Value(v) => *v,
      _ => unreachable!(),
    }
  }
}

impl std::ops::Mul<CSSNumber> for Angle {
  type Output = Self;

  fn mul(self, other: CSSNumber) -> Angle {
    match self {
      Angle::Deg(v) => Angle::Deg(v * other),
      Angle::Rad(v) => Angle::Rad(v * other),
      Angle::Grad(v) => Angle::Grad(v * other),
      Angle::Turn(v) => Angle::Turn(v * other),
    }
  }
}

impl std::ops::Add<Angle> for Angle {
  type Output = Self;

  fn add(self, other: Angle) -> Angle {
    Angle::Deg(self.to_degrees() + other.to_degrees())
  }
}

impl AddInternal for Angle {
  fn add(self, other: Self) -> Self {
    self + other
  }
}

impl TryAdd<Angle> for Angle {
  fn try_add(&self, other: &Angle) -> Option<Angle> {
    Some(Angle::Deg(self.to_degrees() + other.to_degrees()))
  }
}

impl std::cmp::PartialEq<CSSNumber> for Angle {
  fn eq(&self, other: &CSSNumber) -> bool {
    match self {
      Angle::Deg(a) | Angle::Rad(a) | Angle::Grad(a) | Angle::Turn(a) => a == other,
    }
  }
}

impl std::cmp::PartialEq<Angle> for Angle {
  fn eq(&self, other: &Angle) -> bool {
    self.to_degrees() == other.to_degrees()
  }
}

impl std::cmp::PartialOrd<CSSNumber> for Angle {
  fn partial_cmp(&self, other: &CSSNumber) -> Option<std::cmp::Ordering> {
    match self {
      Angle::Deg(a) | Angle::Rad(a) | Angle::Grad(a) | Angle::Turn(a) => a.partial_cmp(other),
    }
  }
}

impl std::cmp::PartialOrd<Angle> for Angle {
  fn partial_cmp(&self, other: &Angle) -> Option<std::cmp::Ordering> {
    self.to_degrees().partial_cmp(&other.to_degrees())
  }
}

impl Round for Angle {
  fn round(&self, to: &Self, strategy: RoundingStrategy) -> Self {
    match (self, to) {
      (Angle::Deg(a), Angle::Deg(b)) => Angle::Deg(Round::round(a, b, strategy)),
      (Angle::Rad(a), Angle::Rad(b)) => Angle::Rad(Round::round(a, b, strategy)),
      (Angle::Grad(a), Angle::Grad(b)) => Angle::Grad(Round::round(a, b, strategy)),
      (Angle::Turn(a), Angle::Turn(b)) => Angle::Turn(Round::round(a, b, strategy)),
      (a, b) => Angle::Deg(Round::round(&a.to_degrees(), &b.to_degrees(), strategy)),
    }
  }
}

impl std::ops::Rem for Angle {
  type Output = Angle;

  fn rem(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Angle::Deg(a), Angle::Deg(b)) => Angle::Deg(a % b),
      (Angle::Rad(a), Angle::Rad(b)) => Angle::Rad(a % b),
      (Angle::Grad(a), Angle::Grad(b)) => Angle::Grad(a % b),
      (Angle::Turn(a), Angle::Turn(b)) => Angle::Turn(a % b),
      (a, b) => Angle::Deg(a.to_degrees() % b.to_degrees()),
    }
  }
}

/// A CSS [`<angle-percentage>`](https://www.w3.org/TR/css-values-4/#typedef-angle-percentage) value.
/// May be specified as either an angle or a percentage that resolves to an angle.
pub type AnglePercentage = DimensionPercentage<Angle>;
