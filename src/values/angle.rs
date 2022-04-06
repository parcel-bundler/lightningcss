use super::calc::Calc;
use super::length::serialize_dimension;
use super::percentage::DimensionPercentage;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{private::TryAdd, Parse, ToCss};
use cssparser::*;
use std::f32::consts::PI;

#[derive(Debug, Clone)]
pub enum Angle {
  Deg(f32),
  Grad(f32),
  Rad(f32),
  Turn(f32),
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
  pub fn is_zero(&self) -> bool {
    use Angle::*;
    match self {
      Deg(v) | Rad(v) | Grad(v) | Turn(v) => *v == 0.0,
    }
  }

  pub fn to_radians(&self) -> f32 {
    const RAD_PER_DEG: f32 = PI / 180.0;
    match self {
      Angle::Deg(deg) => deg * RAD_PER_DEG,
      Angle::Rad(rad) => *rad,
      Angle::Grad(grad) => grad * 180.0 / 200.0 * RAD_PER_DEG,
      Angle::Turn(turn) => turn * 360.0 * RAD_PER_DEG,
    }
  }

  pub fn to_degrees(&self) -> f32 {
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

impl std::ops::Mul<f32> for Angle {
  type Output = Self;

  fn mul(self, other: f32) -> Angle {
    match self {
      Angle::Deg(v) => Angle::Deg(v * other),
      Angle::Rad(v) => Angle::Deg(v * other),
      Angle::Grad(v) => Angle::Deg(v * other),
      Angle::Turn(v) => Angle::Deg(v * other),
    }
  }
}

impl std::ops::Add<Angle> for Angle {
  type Output = Self;

  fn add(self, other: Angle) -> Angle {
    Angle::Deg(self.to_degrees() + other.to_degrees())
  }
}

impl TryAdd<Angle> for Angle {
  fn try_add(&self, other: &Angle) -> Option<Angle> {
    Some(Angle::Deg(self.to_degrees() + other.to_degrees()))
  }
}

impl std::cmp::PartialEq<f32> for Angle {
  fn eq(&self, other: &f32) -> bool {
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

impl std::cmp::PartialOrd<f32> for Angle {
  fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
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

/// https://drafts.csswg.org/css-values-4/#typedef-angle-percentage
pub type AnglePercentage = DimensionPercentage<Angle>;
