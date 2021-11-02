use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;
use super::calc::Calc;
use std::f32::consts::PI;

#[derive(Debug, Clone)]
pub enum Angle {
  Deg(f32),
  Grad(f32),
  Rad(f32),
  Turn(f32)
}

impl Parse for Angle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      // Angles are always compatible, so they will always compute to a value.
      Ok(_) => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid)),
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
      },
      ref token => return Err(location.new_unexpected_token_error(token.clone())),
    }
  }
}

impl ToCss for Angle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
      },
      Angle::Turn(val) => (*val, "turn")
    };

    use cssparser::ToCss;
    let int_value = if value.fract() == 0.0 {
      Some(value as i32)
    } else {
      None
    };
    let token = Token::Dimension {
      has_sign: value < 0.0,
      value,
      int_value,
      unit: CowRcStr::from(unit)
    };
    if value != 0.0 && value.abs() < 1.0 {
      let mut s = String::new();
      token.to_css(&mut s)?;
      if value < 0.0 {
        dest.write_char('-')?;
        dest.write_str(s.trim_start_matches("-0"))
      } else {
        dest.write_str(s.trim_start_matches('0'))
      }
    } else {
      token.to_css(dest)
    }
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
      _ => unreachable!()
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
