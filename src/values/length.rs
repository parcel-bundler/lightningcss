use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::macros::enum_property;
use crate::printer::Printer;
use std::fmt::Write;
use super::calc::Calc;
use super::percentage::Percentage;
use super::number::serialize_number;

/// https://drafts.csswg.org/css-values-4/#typedef-length-percentage
#[derive(Debug, Clone, PartialEq)]
pub enum LengthPercentage {
  Length(Length),
  Percentage(Percentage),
  Calc(Calc<LengthPercentage>)
}

impl Parse for LengthPercentage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let c = input.try_parse(Calc::parse);
    match c {
      Ok(Calc::Value(v)) => return Ok(*v),
      Ok(calc) => return Ok(LengthPercentage::Calc(calc)),
      _ => {}
    }

    if let Ok(length) = input.try_parse(|input| Length::parse(input)) {
      return Ok(LengthPercentage::Length(length))
    }

    if let Ok(percent) = input.try_parse(|input| Percentage::parse(input)) {
      return Ok(LengthPercentage::Percentage(percent))
    }

    Err(input.new_error_for_next_token())
  }
}

impl std::ops::Mul<f32> for LengthPercentage {
  type Output = Self;

  fn mul(self, other: f32) -> LengthPercentage {
    match self {
      LengthPercentage::Length(l) => LengthPercentage::Length(l * other),
      LengthPercentage::Percentage(p) => LengthPercentage::Percentage(Percentage(p.0 * other)),
      LengthPercentage::Calc(c) => LengthPercentage::Calc(c * other)
    }
  }
}

impl std::ops::Add<LengthPercentage> for LengthPercentage {
  type Output = Self;

  fn add(self, other: LengthPercentage) -> LengthPercentage {
    match self.add_recursive(&other) {
      Some(r) => r,
      None => self.add(other)
    }
  }
}

impl LengthPercentage {
  fn add_recursive(&self, other: &LengthPercentage) -> Option<LengthPercentage> {
    match (self, other) {
      (LengthPercentage::Length(a), LengthPercentage::Length(b)) if a.unit == b.unit => Some(LengthPercentage::Length(a.clone() + b.clone())),
      (LengthPercentage::Length(a), LengthPercentage::Length(b)) => {
        if let (Some(a), Some(b)) = (a.to_px(), b.to_px()) {
          Some(LengthPercentage::Length(Length { value: a + b, unit: Unit::Px }))
        } else {
          None
        }
      },
      (LengthPercentage::Percentage(a), LengthPercentage::Percentage(b)) => Some(LengthPercentage::Percentage(Percentage(a.0 + b.0))),
      (LengthPercentage::Calc(Calc::Value(v)), other) => v.add_recursive(other),
      (other, LengthPercentage::Calc(Calc::Value(v))) => other.add_recursive(v),
      (LengthPercentage::Calc(Calc::Sum(a, b)), other) => {
        if let Some(res) = LengthPercentage::Calc(*a.clone()).add_recursive(other) {
          return Some(res.add(LengthPercentage::from(*b.clone())))
        }

        if let Some(res) = LengthPercentage::Calc(*b.clone()).add_recursive(other) {
          return Some(LengthPercentage::from(*a.clone()).add(res))
        }

        None
      }
      (other, LengthPercentage::Calc(Calc::Sum(a, b))) => {
        if let Some(res) = other.add_recursive(&LengthPercentage::Calc(*a.clone())) {
          return Some(res.add(LengthPercentage::from(*b.clone())))
        }

        if let Some(res) = other.add_recursive(&LengthPercentage::Calc(*b.clone())) {
          return Some(LengthPercentage::from(*a.clone()).add(res))
        }

        None
      }
      _ => None
    }
  }

  fn add(self, other: LengthPercentage) -> LengthPercentage {
    let mut a = self;
    let mut b = other;

    if a == 0.0 {
      return b
    }

    if b == 0.0 {
      return a
    }

    if a < 0.0 && b > 0.0 {
      std::mem::swap(&mut a, &mut b);
    }
    
    match (a, b) {
      (LengthPercentage::Calc(a), LengthPercentage::Calc(b)) => LengthPercentage::Calc(a + b),
      (LengthPercentage::Calc(Calc::Value(a)), b) => a.add(b),
      (a, LengthPercentage::Calc(Calc::Value(b))) => a.add(*b),
      (a, b) => LengthPercentage::Calc(Calc::Sum(Box::new(a.into()), Box::new(b.into())))
    }
  }
}

impl std::convert::Into<Calc<LengthPercentage>> for LengthPercentage {
  fn into(self) -> Calc<LengthPercentage> {
    match self {
      LengthPercentage::Calc(c) => c,
      b => Calc::Value(Box::new(b))
    }
  }
}

impl std::convert::From<Calc<LengthPercentage>> for LengthPercentage {
  fn from(calc: Calc<LengthPercentage>) -> LengthPercentage {
    LengthPercentage::Calc(calc)
  }
}

impl std::cmp::PartialEq<f32> for LengthPercentage {
  fn eq(&self, other: &f32) -> bool {
    match self {
      LengthPercentage::Length(a) => a.value == *other,
      LengthPercentage::Percentage(a) => a.0 == *other,
      LengthPercentage::Calc(_) => false
    }
  }
}

impl std::cmp::PartialOrd<f32> for LengthPercentage {
  fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
    match self {
      LengthPercentage::Length(a) => a.value.partial_cmp(other),
      LengthPercentage::Percentage(a) => a.0.partial_cmp(other),
      LengthPercentage::Calc(_) => None
    }
  }
}

impl ToCss for LengthPercentage {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      LengthPercentage::Length(length) => length.to_css(dest),
      LengthPercentage::Percentage(percent) => percent.to_css(dest),
      LengthPercentage::Calc(calc) => calc.to_css(dest)
    }
  }
}

/// `<length-percentage> | auto`
#[derive(Debug, Clone, PartialEq)]
pub enum LengthPercentageOrAuto {
  Auto,
  LengthPercentage(LengthPercentage)
}

impl Parse for LengthPercentageOrAuto {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("auto")).is_ok() {
      return Ok(LengthPercentageOrAuto::Auto);
    }

    if let Ok(percent) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(LengthPercentageOrAuto::LengthPercentage(percent))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for LengthPercentageOrAuto {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use LengthPercentageOrAuto::*;
    match self {
      Auto => dest.write_str("auto"),
      LengthPercentage(l) => l.to_css(dest),
      _ => Ok(())
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unit {
  Px,
  In,
  Cm,
  Mm,
  Q,
  Pt,
  Pc,
  Em,
  Ex,
  Ch,
  Rem,
  Vw,
  Vh,
  Vmin,
  Vmax,
}

impl Unit {
  pub fn parse<'i, 't>(unit: &str) -> Result<Self, ()> {
    use Unit::*;
    Ok(match_ignore_ascii_case! { unit,
      "px" => Px,
      "in" => In,
      "cm" => Cm,
      "mm" => Mm,
      "q" => Q,
      "pt" => Pt,
      "pc" => Pc,
      "em" => Em,
      "ex" => Ex,
      "ch" => Ch,
      "rem" => Rem,
      "vw" => Vw,
      "vh" => Vh,
      "vmin" => Vmin,
      "vmax" => Vmax,
      _ => return Err(()),
    })
  }

  pub fn as_str(&self) -> &str {
    use Unit::*;
    match self {
      Px => "px",
      In => "in",
      Cm => "cm",
      Mm => "mm",
      Q => "q",
      Pt => "pt",
      Pc => "pc",
      Em => "em",
      Ex => "ex",
      Ch => "ch",
      Rem => "rem",
      Vw => "vw",
      Vh => "vh",
      Vmin => "vmin",
      Vmax => "vmax"
    }
  }
}

/// https://drafts.csswg.org/css-values-4/#lengths
#[derive(Debug, Clone, PartialEq)]
pub struct Length {
  pub value: f32,
  pub unit: Unit
}

impl Length {
  pub fn zero() -> Length {
    Length { value: 0.0, unit: Unit::Px }
  }

  pub fn is_zero(&self) -> bool {
    self.value == 0.0
  }
}

impl Parse for Length {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    let token = input.next()?;
    match *token {
      Token::Dimension { value, ref unit, .. } => {
        Unit::parse(unit)
          .map(|unit| Length {
            value,
            unit
          })
          .map_err(|()| location.new_unexpected_token_error(token.clone()))
      },
      Token::Number { value, .. } => {
        // TODO: quirks mode only?
        Ok(Length {
          value,
          unit: Unit::Px
        })
      }
      ref token => return Err(location.new_unexpected_token_error(token.clone())),
    }
  }
}

impl ToCss for Length {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use cssparser::ToCss;
    if self.value == 0.0 {
      return dest.write_char('0')
    }

    let int_value = if self.value.fract() == 0.0 {
      Some(self.value as i32)
    } else {
      None
    };
    let token = Token::Dimension {
      has_sign: self.value < 0.0,
      value: self.value,
      int_value,
      unit: CowRcStr::from(self.unit.as_str())
    };
    if self.value.abs() < 1.0 {
      let mut s = String::new();
      token.to_css(&mut s)?;
      if self.value < 0.0 {
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

impl std::ops::Mul<f32> for Length {
  type Output = Self;

  fn mul(self, other: f32) -> Length {
    Length {
      value: self.value * other,
      unit: self.unit
    }
  }
}

impl std::ops::Add<Length> for Length {
  type Output = Self;

  fn add(self, other: Length) -> Length {
    Length {
      value: self.value + other.value,
      unit: self.unit
    }
  }
}

// https://www.w3.org/TR/css-values-3/#absolute-lengths
const PX_PER_IN: f32 = 96.0;
const PX_PER_CM: f32 = PX_PER_IN / 2.54;
const PX_PER_MM: f32 = PX_PER_CM / 10.0;
const PX_PER_Q: f32 = PX_PER_CM / 40.0;
const PX_PER_PT: f32 = PX_PER_IN / 72.0;
const PX_PER_PC: f32 = PX_PER_IN / 6.0;

impl Length {
  pub fn to_px(&self) -> Option<f32> {
    match self.unit {
      Unit::Px => Some(self.value),
      Unit::In => Some(self.value * PX_PER_IN),
      Unit::Cm => Some(self.value * PX_PER_CM),
      Unit::Mm => Some(self.value * PX_PER_MM),
      Unit::Q => Some(self.value * PX_PER_Q),
      Unit::Pt => Some(self.value * PX_PER_PT),
      Unit::Pc => Some(self.value * PX_PER_PC),
      _ => None
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LengthOrNumber {
  Length(Length),
  Number(f32)
}

impl Parse for LengthOrNumber {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    // Parse number first so unitless numbers are not parsed as lengths.
    if let Ok(number) = input.try_parse(|input| input.expect_number()) {
      return Ok(LengthOrNumber::Number(number))
    }

    if let Ok(length) = Length::parse(input) {
      return Ok(LengthOrNumber::Length(length))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for LengthOrNumber {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      LengthOrNumber::Length(length) => length.to_css(dest),
      LengthOrNumber::Number(number) => serialize_number(*number, dest)
    }
  }
}
