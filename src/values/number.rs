//! CSS number values.

use super::angle::impl_try_from_angle;
use super::calc::Calc;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::private::AddInternal;
use crate::traits::{Map, Op, Parse, Sign, ToCss, Zero};
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
      Ok(_) => return Err(input.new_custom_error(ParserError::InvalidValue)),
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
    let number = *self;
    // Handle NaN
    if number.is_nan() {
      return dest.write_str("0");
    }
    // Handle infinity
    if number.is_infinite() {
      if number.is_sign_negative() {
        return dest.write_str("calc(-1/0)");
      } else {
        return dest.write_str("calc(1/0)");
      }
    }
    if number != 0.0 && number.abs() < 1.0 {
      let mut s = String::new();
      cssparser::ToCss::to_css(self, &mut s)?;
      if number < 0.0 {
        dest.write_char('-')?;
        dest.write_str(s.trim_start_matches("-").trim_start_matches("0"))
      } else {
        dest.write_str(s.trim_start_matches('0'))
      }
    } else {
      cssparser::ToCss::to_css(self, dest)?;
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
      Calc::Number(n) => n,
      _ => unreachable!(),
    }
  }
}

impl AddInternal for CSSNumber {
  fn add(self, other: Self) -> Self {
    self + other
  }
}

impl Op for CSSNumber {
  fn op<F: FnOnce(f32, f32) -> f32>(&self, to: &Self, op: F) -> Self {
    op(*self, *to)
  }

  fn op_to<T, F: FnOnce(f32, f32) -> T>(&self, rhs: &Self, op: F) -> T {
    op(*self, *rhs)
  }
}

impl Map for CSSNumber {
  fn map<F: FnOnce(f32) -> f32>(&self, op: F) -> Self {
    op(*self)
  }
}

impl Sign for CSSNumber {
  fn sign(&self) -> f32 {
    if *self == 0.0 {
      return if f32::is_sign_positive(*self) { 0.0 } else { -0.0 };
    }
    self.signum()
  }
}

impl Zero for CSSNumber {
  fn zero() -> Self {
    0.0
  }

  fn is_zero(&self) -> bool {
    *self == 0.0
  }
}

impl_try_from_angle!(CSSNumber);

/// A CSS [`<integer>`](https://www.w3.org/TR/css-values-4/#integers) value.
///
/// Integers may be explicit or computed by `calc()`, but are always stored and serialized
/// as their computed value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CSSInteger(pub i32);

impl std::ops::Deref for CSSInteger {
  type Target = i32;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl std::ops::DerefMut for CSSInteger {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl From<i32> for CSSInteger {
  fn from(v: i32) -> Self {
    CSSInteger(v)
  }
}

impl From<CSSInteger> for i32 {
  fn from(v: CSSInteger) -> Self {
    v.0
  }
}

impl From<CSSNumber> for CSSInteger {
  fn from(v: CSSNumber) -> Self {
    CSSInteger(v as i32)
  }
}

impl PartialEq<i32> for CSSInteger {
  fn eq(&self, other: &i32) -> bool {
    self.0 == *other
  }
}

impl PartialOrd<i32> for CSSInteger {
  fn partial_cmp(&self, other: &i32) -> Option<std::cmp::Ordering> {
    self.0.partial_cmp(other)
  }
}

impl std::fmt::Display for CSSInteger {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.0.fmt(f)
  }
}

impl std::ops::Add<i32> for CSSInteger {
  type Output = i32;

  fn add(self, other: i32) -> Self::Output {
    self.0 + other
  }
}

impl std::ops::Sub<i32> for CSSInteger {
  type Output = i32;

  fn sub(self, other: i32) -> Self::Output {
    self.0 - other
  }
}

impl std::ops::Neg for CSSInteger {
  type Output = Self;

  fn neg(self) -> Self::Output {
    CSSInteger(-self.0)
  }
}

impl<'i> Parse<'i> for CSSInteger {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      Ok(Calc::Number(n)) => {
        // Handle infinity and NaN from calc
        if n.is_infinite() {
          if n.is_sign_negative() {
            return Ok(CSSInteger(i32::MIN));
          } else {
            return Ok(CSSInteger(i32::MAX));
          }
        }
        // NaN rounds to 0 per CSS spec
        return Ok(CSSInteger(n.round() as i32));
      }
      // Numbers are always compatible, so they will always compute to a value.
      Ok(_) => return Err(input.new_custom_error(ParserError::InvalidValue)),
      _ => {}
    }

    let integer = input.expect_integer()?;
    Ok(CSSInteger(integer))
  }
}

impl ToCss for CSSInteger {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    // Handle infinity values
    if self.0 == i32::MAX {
      return dest.write_str("calc(1/0)");
    }
    if self.0 == i32::MIN {
      return dest.write_str("calc(-1/0)");
    }
    cssparser::ToCss::to_css(&self.0, dest)?;
    Ok(())
  }
}

impl std::ops::Mul<f32> for CSSInteger {
  type Output = Self;
  fn mul(self, other: f32) -> Self {
    let result = (self.0 as f32 * other).round();
    // Check for overflow and produce infinity
    if result > i32::MAX as f32 {
      CSSInteger(i32::MAX)
    } else if result < i32::MIN as f32 {
      CSSInteger(i32::MIN)
    } else {
      CSSInteger(result as i32)
    }
  }
}

impl AddInternal for CSSInteger {
  fn add(self, other: Self) -> Self {
    let result = self.0.saturating_add(other.0);
    // Check for overflow and produce infinity
    if result == i32::MAX {
      CSSInteger(i32::MAX)
    } else if result == i32::MIN {
      CSSInteger(i32::MIN)
    } else {
      CSSInteger(result)
    }
  }
}

impl Op for CSSInteger {
  fn op<F: FnOnce(f32, f32) -> f32>(&self, to: &Self, op: F) -> Self {
    let result = op(self.0 as f32, to.0 as f32);
    // Check for overflow and produce infinity
    if result > i32::MAX as f32 {
      CSSInteger(i32::MAX)
    } else if result < i32::MIN as f32 {
      CSSInteger(i32::MIN)
    } else {
      CSSInteger(result.round() as i32)
    }
  }

  fn op_to<T, F: FnOnce(f32, f32) -> T>(&self, rhs: &Self, op: F) -> T {
    op(self.0 as f32, rhs.0 as f32)
  }
}

impl Map for CSSInteger {
  fn map<F: FnOnce(f32) -> f32>(&self, op: F) -> Self {
    let result = op(self.0 as f32);
    // Check for overflow and produce infinity
    if result > i32::MAX as f32 {
      CSSInteger(i32::MAX)
    } else if result < i32::MIN as f32 {
      CSSInteger(i32::MIN)
    } else {
      CSSInteger(result as i32)
    }
  }
}

impl Sign for CSSInteger {
  fn sign(&self) -> f32 {
    if self.0 == 0 {
      return if self.0.is_positive() { 0.0 } else { -0.0 };
    }
    self.0.signum() as f32
  }
}

impl std::convert::Into<Calc<CSSInteger>> for CSSInteger {
  fn into(self) -> Calc<CSSInteger> {
    Calc::Value(Box::new(self))
  }
}

impl std::convert::From<Calc<CSSInteger>> for CSSInteger {
  fn from(calc: Calc<CSSInteger>) -> Self {
    match calc {
      Calc::Value(v) => *v,
      Calc::Number(n) => {
        if n.is_infinite() {
          if n.is_sign_negative() {
            CSSInteger(i32::MIN)
          } else {
            CSSInteger(i32::MAX)
          }
        } else if n.is_nan() {
          CSSInteger(0)
        } else {
          CSSInteger(n.round() as i32)
        }
      }
      _ => unreachable!(),
    }
  }
}

impl Zero for CSSInteger {
  fn zero() -> Self {
    CSSInteger(0)
  }

  fn is_zero(&self) -> bool {
    self.0 == 0
  }
}

impl_try_from_angle!(CSSInteger);
