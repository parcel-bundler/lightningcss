//! CSS percentage values.

use super::calc::Calc;
use super::number::CSSNumber;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{private::TryAdd, Parse, ToCss};
use cssparser::*;

/// A CSS [`<percentage>`](https://www.w3.org/TR/css-values-4/#percentages) value.
///
/// Percentages may be explicit or computed by `calc()`, but are always stored and serialized
/// as their computed value.
#[derive(Debug, Clone, PartialEq)]
pub struct Percentage(pub CSSNumber);

impl<'i> Parse<'i> for Percentage {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use cssparser::ToCss;
    let int_value = if (self.0 * 100.0).fract() == 0.0 {
      Some(self.0 as i32)
    } else {
      None
    };
    let percent = Token::Percentage {
      has_sign: self.0 < 0.0,
      unit_value: self.0,
      int_value,
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
      percent.to_css(dest)?;
      Ok(())
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
      _ => unreachable!(),
    }
  }
}

impl std::ops::Mul<CSSNumber> for Percentage {
  type Output = Self;

  fn mul(self, other: CSSNumber) -> Percentage {
    Percentage(self.0 * other)
  }
}

impl std::ops::Add<Percentage> for Percentage {
  type Output = Self;

  fn add(self, other: Percentage) -> Percentage {
    Percentage(self.0 + other.0)
  }
}

impl std::cmp::PartialEq<CSSNumber> for Percentage {
  fn eq(&self, other: &CSSNumber) -> bool {
    self.0 == *other
  }
}

impl std::cmp::PartialOrd<CSSNumber> for Percentage {
  fn partial_cmp(&self, other: &CSSNumber) -> Option<std::cmp::Ordering> {
    self.0.partial_cmp(other)
  }
}

impl std::cmp::PartialOrd<Percentage> for Percentage {
  fn partial_cmp(&self, other: &Percentage) -> Option<std::cmp::Ordering> {
    self.0.partial_cmp(&other.0)
  }
}

/// Either a `<number>` or `<percentage>`.
#[derive(Debug, Clone, PartialEq)]
pub enum NumberOrPercentage {
  /// A percentage.
  Percentage(Percentage),
  /// A number.
  Number(CSSNumber),
}

impl<'i> Parse<'i> for NumberOrPercentage {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(number) = input.try_parse(CSSNumber::parse) {
      return Ok(NumberOrPercentage::Number(number));
    }

    if let Ok(percent) = input.try_parse(|input| Percentage::parse(input)) {
      return Ok(NumberOrPercentage::Percentage(percent));
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for NumberOrPercentage {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      NumberOrPercentage::Percentage(percent) => percent.to_css(dest),
      NumberOrPercentage::Number(number) => number.to_css(dest),
    }
  }
}

impl std::cmp::PartialEq<CSSNumber> for NumberOrPercentage {
  fn eq(&self, other: &CSSNumber) -> bool {
    match self {
      NumberOrPercentage::Number(a) => *a == *other,
      NumberOrPercentage::Percentage(a) => a.0 == *other,
    }
  }
}

impl std::convert::Into<CSSNumber> for &NumberOrPercentage {
  fn into(self) -> CSSNumber {
    match self {
      NumberOrPercentage::Number(a) => *a,
      NumberOrPercentage::Percentage(a) => a.0,
    }
  }
}

/// A generic type that allows any kind of dimension and percentage to be
/// used standalone or mixed within a `calc()` expression.
///
/// <https://drafts.csswg.org/css-values-4/#mixed-percentages>
#[derive(Debug, Clone, PartialEq)]
pub enum DimensionPercentage<D> {
  /// An explicit dimension value.
  Dimension(D),
  /// A percentage.
  Percentage(Percentage),
  /// A `calc()` expression.
  Calc(Box<Calc<DimensionPercentage<D>>>),
}

impl<
    'i,
    D: Parse<'i>
      + std::ops::Mul<CSSNumber, Output = D>
      + TryAdd<D>
      + Clone
      + std::cmp::PartialEq<CSSNumber>
      + std::cmp::PartialOrd<CSSNumber>
      + std::cmp::PartialOrd<D>
      + std::fmt::Debug,
  > Parse<'i> for DimensionPercentage<D>
{
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      Ok(calc) => return Ok(DimensionPercentage::Calc(Box::new(calc))),
      _ => {}
    }

    if let Ok(length) = input.try_parse(|input| D::parse(input)) {
      return Ok(DimensionPercentage::Dimension(length));
    }

    if let Ok(percent) = input.try_parse(|input| Percentage::parse(input)) {
      return Ok(DimensionPercentage::Percentage(percent));
    }

    Err(input.new_error_for_next_token())
  }
}

impl<D: std::ops::Mul<CSSNumber, Output = D>> std::ops::Mul<CSSNumber> for DimensionPercentage<D> {
  type Output = Self;

  fn mul(self, other: CSSNumber) -> DimensionPercentage<D> {
    match self {
      DimensionPercentage::Dimension(l) => DimensionPercentage::Dimension(l * other),
      DimensionPercentage::Percentage(p) => DimensionPercentage::Percentage(Percentage(p.0 * other)),
      DimensionPercentage::Calc(c) => DimensionPercentage::Calc(Box::new(*c * other)),
    }
  }
}

impl<D: TryAdd<D> + Clone + std::cmp::PartialEq<CSSNumber> + std::cmp::PartialOrd<CSSNumber> + std::fmt::Debug>
  std::ops::Add<DimensionPercentage<D>> for DimensionPercentage<D>
{
  type Output = Self;

  fn add(self, other: DimensionPercentage<D>) -> DimensionPercentage<D> {
    match self.add_recursive(&other) {
      Some(r) => r,
      None => self.add(other),
    }
  }
}

impl<D: TryAdd<D> + Clone + std::cmp::PartialEq<CSSNumber> + std::cmp::PartialOrd<CSSNumber> + std::fmt::Debug>
  DimensionPercentage<D>
{
  fn add_recursive(&self, other: &DimensionPercentage<D>) -> Option<DimensionPercentage<D>> {
    match (self, other) {
      (DimensionPercentage::Dimension(a), DimensionPercentage::Dimension(b)) => {
        if let Some(res) = a.try_add(b) {
          Some(DimensionPercentage::Dimension(res))
        } else {
          None
        }
      }
      (DimensionPercentage::Percentage(a), DimensionPercentage::Percentage(b)) => {
        Some(DimensionPercentage::Percentage(Percentage(a.0 + b.0)))
      }
      (DimensionPercentage::Calc(a), other) => match &**a {
        Calc::Value(v) => v.add_recursive(other),
        Calc::Sum(a, b) => {
          if let Some(res) = DimensionPercentage::Calc(Box::new(*a.clone())).add_recursive(other) {
            return Some(res.add(DimensionPercentage::from(*b.clone())));
          }

          if let Some(res) = DimensionPercentage::Calc(Box::new(*b.clone())).add_recursive(other) {
            return Some(DimensionPercentage::from(*a.clone()).add(res));
          }

          None
        }
        _ => None,
      },
      (other, DimensionPercentage::Calc(b)) => match &**b {
        Calc::Value(v) => other.add_recursive(&*v),
        Calc::Sum(a, b) => {
          if let Some(res) = other.add_recursive(&DimensionPercentage::Calc(Box::new(*a.clone()))) {
            return Some(res.add(DimensionPercentage::from(*b.clone())));
          }

          if let Some(res) = other.add_recursive(&DimensionPercentage::Calc(Box::new(*b.clone()))) {
            return Some(DimensionPercentage::from(*a.clone()).add(res));
          }

          None
        }
        _ => None,
      },
      _ => None,
    }
  }

  fn add(self, other: DimensionPercentage<D>) -> DimensionPercentage<D> {
    let mut a = self;
    let mut b = other;

    if a == 0.0 {
      return b;
    }

    if b == 0.0 {
      return a;
    }

    if a < 0.0 && b > 0.0 {
      std::mem::swap(&mut a, &mut b);
    }

    match (a, b) {
      (DimensionPercentage::Calc(a), DimensionPercentage::Calc(b)) => DimensionPercentage::Calc(Box::new(*a + *b)),
      (DimensionPercentage::Calc(calc), b) => {
        if let Calc::Value(a) = *calc {
          a.add(b)
        } else {
          DimensionPercentage::Calc(Box::new(Calc::Sum(Box::new((*calc).into()), Box::new(b.into()))))
        }
      }
      (a, DimensionPercentage::Calc(calc)) => {
        if let Calc::Value(b) = *calc {
          a.add(*b)
        } else {
          DimensionPercentage::Calc(Box::new(Calc::Sum(Box::new(a.into()), Box::new((*calc).into()))))
        }
      }
      (a, b) => DimensionPercentage::Calc(Box::new(Calc::Sum(Box::new(a.into()), Box::new(b.into())))),
    }
  }
}

impl<D> std::convert::Into<Calc<DimensionPercentage<D>>> for DimensionPercentage<D> {
  fn into(self) -> Calc<DimensionPercentage<D>> {
    match self {
      DimensionPercentage::Calc(c) => *c,
      b => Calc::Value(Box::new(b)),
    }
  }
}

impl<D> std::convert::From<Calc<DimensionPercentage<D>>> for DimensionPercentage<D> {
  fn from(calc: Calc<DimensionPercentage<D>>) -> DimensionPercentage<D> {
    DimensionPercentage::Calc(Box::new(calc))
  }
}

impl<D: std::cmp::PartialEq<CSSNumber>> std::cmp::PartialEq<CSSNumber> for DimensionPercentage<D> {
  fn eq(&self, other: &CSSNumber) -> bool {
    match self {
      DimensionPercentage::Dimension(a) => *a == *other,
      DimensionPercentage::Percentage(a) => *a == *other,
      DimensionPercentage::Calc(_) => false,
    }
  }
}

impl<D: std::cmp::PartialOrd<CSSNumber>> std::cmp::PartialOrd<CSSNumber> for DimensionPercentage<D> {
  fn partial_cmp(&self, other: &CSSNumber) -> Option<std::cmp::Ordering> {
    match self {
      DimensionPercentage::Dimension(a) => a.partial_cmp(other),
      DimensionPercentage::Percentage(a) => a.partial_cmp(other),
      DimensionPercentage::Calc(_) => None,
    }
  }
}

impl<D: std::cmp::PartialOrd<D>> std::cmp::PartialOrd<DimensionPercentage<D>> for DimensionPercentage<D> {
  fn partial_cmp(&self, other: &DimensionPercentage<D>) -> Option<std::cmp::Ordering> {
    match (self, other) {
      (DimensionPercentage::Dimension(a), DimensionPercentage::Dimension(b)) => a.partial_cmp(b),
      (DimensionPercentage::Percentage(a), DimensionPercentage::Percentage(b)) => a.partial_cmp(b),
      _ => None,
    }
  }
}

impl<
    D: ToCss + std::cmp::PartialOrd<CSSNumber> + std::ops::Mul<CSSNumber, Output = D> + Clone + std::fmt::Debug,
  > ToCss for DimensionPercentage<D>
{
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      DimensionPercentage::Dimension(length) => length.to_css(dest),
      DimensionPercentage::Percentage(percent) => percent.to_css(dest),
      DimensionPercentage::Calc(calc) => calc.to_css(dest),
    }
  }
}
