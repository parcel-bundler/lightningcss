//! CSS number values.

use super::angle::impl_try_from_angle;
use super::calc::Calc;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::private::AddInternal;
use crate::traits::{IsCompatible, Map, Op, Parse, ParseNumeric, Sign, ToCss, TrySign, Zero};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// Describes what values are allowed when parsing a numeric value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericRange {
  /// All values are allowed.
  All,
  /// Only values >= 0 are allowed.
  NonNegative,
}

impl NumericRange {
  /// Checks whether a value is allowed.
  pub fn check(
    &self,
    value: f32,
    location: SourceLocation,
  ) -> Result<f32, ParseError<'static, ParserError<'static>>> {
    match self {
      NumericRange::All => Ok(value),
      NumericRange::NonNegative if value >= 0.0 => Ok(value),
      _ => Err(location.new_custom_error(ParserError::InvalidValue)),
    }
  }

  /// Clamps the value to the valid range.
  pub fn clamp(&self, value: f32) -> f32 {
    match self {
      NumericRange::All => value,
      NumericRange::NonNegative => value.max(0.0),
    }
  }
}

/// A numeric value in a grammar position that only allows non-negative values.
///
/// Negative literals are rejected. Calculations are clamped when their result
/// can be resolved, or preserved for the browser to clamp after resolution.
/// This wrapper constrains parsing; its contents may still be modified directly.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct NonNegative<T>(pub T);

impl<'i, T: ParseNumeric<'i>> Parse<'i> for NonNegative<T> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    T::parse_with_range(input, NumericRange::NonNegative).map(Self)
  }
}

impl<T: ToCss> ToCss for NonNegative<T> {
  fn to_css<W: std::fmt::Write>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> {
    self.0.to_css(dest)
  }
}

impl<T: IsCompatible> IsCompatible for NonNegative<T> {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    self.0.is_compatible(browsers)
  }
}

impl<T: Zero> Zero for NonNegative<T> {
  fn zero() -> Self {
    Self(T::zero())
  }

  fn is_zero(&self) -> bool {
    self.0.is_zero()
  }
}

impl<T: TrySign> TrySign for NonNegative<T> {
  fn try_sign(&self) -> Option<f32> {
    self.0.try_sign()
  }
}

/// A CSS [`<number>`](https://www.w3.org/TR/css-values-4/#numbers) value.
///
/// Numbers may be explicit or computed by `calc()`, but are always stored and serialized
/// as their computed value.
pub type CSSNumber = f32;

impl<'i> Parse<'i> for CSSNumber {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Self::parse_with_range(input, NumericRange::All)
  }
}

impl<'i> ParseNumeric<'i> for CSSNumber {
  fn parse_with_range<'t>(
    input: &mut Parser<'i, 't>,
    range: NumericRange,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(range.clamp(*v)),
      Ok(Calc::Number(n)) => return Ok(range.clamp(n)),
      // Numbers are always compatible, so they will always compute to a value.
      Ok(_) => return Err(input.new_custom_error(ParserError::InvalidValue)),
      _ => {}
    }

    let location = input.current_source_location();
    let number = input.expect_number()?;
    range.check(number, location)
  }
}

impl ToCss for CSSNumber {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let number = *self;
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
pub type CSSInteger = i32;

impl<'i> Parse<'i> for CSSInteger {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Self::parse_with_range(input, NumericRange::All)
  }
}

impl<'i> ParseNumeric<'i> for CSSInteger {
  fn parse_with_range<'t>(
    input: &mut Parser<'i, 't>,
    range: NumericRange,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // TODO: calc??
    let location = input.current_source_location();
    let integer = input.expect_integer()?;
    range.check(integer as f32, location)?;
    Ok(integer)
  }
}

impl ToCss for CSSInteger {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    cssparser::ToCss::to_css(self, dest)?;
    Ok(())
  }
}

impl Zero for CSSInteger {
  fn zero() -> Self {
    0
  }

  fn is_zero(&self) -> bool {
    *self == 0
  }
}
