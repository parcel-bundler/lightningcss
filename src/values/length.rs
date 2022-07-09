//! CSS length values.

use super::angle::impl_try_from_angle;
use super::calc::{Calc, MathFunction, Round, RoundingStrategy, TryRem, TryRound};
use super::number::CSSNumber;
use super::percentage::DimensionPercentage;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{
  private::{AddInternal, TryAdd},
  Parse, ToCss,
};
use const_str;
use cssparser::*;

/// A CSS [`<length-percentage>`](https://www.w3.org/TR/css-values-4/#typedef-length-percentage) value.
/// May be specified as either a length or a percentage that resolves to an length.
pub type LengthPercentage = DimensionPercentage<LengthValue>;

impl LengthPercentage {
  /// Constructs a value of zero pixels.
  pub fn zero() -> LengthPercentage {
    LengthPercentage::px(0.0)
  }

  /// Constructs a `LengthPercentage` with the given pixel value.
  pub fn px(val: CSSNumber) -> LengthPercentage {
    LengthPercentage::Dimension(LengthValue::Px(val))
  }

  pub(crate) fn to_css_unitless<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      DimensionPercentage::Dimension(d) => d.to_css_unitless(dest),
      _ => self.to_css(dest),
    }
  }
}

/// Either a [`<length-percentage>`](https://www.w3.org/TR/css-values-4/#typedef-length-percentage), or the `auto` keyword.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum LengthPercentageOrAuto {
  /// The `auto` keyword.
  Auto,
  /// A [`<length-percentage>`](https://www.w3.org/TR/css-values-4/#typedef-length-percentage).
  LengthPercentage(LengthPercentage),
}

impl<'i> Parse<'i> for LengthPercentageOrAuto {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("auto")).is_ok() {
      return Ok(LengthPercentageOrAuto::Auto);
    }

    if let Ok(percent) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(LengthPercentageOrAuto::LengthPercentage(percent));
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for LengthPercentageOrAuto {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use LengthPercentageOrAuto::*;
    match self {
      Auto => dest.write_str("auto"),
      LengthPercentage(l) => l.to_css(dest),
    }
  }
}

const PX_PER_IN: f32 = 96.0;
const PX_PER_CM: f32 = PX_PER_IN / 2.54;
const PX_PER_MM: f32 = PX_PER_CM / 10.0;
const PX_PER_Q: f32 = PX_PER_CM / 40.0;
const PX_PER_PT: f32 = PX_PER_IN / 72.0;
const PX_PER_PC: f32 = PX_PER_IN / 6.0;

macro_rules! define_length_units {
  (
    $(
      $(#[$meta: meta])*
      $name: ident,
    )+
  ) => {
    /// A CSS [`<length>`](https://www.w3.org/TR/css-values-4/#lengths) value,
    /// without support for `calc()`. See also: [Length](Length).
    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(tag = "unit", content = "value", rename_all = "kebab-case"))]
    pub enum LengthValue {
      $(
        $(#[$meta])*
        $name(CSSNumber),
      )+
    }

    impl<'i> Parse<'i> for LengthValue {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let location = input.current_source_location();
        let token = input.next()?;
        match *token {
          Token::Dimension { value, ref unit, .. } => {
            Ok(match unit {
              $(
                s if s.eq_ignore_ascii_case(stringify!($name)) => LengthValue::$name(value),
              )+
              _ => return Err(location.new_unexpected_token_error(token.clone())),
            })
          },
          Token::Number { value, .. } => {
            // TODO: quirks mode only?
            Ok(LengthValue::Px(value))
          }
          ref token => return Err(location.new_unexpected_token_error(token.clone())),
        }
      }
    }

    impl LengthValue {
      /// Returns the numeric value and unit string for the length value.
      pub fn to_unit_value(&self) -> (CSSNumber, &str) {
        match self {
          $(
            LengthValue::$name(value) => (*value, const_str::convert_ascii_case!(lower, stringify!($name))),
          )+
        }
      }
    }

    impl TryAdd<LengthValue> for LengthValue {
      fn try_add(&self, other: &LengthValue) -> Option<LengthValue> {
        use LengthValue::*;
        match (self, other) {
          $(
            ($name(a), $name(b)) => Some($name(a + b)),
          )+
          (a, b) => {
            if let (Some(a), Some(b)) = (a.to_px(), b.to_px()) {
              Some(Px(a + b))
            } else {
              None
            }
          }
        }
      }
    }

    impl std::ops::Mul<CSSNumber> for LengthValue {
      type Output = Self;

      fn mul(self, other: CSSNumber) -> LengthValue {
        use LengthValue::*;
        match self {
          $(
            $name(value) => $name(value * other),
          )+
        }
      }
    }

    impl std::cmp::PartialEq<CSSNumber> for LengthValue {
      fn eq(&self, other: &CSSNumber) -> bool {
        use LengthValue::*;
        match self {
          $(
            $name(value) => value == other,
          )+
        }
      }
    }

    impl std::cmp::PartialOrd<CSSNumber> for LengthValue {
      fn partial_cmp(&self, other: &CSSNumber) -> Option<std::cmp::Ordering> {
        use LengthValue::*;
        match self {
          $(
            $name(value) => value.partial_cmp(other),
          )+
        }
      }
    }

    impl std::cmp::PartialOrd<LengthValue> for LengthValue {
      fn partial_cmp(&self, other: &LengthValue) -> Option<std::cmp::Ordering> {
        use LengthValue::*;
        match (self, other) {
          $(
            ($name(a), $name(b)) => a.partial_cmp(b),
          )+
          (a, b) => {
            if let (Some(a), Some(b)) = (a.to_px(), b.to_px()) {
              a.partial_cmp(&b)
            } else {
              None
            }
          }
        }
      }
    }

    impl TryRound for LengthValue {
      fn try_round(&self, to: &Self, strategy: RoundingStrategy) -> Option<Self> {
        use LengthValue::*;
        match (self, to) {
          $(
            ($name(a), $name(b)) => Some($name(Round::round(a, b, strategy))),
          )+
          (a, b) => {
            if let (Some(a), Some(b)) = (a.to_px(), b.to_px()) {
              Some(Px(Round::round(&a, &b, strategy)))
            } else {
              None
            }
          }
        }
      }
    }

    impl TryRem for LengthValue {
      fn try_rem(&self, rhs: &Self) -> Option<Self> {
        use LengthValue::*;
        match (self, rhs) {
          $(
            ($name(a), $name(b)) => Some($name(a % b)),
          )+
          (a, b) => {
            if let (Some(a), Some(b)) = (a.to_px(), b.to_px()) {
              Some(Px(a % b))
            } else {
              None
            }
          }
        }
      }
    }

    impl_try_from_angle!(LengthValue);
  };
}

define_length_units! {
  // https://www.w3.org/TR/css-values-4/#absolute-lengths
  /// A length in pixels.
  Px,
  /// A length in inches. 1in = 96px.
  In,
  /// A length in centimeters. 1cm = 96px / 2.54.
  Cm,
  /// A length in millimeters. 1mm = 1/10th of 1cm.
  Mm,
  /// A length in quarter-millimeters. 1Q = 1/40th of 1cm.
  Q,
  /// A length in points. 1pt = 1/72nd of 1in.
  Pt,
  /// A length in picas. 1pc = 1/6th of 1in.
  Pc,

  // https://www.w3.org/TR/css-values-4/#font-relative-lengths
  /// A length in the `em` unit. An `em` is equal to the computed value of the
  /// font-size property of the element on which it is used.
  Em,
  /// A length in the `rem` unit. A `rem` is equal to the computed value of the
  /// `em` unit on the root element.
  Rem,
  /// A length in `ex` unit. An `ex` is equal to the x-height of the font.
  Ex,
  /// A length in the `rex` unit. A `rex` is equal to the value of the `ex` unit on the root element.
  Rex,
  /// A length in the `ch` unit. A `ch` is equal to the width of the zero ("0") character in the current font.
  Ch,
  /// A length in the `rch` unit. An `rch` is equal to the value of the `ch` unit on the root element.
  Rch,
  /// A length in the `cap` unit. A `cap` is equal to the cap-height of the font.
  Cap,
  /// A length in the `rcap` unit. An `rcap` is equal to the value of the `cap` unit on the root element.
  Rcap,
  /// A length in the `ic` unit. An `ic` is equal to the width of the “水” (CJK water ideograph) character in the current font.
  Ic,
  /// A length in the `ric` unit. An `ric` is equal to the value of the `ic` unit on the root element.
  Ric,
  /// A length in the `lh` unit. An `lh` is equal to the computed value of the `line-height` property.
  Lh,
  /// A length in the `rlh` unit. An `rlh` is equal to the value of the `lh` unit on the root element.
  Rlh,

  // https://www.w3.org/TR/css-values-4/#viewport-relative-units
  /// A length in the `vw` unit. A `vw` is equal to 1% of the [viewport width](https://www.w3.org/TR/css-values-4/#ua-default-viewport-size).
  Vw,
  /// A length in the `lvw` unit. An `lvw` is equal to 1% of the [large viewport width](https://www.w3.org/TR/css-values-4/#large-viewport-size).
  Lvw,
  /// A length in the `svw` unit. An `svw` is equal to 1% of the [small viewport width](https://www.w3.org/TR/css-values-4/#small-viewport-size).
  Svw,
  /// A length in the `dvw` unit. An `dvw` is equal to 1% of the [dynamic viewport width](https://www.w3.org/TR/css-values-4/#dynamic-viewport-size).
  Dvw,

  /// A length in the `vh` unit. A `vh` is equal to 1% of the [viewport height](https://www.w3.org/TR/css-values-4/#ua-default-viewport-size).
  Vh,
  /// A length in the `lvh` unit. An `lvh` is equal to 1% of the [large viewport height](https://www.w3.org/TR/css-values-4/#large-viewport-size).
  Lvh,
  /// A length in the `svh` unit. An `svh` is equal to 1% of the [small viewport height](https://www.w3.org/TR/css-values-4/#small-viewport-size).
  Svh,
  /// A length in the `dvh` unit. An `dvh` is equal to 1% of the [dynamic viewport height](https://www.w3.org/TR/css-values-4/#dynamic-viewport-size).
  Dvh,

  /// A length in the `vi` unit. A `vi` is equal to 1% of the [viewport size](https://www.w3.org/TR/css-values-4/#ua-default-viewport-size)
  /// in the box's [inline axis](https://www.w3.org/TR/css-writing-modes-4/#inline-axis).
  Vi,
  /// A length in the `svi` unit. A `svi` is equal to 1% of the [small viewport size](https://www.w3.org/TR/css-values-4/#small-viewport-size)
  /// in the box's [inline axis](https://www.w3.org/TR/css-writing-modes-4/#inline-axis).
  Svi,
  /// A length in the `lvi` unit. A `lvi` is equal to 1% of the [large viewport size](https://www.w3.org/TR/css-values-4/#large-viewport-size)
  /// in the box's [inline axis](https://www.w3.org/TR/css-writing-modes-4/#inline-axis).
  Lvi,
  /// A length in the `dvi` unit. A `dvi` is equal to 1% of the [dynamic viewport size](https://www.w3.org/TR/css-values-4/#dynamic-viewport-size)
  /// in the box's [inline axis](https://www.w3.org/TR/css-writing-modes-4/#inline-axis).
  Dvi,

  /// A length in the `vb` unit. A `vb` is equal to 1% of the [viewport size](https://www.w3.org/TR/css-values-4/#ua-default-viewport-size)
  /// in the box's [block axis](https://www.w3.org/TR/css-writing-modes-4/#block-axis).
  Vb,
  /// A length in the `svb` unit. A `svb` is equal to 1% of the [small viewport size](https://www.w3.org/TR/css-values-4/#small-viewport-size)
  /// in the box's [block axis](https://www.w3.org/TR/css-writing-modes-4/#block-axis).
  Svb,
  /// A length in the `lvb` unit. A `lvb` is equal to 1% of the [large viewport size](https://www.w3.org/TR/css-values-4/#large-viewport-size)
  /// in the box's [block axis](https://www.w3.org/TR/css-writing-modes-4/#block-axis).
  Lvb,
  /// A length in the `dvb` unit. A `dvb` is equal to 1% of the [dynamic viewport size](https://www.w3.org/TR/css-values-4/#dynamic-viewport-size)
  /// in the box's [block axis](https://www.w3.org/TR/css-writing-modes-4/#block-axis).
  Dvb,

  /// A length in the `vmin` unit. A `vmin` is equal to the smaller of `vw` and `vh`.
  Vmin,
  /// A length in the `svmin` unit. An `svmin` is equal to the smaller of `svw` and `svh`.
  Svmin,
  /// A length in the `lvmin` unit. An `lvmin` is equal to the smaller of `lvw` and `lvh`.
  Lvmin,
  /// A length in the `dvmin` unit. A `dvmin` is equal to the smaller of `dvw` and `dvh`.
  Dvmin,

  /// A length in the `vmax` unit. A `vmax` is equal to the larger of `vw` and `vh`.
  Vmax,
  /// A length in the `svmax` unit. An `svmax` is equal to the larger of `svw` and `svh`.
  Svmax,
  /// A length in the `lvmax` unit. An `lvmax` is equal to the larger of `lvw` and `lvh`.
  Lvmax,
  /// A length in the `dvmax` unit. An `dvmax` is equal to the larger of `dvw` and `dvh`.
  Dvmax,
}

impl ToCss for LengthValue {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let (value, unit) = self.to_unit_value();

    // The unit can be omitted if the value is zero, except inside calc()
    // expressions, where unitless numbers won't be parsed as dimensions.
    if !dest.in_calc && value == 0.0 {
      return dest.write_char('0');
    }

    serialize_dimension(value, unit, dest)
  }
}

impl LengthValue {
  pub(crate) fn to_css_unitless<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      LengthValue::Px(value) => value.to_css(dest),
      _ => self.to_css(dest),
    }
  }
}

pub(crate) fn serialize_dimension<W>(value: f32, unit: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  use cssparser::ToCss;
  let int_value = if value.fract() == 0.0 { Some(value as i32) } else { None };
  let token = Token::Dimension {
    has_sign: value < 0.0,
    value,
    int_value,
    unit: CowRcStr::from(unit),
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
    token.to_css(dest)?;
    Ok(())
  }
}

impl LengthValue {
  /// Attempts to convert the value to pixels.
  /// Returns `None` if the conversion is not possible.
  pub fn to_px(&self) -> Option<CSSNumber> {
    use LengthValue::*;
    match self {
      Px(value) => Some(*value),
      In(value) => Some(value * PX_PER_IN),
      Cm(value) => Some(value * PX_PER_CM),
      Mm(value) => Some(value * PX_PER_MM),
      Q(value) => Some(value * PX_PER_Q),
      Pt(value) => Some(value * PX_PER_PT),
      Pc(value) => Some(value * PX_PER_PC),
      _ => None,
    }
  }
}

/// A CSS [`<length>`](https://www.w3.org/TR/css-values-4/#lengths) value, with support for `calc()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Length {
  /// An explicitly specified length value.
  Value(LengthValue),
  /// A computed length value using `calc()`.
  Calc(Box<Calc<Length>>),
}

impl<'i> Parse<'i> for Length {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(Calc::parse) {
      Ok(Calc::Value(v)) => return Ok(*v),
      Ok(calc) => return Ok(Length::Calc(Box::new(calc))),
      _ => {}
    }

    let len = LengthValue::parse(input)?;
    Ok(Length::Value(len))
  }
}

impl ToCss for Length {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Length::Value(a) => a.to_css(dest),
      Length::Calc(c) => c.to_css(dest),
    }
  }
}

impl std::ops::Mul<CSSNumber> for Length {
  type Output = Self;

  fn mul(self, other: CSSNumber) -> Length {
    match self {
      Length::Value(a) => Length::Value(a * other),
      Length::Calc(a) => Length::Calc(Box::new(*a * other)),
    }
  }
}

impl std::ops::Add<Length> for Length {
  type Output = Self;

  fn add(self, other: Length) -> Length {
    // Unwrap calc(...) functions so we can add inside.
    // Then wrap the result in a calc(...) again if necessary.
    let a = unwrap_calc(self);
    let b = unwrap_calc(other);
    let res = AddInternal::add(a, b);
    match res {
      Length::Calc(c) => match *c {
        Calc::Value(l) => *l,
        Calc::Function(f) if !matches!(*f, MathFunction::Calc(_)) => Length::Calc(Box::new(Calc::Function(f))),
        c => Length::Calc(Box::new(Calc::Function(Box::new(MathFunction::Calc(c))))),
      },
      _ => res,
    }
  }
}

fn unwrap_calc(length: Length) -> Length {
  match length {
    Length::Calc(c) => match *c {
      Calc::Function(f) => match *f {
        MathFunction::Calc(c) => Length::Calc(Box::new(c)),
        c => Length::Calc(Box::new(Calc::Function(Box::new(c)))),
      },
      _ => Length::Calc(c),
    },
    _ => length,
  }
}

impl AddInternal for Length {
  fn add(self, other: Self) -> Self {
    match self.try_add(&other) {
      Some(r) => r,
      None => self.add(other),
    }
  }
}

impl Length {
  /// Constructs a zero length value.
  pub fn zero() -> Length {
    Length::Value(LengthValue::Px(0.0))
  }

  /// Constructs a length with the given pixel value.
  pub fn px(px: CSSNumber) -> Length {
    Length::Value(LengthValue::Px(px))
  }

  /// Attempts to convert the length to pixels.
  /// Returns `None` if the conversion is not possible.
  pub fn to_px(&self) -> Option<CSSNumber> {
    match self {
      Length::Value(a) => a.to_px(),
      _ => None,
    }
  }

  fn add(self, other: Length) -> Length {
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
      (Length::Calc(a), Length::Calc(b)) => return Length::Calc(Box::new(a.add(*b))),
      (Length::Calc(calc), b) => {
        if let Calc::Value(a) = *calc {
          a.add(b)
        } else {
          Length::Calc(Box::new(Calc::Sum(Box::new((*calc).into()), Box::new(b.into()))))
        }
      }
      (a, Length::Calc(calc)) => {
        if let Calc::Value(b) = *calc {
          a.add(*b)
        } else {
          Length::Calc(Box::new(Calc::Sum(Box::new(a.into()), Box::new((*calc).into()))))
        }
      }
      (a, b) => Length::Calc(Box::new(Calc::Sum(Box::new(a.into()), Box::new(b.into())))),
    }
  }
}

impl TryAdd<Length> for Length {
  fn try_add(&self, other: &Length) -> Option<Length> {
    match (self, other) {
      (Length::Value(a), Length::Value(b)) => {
        if let Some(res) = a.try_add(b) {
          Some(Length::Value(res))
        } else {
          None
        }
      }
      (Length::Calc(a), other) => match &**a {
        Calc::Value(v) => v.try_add(other),
        Calc::Sum(a, b) => {
          if let Some(res) = Length::Calc(Box::new(*a.clone())).try_add(other) {
            return Some(res.add(Length::from(*b.clone())));
          }

          if let Some(res) = Length::Calc(Box::new(*b.clone())).try_add(other) {
            return Some(Length::from(*a.clone()).add(res));
          }

          None
        }
        _ => None,
      },
      (other, Length::Calc(b)) => match &**b {
        Calc::Value(v) => other.try_add(&*v),
        Calc::Sum(a, b) => {
          if let Some(res) = other.try_add(&Length::Calc(Box::new(*a.clone()))) {
            return Some(res.add(Length::from(*b.clone())));
          }

          if let Some(res) = other.try_add(&Length::Calc(Box::new(*b.clone()))) {
            return Some(Length::from(*a.clone()).add(res));
          }

          None
        }
        _ => None,
      },
    }
  }
}

impl std::convert::Into<Calc<Length>> for Length {
  fn into(self) -> Calc<Length> {
    match self {
      Length::Calc(c) => *c,
      b => Calc::Value(Box::new(b)),
    }
  }
}

impl std::convert::From<Calc<Length>> for Length {
  fn from(calc: Calc<Length>) -> Length {
    Length::Calc(Box::new(calc))
  }
}

impl std::cmp::PartialEq<CSSNumber> for Length {
  fn eq(&self, other: &CSSNumber) -> bool {
    match self {
      Length::Value(a) => *a == *other,
      Length::Calc(_) => false,
    }
  }
}

impl std::cmp::PartialOrd<CSSNumber> for Length {
  fn partial_cmp(&self, other: &CSSNumber) -> Option<std::cmp::Ordering> {
    match self {
      Length::Value(a) => a.partial_cmp(other),
      Length::Calc(_) => None,
    }
  }
}

impl std::cmp::PartialOrd<Length> for Length {
  fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
    match (self, other) {
      (Length::Value(a), Length::Value(b)) => a.partial_cmp(b),
      _ => None,
    }
  }
}

impl TryRound for Length {
  fn try_round(&self, to: &Self, strategy: RoundingStrategy) -> Option<Self> {
    match (self, to) {
      (Length::Value(a), Length::Value(b)) => a.try_round(b, strategy).map(Length::Value),
      _ => None,
    }
  }
}

impl TryRem for Length {
  fn try_rem(&self, rhs: &Self) -> Option<Self> {
    match (self, rhs) {
      (Length::Value(a), Length::Value(b)) => a.try_rem(b).map(Length::Value),
      _ => None,
    }
  }
}

impl_try_from_angle!(Length);

/// Either a [`<length>`](https://www.w3.org/TR/css-values-4/#lengths) or a [`<number>`](https://www.w3.org/TR/css-values-4/#numbers).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum LengthOrNumber {
  /// A length.
  Length(Length),
  /// A number.
  Number(CSSNumber),
}

impl Default for LengthOrNumber {
  fn default() -> LengthOrNumber {
    LengthOrNumber::Number(0.0)
  }
}

impl<'i> Parse<'i> for LengthOrNumber {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Parse number first so unitless numbers are not parsed as lengths.
    if let Ok(number) = input.try_parse(CSSNumber::parse) {
      return Ok(LengthOrNumber::Number(number));
    }

    if let Ok(length) = Length::parse(input) {
      return Ok(LengthOrNumber::Length(length));
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for LengthOrNumber {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      LengthOrNumber::Length(length) => length.to_css(dest),
      LengthOrNumber::Number(number) => number.to_css(dest),
    }
  }
}
