use cssparser::*;
use super::traits::{Parse, ToCss};
use super::macros::enum_property;
use crate::printer::Printer;
use std::fmt::Write;
use super::calc::Calc;

/// https://drafts.csswg.org/css-sizing-3/#specifying-sizes

/// https://drafts.csswg.org/css-sizing-3/#preferred-size-properties
#[derive(Debug, Clone, PartialEq)]
pub enum Size {
  Auto,
  LengthPercentage(LengthPercentage),
  MinContent,
  MaxContent,
  FitContent(LengthPercentage)
}

impl Parse for Size {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("auto")).is_ok() {
      return Ok(Size::Auto);
    }

    if input.try_parse(|i| i.expect_ident_matching("min-content")).is_ok() {
      return Ok(Size::MinContent);
    }

    if input.try_parse(|i| i.expect_ident_matching("max-content")).is_ok() {
      return Ok(Size::MaxContent);
    }

    if let Ok(l) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(Size::LengthPercentage(l))
    }

    if let Ok(l) = parse_fit_content(input) {
      return Ok(Size::FitContent(l))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for Size {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use Size::*;
    match self {
      Auto => dest.write_str("auto"),
      MinContent => dest.write_str("min-content"),
      MaxContent => dest.write_str("max-content"),
      FitContent(l) => {
        dest.write_str("fit-content(")?;
        l.to_css(dest)?;
        dest.write_str(")")
      }
      LengthPercentage(l) => l.to_css(dest),
      _ => Ok(())
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Size2D<T>(pub T, pub T);

impl<T> Parse for Size2D<T> where T: Parse + Clone {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let first = T::parse(input)?;
    let second = input.try_parse(T::parse).unwrap_or_else(|_| first.clone());
    Ok(Size2D(first, second))
  }
}

impl<T> ToCss for Size2D<T> where T: ToCss + PartialEq {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.0.to_css(dest)?;
    if self.1 != self.0 {
      dest.write_str(" ")?;
      self.1.to_css(dest)?;
    }
    Ok(())
  }
}

/// https://drafts.csswg.org/css-sizing-3/#min-size-properties
/// https://drafts.csswg.org/css-sizing-3/#max-size-properties
#[derive(Debug, Clone, PartialEq)]
pub enum MinMaxSize {
  None,
  LengthPercentage(LengthPercentage),
  MinContent,
  MaxContent,
  FitContent(LengthPercentage)
}

impl Parse for MinMaxSize {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(MinMaxSize::None);
    }

    if input.try_parse(|i| i.expect_ident_matching("min-content")).is_ok() {
      return Ok(MinMaxSize::MinContent);
    }

    if input.try_parse(|i| i.expect_ident_matching("max-content")).is_ok() {
      return Ok(MinMaxSize::MaxContent);
    }

    if let Ok(percent) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(MinMaxSize::LengthPercentage(percent))
    }

    if let Ok(l) = parse_fit_content(input) {
      return Ok(MinMaxSize::FitContent(l))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for MinMaxSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use MinMaxSize::*;
    match self {
      None => dest.write_str("none"),
      MinContent => dest.write_str("min-content"),
      MaxContent => dest.write_str("max-content"),
      FitContent(l) => {
        dest.write_str("fit-content(")?;
        l.to_css(dest)?;
        dest.write_str(")")
      }
      LengthPercentage(l) => l.to_css(dest),
      _ => Ok(())
    }
  }
}

/// https://drafts.csswg.org/css-values-4/#typedef-length-percentage
#[derive(Debug, Clone, PartialEq)]
pub enum LengthPercentage {
  Length(Length),
  Percentage(Percentage),
  Calc(Calc<LengthPercentage>)
}

impl Parse for LengthPercentage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(length) = input.try_parse(|input| Length::parse(input)) {
      return Ok(LengthPercentage::Length(length))
    }

    if let Ok(percent) = input.try_parse(|input| Percentage::parse(input)) {
      return Ok(LengthPercentage::Percentage(percent))
    }

    let f = input.expect_function()?;
    match_ignore_ascii_case! { &f,
      "calc" => {
        let calc = Calc::parse(input)?;
        return Ok(LengthPercentage::Calc(calc))
      },
      _ => {}
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
      (LengthPercentage::Percentage(a), LengthPercentage::Percentage(b)) => Some(LengthPercentage::Percentage(Percentage(a.0 + b.0))),
      (LengthPercentage::Calc(Calc::Value(v)), other) => v.add_recursive(other),
      (other, LengthPercentage::Calc(Calc::Value(v))) => other.add_recursive(v),
      (LengthPercentage::Calc(Calc::Sum(a, b)), other) => {
        if let Some(res) = LengthPercentage::Calc(*a.clone()).add_recursive(other) {
          return Some(LengthPercentage::Calc(Calc::Value(Box::new(res)) + *b.clone()))
        }

        if let Some(res) = LengthPercentage::Calc(*b.clone()).add_recursive(other) {
          return Some(LengthPercentage::Calc(*a.clone() + Calc::Value(Box::new(res))))
        }

        None
      }
      (other, LengthPercentage::Calc(Calc::Sum(a, b))) => {
        if let Some(res) = other.add_recursive(&LengthPercentage::Calc(*a.clone())) {
          return Some(LengthPercentage::Calc(Calc::Value(Box::new(res)) + *b.clone()))
        }

        if let Some(res) = other.add_recursive(&LengthPercentage::Calc(*b.clone())) {
          return Some(LengthPercentage::Calc(*a.clone() + Calc::Value(Box::new(res))))
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
      (LengthPercentage::Calc(a), b) => LengthPercentage::Calc(a + Calc::Value(Box::new(b))),
      (a, LengthPercentage::Calc(b)) => LengthPercentage::Calc(Calc::Value(Box::new(a)) + b),
      (a, b) => LengthPercentage::Calc(Calc::Sum(Box::new(Calc::Value(Box::new(a))), Box::new(Calc::Value(Box::new(b)))))
    }
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
      LengthPercentage::Calc(calc) => {
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
        dest.write_str(s.get(2..).unwrap_or(&s))
      } else {
        dest.write_str(s.get(1..).unwrap_or(&s))
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

/// https://drafts.csswg.org/css-values-4/#percentages
#[derive(Debug, Clone, PartialEq)]
pub struct Percentage(pub f32);

impl Parse for Percentage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let percent = input.expect_percentage()?;
    Ok(Percentage(percent))
  }
}

impl ToCss for Percentage {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use cssparser::ToCss;
    let int_value = if (self.0 * 100.0).fract() == 0.0 {
      Some(self.0 as i32)
    } else {
      None
    };
    let percent = Token::Percentage {
      has_sign: self.0 < 0.0,
      unit_value: self.0,
      int_value
    };
    if self.0 != 0.0 && self.0.abs() < 0.01 {
      let mut s = String::new();
      percent.to_css(&mut s)?;
      if self.0 < 0.0 {
        dest.write_char('-')?;
        dest.write_str(s.get(2..).unwrap_or(&s))
      } else {
        dest.write_str(s.get(1..).unwrap_or(&s))
      }
    } else {
      percent.to_css(dest)
    }
  }
}

fn parse_fit_content<'i, 't>(input: &mut Parser<'i, 't>) -> Result<LengthPercentage, ParseError<'i, ()>> {
  input.expect_function_matching("fit-content")?;
  input.parse_nested_block(|input| LengthPercentage::parse(input))
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

impl Parse for f32 {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(number) = input.try_parse(|input| input.expect_number()) {
      return Ok(number)
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for f32 {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    serialize_number(*self, dest)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberOrPercentage {
  Percentage(Percentage),
  Number(f32),
}

impl Parse for NumberOrPercentage {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(percent) = input.try_parse(|input| Percentage::parse(input)) {
      return Ok(NumberOrPercentage::Percentage(percent))
    }

    if let Ok(number) = input.try_parse(|input| input.expect_number()) {
      return Ok(NumberOrPercentage::Number(number))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for NumberOrPercentage {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      NumberOrPercentage::Percentage(percent) => percent.to_css(dest),
      NumberOrPercentage::Number(number) => serialize_number(*number, dest)
    }
  }
}

pub fn serialize_number<W>(number: f32, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
  use cssparser::ToCss;
  let int_value = if number.fract() == 0.0 {
    Some(number as i32)
  } else {
    None
  };
  let tok = Token::Number {
    has_sign: number < 0.0,
    value: number,
    int_value
  };
  if number != 0.0 && number.abs() < 1.0 {
    let mut s = String::new();
    tok.to_css(&mut s)?;
    dest.write_str(s.get(1..).unwrap_or(&s))
  } else {
    tok.to_css(dest)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Position<S> {
  /// `center`
  Center,
  /// `<length-percentage>`
  Length(LengthPercentage),
  /// `<side> <length-percentage>?`
  Side(S, Option<LengthPercentage>),
}

impl<S: Parse> Parse for Position<S> {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("center")).is_ok() {
      return Ok(Position::Center);
    }

    if let Ok(lp) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(Position::Length(lp));
    }

    let keyword = S::parse(input)?;
    let lp = input.try_parse(|input| LengthPercentage::parse(input)).ok();
    Ok(Position::Side(keyword, lp))
  }
}

impl<S: ToCss> ToCss for Position<S> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use Position::*;
    match &self {
      Center => {
        if dest.minify {
          dest.write_str("50%")
        } else {
          dest.write_str("center")
        }
      }
      Length(lp) => lp.to_css(dest),
      Side(s, lp) => {
        s.to_css(dest)?;
        if let Some(lp) = lp {
          dest.write_str(" ")?;
          lp.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

enum_property!(HorizontalPositionKeyword,
  Left,
  Right
);

enum_property!(VerticalPositionKeyword,
  Top,
  Bottom
);

pub type HorizontalPosition = Position<HorizontalPositionKeyword>;
pub type VerticalPosition = Position<VerticalPositionKeyword>;

#[derive(Debug, Clone, PartialEq)]
pub enum Angle {
  Deg(f32),
  Grad(f32),
  Rad(f32),
  Turn(f32)
}

impl Parse for Angle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
      Angle::Rad(val) => (*val, "rad"),
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
        dest.write_str(s.get(2..).unwrap_or(&s))
      } else {
        dest.write_str(s.get(1..).unwrap_or(&s))
      }
    } else {
      token.to_css(dest)
    }
  }
}
