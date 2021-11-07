use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::length::LengthPercentage;
use crate::macros::enum_property;

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

fn parse_fit_content<'i, 't>(input: &mut Parser<'i, 't>) -> Result<LengthPercentage, ParseError<'i, ()>> {
  input.expect_function_matching("fit-content")?;
  input.parse_nested_block(|input| LengthPercentage::parse(input))
}

// https://drafts.csswg.org/css-sizing-3/#box-sizing
enum_property!(BoxSizing,
  ("content-box", ContentBox),
  ("border-box", BorderBox)
);
