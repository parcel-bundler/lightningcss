//! CSS easing functions.

use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::number::{CSSInteger, CSSNumber};
use cssparser::*;
use std::fmt::Write;

/// A CSS [easing function](https://www.w3.org/TR/css-easing-1/#easing-functions).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum EasingFunction {
  /// A linear easing function.
  Linear,
  /// Equivalent to `cubic-bezier(0.25, 0.1, 0.25, 1)`.
  Ease,
  /// Equivalent to `cubic-bezier(0.42, 0, 1, 1)`.
  EaseIn,
  /// Equivalent to `cubic-bezier(0, 0, 0.58, 1)`.
  EaseOut,
  /// Equivalent to `cubic-bezier(0.42, 0, 0.58, 1)`.
  EaseInOut,
  /// A custom cubic Bézier easing function.
  ///
  /// The four numbers specify points P1 and P2 of the curve as (x1, y1, x2, y2).
  CubicBezier(CSSNumber, CSSNumber, CSSNumber, CSSNumber),
  /// A step easing function.
  ///
  /// The first parameter specifies the number of intervals in the function.
  /// The second parameter specifies the step position.
  Steps(CSSInteger, StepPosition),
}

impl<'i> Parse<'i> for EasingFunction {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    if let Ok(ident) = input.try_parse(|i| i.expect_ident_cloned()) {
      let keyword = match_ignore_ascii_case! { &ident,
        "linear" => EasingFunction::Linear,
        "ease" => EasingFunction::Ease,
        "ease-in" => EasingFunction::EaseIn,
        "ease-out" => EasingFunction::EaseOut,
        "ease-in-out" => EasingFunction::EaseInOut,
        "step-start" => EasingFunction::Steps(1, StepPosition::Start),
        "step-end" => EasingFunction::Steps(1, StepPosition::End),
        _ => return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())))
      };
      return Ok(keyword);
    }

    let function = input.expect_function()?.clone();
    input.parse_nested_block(|input| {
      match_ignore_ascii_case! { &function,
        "cubic-bezier" => {
          let x1 = CSSNumber::parse(input)?;
          input.expect_comma()?;
          let y1 = CSSNumber::parse(input)?;
          input.expect_comma()?;
          let x2 = CSSNumber::parse(input)?;
          input.expect_comma()?;
          let y2 = CSSNumber::parse(input)?;
          Ok(EasingFunction::CubicBezier(x1, y1, x2, y2))
        },
        "steps" => {
          let steps = CSSInteger::parse(input)?;
          let position = input.try_parse(|input| {
            input.expect_comma()?;
            StepPosition::parse(input)
          }).unwrap_or(StepPosition::End);
          Ok(EasingFunction::Steps(steps, position))
        },
        _ => return Err(location.new_unexpected_token_error(Token::Ident(function.clone())))
      }
    })
  }
}

impl ToCss for EasingFunction {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      EasingFunction::Linear => dest.write_str("linear"),
      EasingFunction::Ease => dest.write_str("ease"),
      EasingFunction::EaseIn => dest.write_str("ease-in"),
      EasingFunction::EaseOut => dest.write_str("ease-out"),
      EasingFunction::EaseInOut => dest.write_str("ease-in-out"),
      x if *x == EasingFunction::CubicBezier(0.25, 0.1, 0.25, 1.0) => dest.write_str("ease"),
      x if *x == EasingFunction::CubicBezier(0.42, 0.0, 1.0, 1.0) => dest.write_str("ease-in"),
      x if *x == EasingFunction::CubicBezier(0.0, 0.0, 0.58, 1.0) => dest.write_str("ease-out"),
      x if *x == EasingFunction::CubicBezier(0.42, 0.0, 0.58, 1.0) => dest.write_str("ease-in-out"),
      EasingFunction::CubicBezier(x1, y1, x2, y2) => {
        dest.write_str("cubic-bezier(")?;
        x1.to_css(dest)?;
        dest.delim(',', false)?;
        y1.to_css(dest)?;
        dest.delim(',', false)?;
        x2.to_css(dest)?;
        dest.delim(',', false)?;
        y2.to_css(dest)?;
        dest.write_char(')')
      }
      EasingFunction::Steps(1, StepPosition::Start) => dest.write_str("step-start"),
      EasingFunction::Steps(1, StepPosition::End) => dest.write_str("step-end"),
      EasingFunction::Steps(steps, position) => {
        dest.write_str("steps(")?;
        write!(dest, "{}", steps)?;
        dest.delim(',', false)?;
        position.to_css(dest)?;
        dest.write_char(')')
      }
    }
  }
}

impl EasingFunction {
  /// Returns whether the given string is a valid easing function name.
  pub fn is_ident(s: &str) -> bool {
    match s {
      "linear" | "ease" | "ease-in" | "ease-out" | "ease-in-out" | "step-start" | "step-end" => true,
      _ => false,
    }
  }
}

/// A [step position](https://www.w3.org/TR/css-easing-1/#step-position), used within the `steps()` function.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum StepPosition {
  /// The first rise occurs at input progress value of 0.
  Start,
  /// The last rise occurs at input progress value of 1.
  End,
  /// All rises occur within the range (0, 1).
  JumpNone,
  /// The first rise occurs at input progress value of 0 and the last rise occurs at input progress value of 1.
  JumpBoth,
}

impl<'i> Parse<'i> for StepPosition {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    let keyword = match_ignore_ascii_case! { &ident,
      "start" => StepPosition::Start,
      "end" => StepPosition::End,
      "jump-start" => StepPosition::Start,
      "jump-end" => StepPosition::End,
      "jump-none" => StepPosition::JumpNone,
      "jump-both" => StepPosition::JumpBoth,
      _ => return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())))
    };
    Ok(keyword)
  }
}

impl ToCss for StepPosition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      StepPosition::Start => dest.write_str("start"),
      StepPosition::End => dest.write_str("end"),
      StepPosition::JumpNone => dest.write_str("jump-none"),
      StepPosition::JumpBoth => dest.write_str("jump-both"),
    }
  }
}
