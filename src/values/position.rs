use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::macros::enum_property;
use crate::printer::Printer;
use std::fmt::Write;
use super::length::LengthPercentage;

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
