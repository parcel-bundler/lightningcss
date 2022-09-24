//! CSS properties related to Multi-column Layout.
//! https://www.w3.org/TR/css-multicol-1/

use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, shorthand_handler};
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::length::{IntegerOrAuto, Length, LengthOrAuto, LengthValue};
use cssparser::*;
use std::ops::Not;

define_shorthand! {
  /// A value for the [columns](https://www.w3.org/TR/css-multicol-1/#columns) shorthand property.
  /// columns = <'column-width'> || <'column-count'>
  pub struct Columns {
    /// The column-width property.
    width: ColumnWidth(LengthOrAuto),
    /// The column-count property.
    count: ColumnCount(IntegerOrAuto),
  }
}

impl<'i> Parse<'i> for Columns {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let state = input.state();
    let with_or_count = LengthOrAuto::parse(input)?;

    if let LengthOrAuto::Length(Length::Value(LengthValue::Unitless(..))) = with_or_count {
      // first is unitless, so it must be column-count
      // reparse with column-count
      input.reset(&state);
      let count = IntegerOrAuto::parse(input)?;

      let width = if input.try_parse(|input| LengthOrAuto::parse(input)).is_ok() {
        LengthOrAuto::parse(input)?
      } else {
        LengthOrAuto::Auto
      };

      return Ok(Columns { width, count });
    } else if matches!(with_or_count, LengthOrAuto::Auto).not()
      && matches!(
        with_or_count,
        LengthOrAuto::Length(Length::Value(LengthValue::Unitless(..)))
      )
      .not()
    {
      // first is neither unitless nor auto, so it must be column-width
      let count = if input.try_parse(|input| IntegerOrAuto::parse(input)).is_ok() {
        IntegerOrAuto::parse(input)?
      } else {
        IntegerOrAuto::default()
      };

      return Ok(Columns {
        width: with_or_count,
        count,
      });
    }

    // first is auto, check second
    let state = input.state();
    let count = if input.try_parse(|input| LengthOrAuto::parse(input)).is_ok() {
      LengthOrAuto::parse(input)?
    } else {
      LengthOrAuto::default()
    };

    if let LengthOrAuto::Auto = count {
      // second is auto, so first must be column-width
      Ok(Columns {
        width: LengthOrAuto::Auto,
        count: IntegerOrAuto::Auto,
      })
    } else if matches!(count, LengthOrAuto::Length(Length::Value(LengthValue::Unitless(..)))).not() {
      // second is not unitless, so first must be column-count
      // reparse with column-with
      Ok(Columns {
        width: count,
        count: IntegerOrAuto::Auto,
      })
    } else {
      // second is unitless, so first must be column-width
      input.reset(&state);
      let count = IntegerOrAuto::parse(input)?;

      Ok(Columns {
        width: with_or_count,
        count,
      })
    }
  }
}

impl<'i> ToCss for Columns {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.width != LengthOrAuto::default() {
      self.width.to_css(dest)?;
    }

    if self.width != LengthOrAuto::default() && self.count != IntegerOrAuto::default() {
      dest.write_str(" ")?;
    }

    if self.count != IntegerOrAuto::default() {
      self.count.to_css(dest)?;
    }
    Ok(())
  }
}

shorthand_handler!(ColumnsHandler -> Columns {
  width: ColumnWidth(LengthOrAuto),
  count: ColumnCount(IntegerOrAuto),
});
