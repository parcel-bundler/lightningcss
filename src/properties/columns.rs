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
use crate::values::length::{LengthOrAuto, IntegerOrAuto};
use cssparser::*;

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
    let width = LengthOrAuto::parse(input)?;
    let count = if input.try_parse(|input| IntegerOrAuto::parse(input)).is_ok() {
      IntegerOrAuto::parse(input)?
    } else {
      IntegerOrAuto::default()
    };
    Ok(Columns { width, count })
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
