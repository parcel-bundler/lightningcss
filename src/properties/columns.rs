//! CSS properties related to Multi-column Layout.
//! https://www.w3.org/TR/css-multicol-1/

use super::{Property, PropertyId};
use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property};
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{Parse, PropertyHandler, Shorthand, ToCss};
use cssparser::*;

define_shorthand! {
  /// A value for the [columns](https://www.w3.org/TR/css-multicol-1/#columns) shorthand property.
  /// columns = <'column-width'> || <'column-count'>
  pub struct Columns {
    /// column-width
    a: ColumnWidth(),
    /// column-count
    b: ColumnCount(),
  }
}
