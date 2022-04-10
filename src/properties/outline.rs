//! CSS properties related to outlines.

use super::border::{BorderSideWidth, BorderStyle, GenericBorder};
use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::shorthand_handler;
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{FallbackValues, Parse, PropertyHandler, ToCss};
use crate::values::color::CssColor;
use cssparser::*;

/// A value for the [outline-style](https://drafts.csswg.org/css-ui/#outline-style) property.
#[derive(Debug, Clone, PartialEq)]
pub enum OutlineStyle {
  /// The `auto` keyword.
  Auto,
  /// A value equivalent to the `border-style` property.
  BorderStyle(BorderStyle),
}

impl<'i> Parse<'i> for OutlineStyle {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(border_style) = input.try_parse(BorderStyle::parse) {
      return Ok(OutlineStyle::BorderStyle(border_style));
    }

    input.expect_ident_matching("auto")?;
    Ok(OutlineStyle::Auto)
  }
}

impl ToCss for OutlineStyle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      OutlineStyle::Auto => dest.write_str("auto"),
      OutlineStyle::BorderStyle(border_style) => border_style.to_css(dest),
    }
  }
}

impl Default for OutlineStyle {
  fn default() -> OutlineStyle {
    OutlineStyle::BorderStyle(BorderStyle::None)
  }
}

/// A value for the [outline](https://drafts.csswg.org/css-ui/#outline) shorthand property.
pub type Outline = GenericBorder<OutlineStyle>;

shorthand_handler!(OutlineHandler -> Outline {
  width: OutlineWidth(BorderSideWidth),
  style: OutlineStyle(OutlineStyle),
  color: OutlineColor(CssColor, fallback: true),
});
