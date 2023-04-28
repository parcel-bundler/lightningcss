//! CSS properties related to outlines.

use super::border::{BorderSideWidth, GenericBorder, LineStyle};
use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{impl_shorthand, shorthand_handler};
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{FallbackValues, IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::color::CssColor;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A value for the [outline-style](https://drafts.csswg.org/css-ui/#outline-style) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum OutlineStyle {
  /// The `auto` keyword.
  Auto,
  /// A value equivalent to the `border-style` property.
  LineStyle(LineStyle),
}

impl<'i> Parse<'i> for OutlineStyle {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(border_style) = input.try_parse(LineStyle::parse) {
      return Ok(OutlineStyle::LineStyle(border_style));
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
      OutlineStyle::LineStyle(border_style) => border_style.to_css(dest),
    }
  }
}

impl Default for OutlineStyle {
  fn default() -> OutlineStyle {
    OutlineStyle::LineStyle(LineStyle::None)
  }
}

impl IsCompatible for OutlineStyle {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

/// A value for the [outline](https://drafts.csswg.org/css-ui/#outline) shorthand property.
pub type Outline = GenericBorder<OutlineStyle, 11>;

impl_shorthand! {
  Outline(Outline) {
    width: [OutlineWidth],
    style: [OutlineStyle],
    color: [OutlineColor],
  }
}

shorthand_handler!(OutlineHandler -> Outline fallbacks: true {
  width: OutlineWidth(BorderSideWidth),
  style: OutlineStyle(OutlineStyle),
  color: OutlineColor(CssColor, fallback: true),
});
