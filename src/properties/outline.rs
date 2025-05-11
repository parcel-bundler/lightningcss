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
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum OutlineStyle {
  /// The `auto` keyword.
  Auto,
  /// A value equivalent to the `border-style` property.
  LineStyle(LineStyle),
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
