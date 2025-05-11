//! CSS properties related to overflow.

use super::{Property, PropertyId};
use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property};
use crate::printer::Printer;
use crate::traits::{Parse, PropertyHandler, Shorthand, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

enum_property! {
  /// An [overflow](https://www.w3.org/TR/css-overflow-3/#overflow-properties) keyword
  /// as used in the `overflow-x`, `overflow-y`, and `overflow` properties.
  pub enum OverflowKeyword {
    /// Overflowing content is visible.
    Visible,
    /// Overflowing content is hidden. Programmatic scrolling is allowed.
    Hidden,
    /// Overflowing content is clipped. Programmatic scrolling is not allowed.
    Clip,
    /// The element is scrollable.
    Scroll,
    /// Overflowing content scrolls if needed.
    Auto,
  }
}

define_shorthand! {
  /// A value for the [overflow](https://www.w3.org/TR/css-overflow-3/#overflow-properties) shorthand property.
  pub struct Overflow {
    /// The overflow mode for the x direction.
    x: OverflowX(OverflowKeyword),
    /// The overflow mode for the y direction.
    y: OverflowY(OverflowKeyword),
  }
}

impl<'i> Parse<'i> for Overflow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let x = OverflowKeyword::parse(input)?;
    let y = input.try_parse(OverflowKeyword::parse).unwrap_or_else(|_| x.clone());
    Ok(Overflow { x, y })
  }
}

impl ToCss for Overflow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x.to_css(dest)?;
    if self.y != self.x {
      dest.write_char(' ')?;
      self.y.to_css(dest)?;
    }
    Ok(())
  }
}

enum_property! {
  /// A value for the [text-overflow](https://www.w3.org/TR/css-overflow-3/#text-overflow) property.
  pub enum TextOverflow {
    /// Overflowing text is clipped.
    Clip,
    /// Overflowing text is truncated with an ellipsis.
    Ellipsis,
  }
}

#[derive(Default)]
pub(crate) struct OverflowHandler {
  x: Option<OverflowKeyword>,
  y: Option<OverflowKeyword>,
}

impl<'i> PropertyHandler<'i> for OverflowHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    match property {
      OverflowX(val) => self.x = Some(*val),
      OverflowY(val) => self.y = Some(*val),
      Overflow(val) => {
        self.x = Some(val.x);
        self.y = Some(val.y);
      }
      Unparsed(val)
        if matches!(
          val.property_id,
          PropertyId::OverflowX | PropertyId::OverflowY | PropertyId::Overflow
        ) =>
      {
        self.finalize(dest, context);
        dest.push(property.clone());
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList, context: &mut PropertyHandlerContext<'i, '_>) {
    if self.x.is_none() && self.y.is_none() {
      return;
    }

    let x = std::mem::take(&mut self.x);
    let y = std::mem::take(&mut self.y);

    match (x, y) {
      // Only use shorthand syntax if the x and y values are the
      // same or the two-value syntax is supported by all targets.
      (Some(x), Some(y)) if x == y || context.targets.is_compatible(Feature::OverflowShorthand) => {
        dest.push(Property::Overflow(Overflow { x, y }))
      }
      _ => {
        if let Some(x) = x {
          dest.push(Property::OverflowX(x))
        }

        if let Some(y) = y {
          dest.push(Property::OverflowY(y))
        }
      }
    }
  }
}
