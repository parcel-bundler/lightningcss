use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::{Property, PropertyId};
use crate::declaration::DeclarationList;
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};
use crate::context::PropertyHandlerContext;

enum_property! {
  pub enum OverflowKeyword {
    Visible,
    Hidden,
    Clip,
    Scroll,
    Auto,
  }
}

/// https://www.w3.org/TR/2020/WD-css-overflow-3-20200603/#overflow-properties
#[derive(Debug, Clone, PartialEq)]
pub struct Overflow {
  pub x: OverflowKeyword,
  pub y: OverflowKeyword
}

impl<'i> Parse<'i> for Overflow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let x = OverflowKeyword::parse(input)?;
    let y = input.try_parse(OverflowKeyword::parse).unwrap_or_else(|_| x.clone());
    Ok(Overflow { x, y })
  }
}

impl ToCss for Overflow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.x.to_css(dest)?;
    if self.y != self.x {
      dest.write_char(' ')?;
      self.y.to_css(dest)?;
    }
    Ok(())
  }
}

enum_property! {
  /// https://www.w3.org/TR/2020/WD-css-overflow-3-20200603/#text-overflow
  pub enum TextOverflow {
    Clip,
    Ellipsis,
  }
}

#[derive(Default)]
pub(crate) struct OverflowHandler {
  targets: Option<Browsers>,
  x: Option<OverflowKeyword>,
  y: Option<OverflowKeyword>
}

impl OverflowHandler {
  pub fn new(targets: Option<Browsers>) -> OverflowHandler {
    OverflowHandler {
      targets,
      ..OverflowHandler::default()
    }
  }
}


impl<'i> PropertyHandler<'i> for OverflowHandler {
  fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i>) -> bool {
    use Property::*;

    match property {
      OverflowX(val) => self.x = Some(*val),
      OverflowY(val) => self.y = Some(*val),
      Overflow(val) => {
        self.x = Some(val.x);
        self.y = Some(val.y);
      }
      Unparsed(val) if matches!(val.property_id, PropertyId::OverflowX | PropertyId::OverflowY | PropertyId::Overflow) => {
        self.finalize(dest, context);
        dest.push(property.clone());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList, _: &mut PropertyHandlerContext<'i>) {
    if self.x.is_none() && self.y.is_none() {
      return
    }

    let x = std::mem::take(&mut self.x);
    let y = std::mem::take(&mut self.y);

    match (x, y) {
      // Only use shorthand syntax if the x and y values are the 
      // same or the two-value syntax is supported by all targets.
      (Some(x), Some(y)) if x == y || self.targets.is_none() || Feature::OverflowShorthand.is_compatible(self.targets.unwrap()) => {
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
