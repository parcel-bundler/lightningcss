use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::Property;
use crate::declaration::DeclarationList;
use crate::macros::enum_property;
use crate::printer::Printer;
use std::fmt::Write;

enum_property!(OverflowKeyword,
  Visible,
  Hidden,
  Clip,
  Scroll,
  Auto
);

/// https://www.w3.org/TR/2020/WD-css-overflow-3-20200603/#overflow-properties
#[derive(Debug, Clone, PartialEq)]
pub struct Overflow {
  x: OverflowKeyword,
  y: OverflowKeyword
}

impl Parse for Overflow {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let x = OverflowKeyword::parse(input)?;
    let y = input.try_parse(OverflowKeyword::parse).unwrap_or_else(|_| x.clone());
    Ok(Overflow { x, y })
  }
}

impl ToCss for Overflow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.x.to_css(dest)?;
    if self.y != self.x {
      dest.write_char(' ')?;
      self.y.to_css(dest)?;
    }
    Ok(())
  }
}

// https://www.w3.org/TR/2020/WD-css-overflow-3-20200603/#text-overflow
enum_property!(TextOverflow,
  Clip,
  Ellipsis
);

#[derive(Default)]
pub struct OverflowHandler {
  x: Option<OverflowKeyword>,
  y: Option<OverflowKeyword>
}

impl PropertyHandler for OverflowHandler {
  fn handle_property(&mut self, property: &Property, _: &mut DeclarationList) -> bool {
    use Property::*;

    match property {
      OverflowX(val) => self.x = Some(*val),
      OverflowY(val) => self.y = Some(*val),
      Overflow(val) => {
        self.x = Some(val.x);
        self.y = Some(val.y);
      },
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    let x = std::mem::take(&mut self.x);
    let y = std::mem::take(&mut self.y);

    if let (Some(x), Some(y)) = (x, y) {
      dest.push(Property::Overflow(Overflow { x, y }))
    } else {
      if let Some(x) = x {
        dest.push(Property::OverflowX(x))
      }

      if let Some(y) = y {
        dest.push(Property::OverflowY(y))
      }
    }
  }
}
