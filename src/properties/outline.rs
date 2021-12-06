use cssparser::*;
use super::border::{BorderStyle, GenericBorder, BorderSideWidth};
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::color::CssColor;
use super::{Property, PropertyId};
use crate::declaration::DeclarationList;
use crate::printer::Printer;

#[derive(Debug, Clone, PartialEq)]
pub enum OutlineStyle {
  Auto,
  BorderStyle(BorderStyle)
}

impl Parse for OutlineStyle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(border_style) = input.try_parse(BorderStyle::parse) {
      return Ok(OutlineStyle::BorderStyle(border_style))
    }

    input.expect_ident_matching("auto")?;
    Ok(OutlineStyle::Auto)
  }
}

impl ToCss for OutlineStyle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      OutlineStyle::Auto => dest.write_str("auto"),
      OutlineStyle::BorderStyle(border_style) => border_style.to_css(dest)
    }
  }
}

impl Default for OutlineStyle {
  fn default() -> OutlineStyle {
    OutlineStyle::BorderStyle(BorderStyle::None)
  }
}

pub type Outline = GenericBorder<OutlineStyle>;

#[derive(Default, Debug)]
pub(crate) struct OutlineHandler {
  pub width: Option<BorderSideWidth>,
  pub style: Option<OutlineStyle>,
  pub color: Option<CssColor>
}

impl PropertyHandler for OutlineHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;

    match property {
      OutlineColor(val) => self.color = Some(val.clone()),
      OutlineStyle(val) => self.style = Some(val.clone()),
      OutlineWidth(val) => self.width = Some(val.clone()),
      Outline(val) => {
        self.color = Some(val.color.clone());
        self.style = Some(val.style.clone());
        self.width = Some(val.width.clone());
      }
      Unparsed(val) if is_outline_property(&val.property_id) => {
        self.finalize(dest);
        dest.push(property.clone());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, decls: &mut DeclarationList) {
    if self.width.is_none() && self.style.is_none() && self.color.is_none() {
      return
    }

    let width = std::mem::take(&mut self.width);
    let style = std::mem::take(&mut self.style);
    let color = std::mem::take(&mut self.color);
    if width.is_some() && style.is_some() && color.is_some() {
      decls.push(Property::Outline(Outline {
        width: width.unwrap(),
        style: style.unwrap(),
        color: color.unwrap()
      }))
    } else {
      if let Some(color) = color {
        decls.push(Property::OutlineColor(color))
      }

      if let Some(style) = style {
        decls.push(Property::OutlineStyle(style))
      }

      if let Some(width) = width {
        decls.push(Property::OutlineWidth(width))
      }
    }
  }
}

#[inline]
fn is_outline_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::OutlineColor |
    PropertyId::OutlineStyle |
    PropertyId::OutlineWidth |
    PropertyId::Outline => true,
    _ => false
  }
}
