use cssparser::*;
use crate::values::macros::*;
use crate::values::length::{LengthPercentageOrAuto, LengthPercentage, Length, Percentage};
use crate::values::traits::{Parse, ToCss, PropertyHandler};
use super::Property;
use crate::printer::Printer;

// https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119/#propdef-flex-direction
enum_property!(FlexDirection,
  ("row", Row),
  ("row-reverse", RowReverse),
  ("column", Column),
  ("column-reverse", ColumnReverse)
);

impl Default for FlexDirection {
  fn default() -> FlexDirection {
    FlexDirection::Row
  }
}

// https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119/#flex-wrap-property
enum_property!(FlexWrap,
  ("nowrap", NoWrap),
  ("wrap", Wrap),
  ("wrap-reverse", WrapReverse)
);

impl Default for FlexWrap {
  fn default() -> FlexWrap {
    FlexWrap::NoWrap
  }
}

/// https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119/#flex-flow-property
#[derive(Debug, Clone, PartialEq)]
pub struct FlexFlow {
  direction: FlexDirection,
  wrap: FlexWrap
}

impl Parse for FlexFlow {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut direction = None;
    let mut wrap = None;
    loop {
      if direction.is_none() {
        if let Ok(value) = input.try_parse(FlexDirection::parse) {
          direction = Some(value);
          continue
        }
      }
      if wrap.is_none() {
        if let Ok(value) = input.try_parse(FlexWrap::parse) {
          wrap = Some(value);
          continue
        }
      }
      break
    }

    Ok(FlexFlow {
      direction: direction.unwrap_or_default(),
      wrap: wrap.unwrap_or_default()
    })
  }
}

impl ToCss for FlexFlow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut needs_space = false;
    if self.direction != FlexDirection::default() || self.wrap == FlexWrap::default() {
      self.direction.to_css(dest)?;
      needs_space = true;
    }

    if self.wrap != FlexWrap::default() {
      if needs_space {
        dest.write_str(" ")?;
      }
      self.wrap.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Flex {
  pub grow: f32,
  pub shrink: f32,
  pub basis: LengthPercentageOrAuto
}

impl Parse for Flex {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(Flex {
        grow: 0.0,
        shrink: 0.0,
        basis: LengthPercentageOrAuto::Auto
      })
    }

    let mut grow = None;
    let mut shrink = None;
    let mut basis = None;

    loop {
      if grow.is_none() {
        if let Ok(val) = input.try_parse(f32::parse) {
          grow = Some(val);
          shrink = input.try_parse(f32::parse).ok();
          continue
        }
      }

      if basis.is_none() {
        if let Ok(val) = input.try_parse(LengthPercentageOrAuto::parse) {
          basis = Some(val);
          continue
        }
      }

      break
    }

    Ok(Flex {
      grow: grow.unwrap_or(1.0),
      shrink: shrink.unwrap_or(1.0),
      basis: basis.unwrap_or(LengthPercentageOrAuto::LengthPercentage(LengthPercentage::Percentage(Percentage(0.0))))
    })
  }
}

impl ToCss for Flex {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if self.grow == 0.0 && self.shrink == 0.0 && self.basis == LengthPercentageOrAuto::Auto {
      dest.write_str("none")?;
      return Ok(())
    }

    let is_basis_zero = match &self.basis {
      LengthPercentageOrAuto::LengthPercentage(LengthPercentage::Percentage(Percentage(b))) | 
      LengthPercentageOrAuto::LengthPercentage(LengthPercentage::Length(Length {value: b, ..})) => *b == 0.0,
      _ => false
    };

    if self.grow != 1.0 || self.shrink != 1.0 || is_basis_zero {
      self.grow.to_css(dest)?;
      if self.shrink != 1.0 {
        dest.write_str(" ")?;
        self.shrink.to_css(dest)?;
      }
    }

    if !is_basis_zero {
      if self.grow != 1.0 || self.shrink != 1.0 {
        dest.write_str(" ")?;
      }
      self.basis.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Default, Debug)]
pub struct FlexHandler {
  direction: Option<FlexDirection>,
  wrap: Option<FlexWrap>,
  grow: Option<f32>,
  shrink: Option<f32>,
  basis: Option<LengthPercentageOrAuto>
}

impl PropertyHandler for FlexHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;

    match property {
      FlexDirection(val) => self.direction = Some(val.clone()),
      FlexWrap(val) => self.wrap = Some(val.clone()),
      FlexFlow(val) => {
        self.direction = Some(val.direction.clone());
        self.wrap = Some(val.wrap.clone());
      }
      FlexGrow(val) => self.grow = Some(val.clone()),
      FlexShrink(val) => self.shrink = Some(val.clone()),
      FlexBasis(val) => self.basis = Some(val.clone()),
      Flex(val) => {
        self.grow = Some(val.grow.clone());
        self.shrink = Some(val.shrink.clone());
        self.basis = Some(val.basis.clone());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self) -> Vec<Property> {
    let mut decls = vec![];
    let direction = std::mem::take(&mut self.direction);
    let wrap = std::mem::take(&mut self.wrap);
    let grow = std::mem::take(&mut self.grow);
    let shrink = std::mem::take(&mut self.shrink);
    let basis = std::mem::take(&mut self.basis);
    
    if direction.is_some() && wrap.is_some() {
      decls.push(Property::FlexFlow(FlexFlow {
        direction: direction.unwrap(),
        wrap: wrap.unwrap()
      }))
    } else {
      if let Some(direction) = direction {
        decls.push(Property::FlexDirection(direction))
      }

      if let Some(wrap) = wrap {
        decls.push(Property::FlexWrap(wrap))
      }
    }

    if grow.is_some() && shrink.is_some() && basis.is_some() {
      decls.push(Property::Flex(Flex {
        grow: grow.unwrap(),
        shrink: shrink.unwrap(),
        basis: basis.unwrap()
      }))
    } else {
      if let Some(grow) = grow {
        decls.push(Property::FlexGrow(grow))
      }

      if let Some(shrink) = shrink {
        decls.push(Property::FlexShrink(shrink))
      }

      if let Some(basis) = basis {
        decls.push(Property::FlexBasis(basis))
      }
    }

    decls
  }
}
