use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use super::rect::Rect;
use super::length::LengthPercentage;
use crate::error::{ParserError, PrinterError};
use crate::properties::border_radius::BorderRadius;
use super::position::Position;
use crate::macros::enum_property;

/// https://www.w3.org/TR/css-shapes-1/#basic-shape-functions
#[derive(Debug, Clone, PartialEq)]
pub enum BasicShape {
  Inset(InsetRect),
  Circle(Circle),
  Ellipse(Ellipse),
  Polygon(Polygon)
}

/// https://www.w3.org/TR/css-shapes-1/#funcdef-inset
#[derive(Debug, Clone, PartialEq)]
pub struct InsetRect {
  pub rect: Rect<LengthPercentage>,
  pub radius: BorderRadius
}

/// https://www.w3.org/TR/css-shapes-1/#funcdef-circle
#[derive(Debug, Clone, PartialEq)]
pub struct Circle {
  pub radius: ShapeRadius,
  pub position: Position,
}

/// https://www.w3.org/TR/css-shapes-1/#typedef-shape-radius
#[derive(Debug, Clone, PartialEq)]
pub enum ShapeRadius {
  LengthPercentage(LengthPercentage),
  ClosestSide,
  FarthestSide
}

/// https://www.w3.org/TR/css-shapes-1/#funcdef-ellipse
#[derive(Debug, Clone, PartialEq)]
pub struct Ellipse {
  pub radius_x: ShapeRadius,
  pub radius_y: ShapeRadius,
  pub position: Position
}

/// https://www.w3.org/TR/css-shapes-1/#funcdef-polygon
#[derive(Debug, Clone, PartialEq)]
pub struct Polygon {
  pub fill_rule: FillRule,
  pub points: Vec<Point>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Point {
  x: LengthPercentage,
  y: LengthPercentage
}

enum_property! {
  pub enum FillRule {
    Nonzero,
    Evenodd,
  }
}

impl Default for FillRule {
  fn default() -> FillRule {
    FillRule::Nonzero
  }
}

impl Parse for BasicShape {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let f = input.expect_function()?;
    match_ignore_ascii_case! { &f,
      "inset" => Ok(BasicShape::Inset(input.parse_nested_block(InsetRect::parse)?)),
      "circle" => Ok(BasicShape::Circle(input.parse_nested_block(Circle::parse)?)),
      "ellipse" => Ok(BasicShape::Ellipse(input.parse_nested_block(Ellipse::parse)?)),
      "polygon" => Ok(BasicShape::Polygon(input.parse_nested_block(Polygon::parse)?)),
      _ => Err(location.new_unexpected_token_error(Token::Ident(f.clone()))),
    }
  }
}

impl Parse for InsetRect {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let rect = Rect::parse(input)?;
    let radius = if input.try_parse(|input| input.expect_ident_matching("round")).is_ok() {
      BorderRadius::parse(input)?
    } else {
      BorderRadius::default()
    };
    Ok(InsetRect { rect, radius })
  }
}

impl Parse for Circle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let radius = input.try_parse(ShapeRadius::parse).unwrap_or_default();
    let position = if input.try_parse(|input| input.expect_ident_matching("at")).is_ok() {
      Position::parse(input)?
    } else {
      Position::center()
    };

    Ok(Circle { radius, position })
  }
}

impl Parse for ShapeRadius {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(len) = input.try_parse(LengthPercentage::parse) {
      return Ok(ShapeRadius::LengthPercentage(len))
    }

    if input.try_parse(|input| input.expect_ident_matching("closest-side")).is_ok() {
      return Ok(ShapeRadius::ClosestSide)
    }

    input.expect_ident_matching("farthest-side")?;
    Ok(ShapeRadius::FarthestSide)
  }
}

impl Default for ShapeRadius {
  fn default() -> ShapeRadius {
    ShapeRadius::ClosestSide
  }
}

impl Parse for Ellipse {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (x, y) = input.try_parse(|input| -> Result<_, ParseError<'i, ParserError<'i>>> {
      Ok((ShapeRadius::parse(input)?, ShapeRadius::parse(input)?))
    }).unwrap_or_default();

    let position = if input.try_parse(|input| input.expect_ident_matching("at")).is_ok() {
      Position::parse(input)?
    } else {
      Position::center()
    };

    Ok(Ellipse {
      radius_x: x,
      radius_y: y,
      position
    })
  }
}

impl Parse for Polygon {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let fill_rule = input.try_parse(FillRule::parse);
    if fill_rule.is_ok() {
      input.expect_comma()?;
    }

    let points = input.parse_comma_separated(Point::parse)?;
    Ok(Polygon {
      fill_rule: fill_rule.unwrap_or_default(),
      points
    })
  }
}

impl Parse for Point {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let x = LengthPercentage::parse(input)?;
    let y = LengthPercentage::parse(input)?;
    Ok(Point { x, y })
  }
}

impl ToCss for BasicShape {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      BasicShape::Inset(rect) => {
        dest.write_str("inset(")?;
        rect.to_css(dest)?;
        dest.write_char(')')
      }
      BasicShape::Circle(circle) => {
        dest.write_str("circle(")?;
        circle.to_css(dest)?;
        dest.write_char(')')
      }
      BasicShape::Ellipse(ellipse) => {
        dest.write_str("ellipse(")?;
        ellipse.to_css(dest)?;
        dest.write_char(')')
      }
      BasicShape::Polygon(poly) => {
        dest.write_str("polygon(")?;
        poly.to_css(dest)?;
        dest.write_char(')')
      }
    }
  }
}

impl ToCss for InsetRect {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.rect.to_css(dest)?;
    if self.radius != BorderRadius::default() {
      dest.write_str(" round ")?;
      self.radius.to_css(dest)?;
    }
    Ok(())
  }
}

impl ToCss for Circle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let mut has_output = false;
    if self.radius != ShapeRadius::default() {
      self.radius.to_css(dest)?;
      has_output = true;
    }

    if !self.position.is_center() {
      if has_output {
        dest.write_char(' ')?;
      }
      dest.write_str("at ")?;
      self.position.to_css(dest)?;
    }

    Ok(())
  }
}

impl ToCss for ShapeRadius {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      ShapeRadius::LengthPercentage(len) => len.to_css(dest),
      ShapeRadius::ClosestSide => dest.write_str("closest-side"),
      ShapeRadius::FarthestSide => dest.write_str("farthest-side")
    }
  }
}

impl ToCss for Ellipse {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let mut has_output = false;
    if self.radius_x != ShapeRadius::default() || self.radius_y != ShapeRadius::default() {
      self.radius_x.to_css(dest)?;
      dest.write_char(' ')?;
      self.radius_y.to_css(dest)?;
      has_output = true;
    }

    if !self.position.is_center() {
      if has_output {
        dest.write_char(' ')?;
      }
      dest.write_str("at ")?;
      self.position.to_css(dest)?;
    }

    Ok(())
  }
}

impl ToCss for Polygon {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    if self.fill_rule != FillRule::default() {
      self.fill_rule.to_css(dest)?;
      dest.delim(',', false)?;
    }
    
    let mut first = true;
    for point in &self.points {
      if first {
        first = false;
      } else {
        dest.delim(',', false)?;
      }
      point.to_css(dest)?;
    }

    Ok(())
  }
}

impl ToCss for Point {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.x.to_css(dest)?;
    dest.write_char(' ')?;
    self.y.to_css(dest)
  }
}
