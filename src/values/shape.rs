//! CSS shape values for masking and clipping.

use super::length::LengthPercentage;
use super::position::Position;
use super::rect::Rect;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::properties::border_radius::BorderRadius;
use crate::traits::{Parse, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A CSS [`<basic-shape>`](https://www.w3.org/TR/css-shapes-1/#basic-shape-functions) value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum BasicShape {
  /// An inset rectangle.
  Inset(InsetRect),
  /// A circle.
  Circle(Circle),
  /// An ellipse.
  Ellipse(Ellipse),
  /// A polygon.
  Polygon(Polygon),
}

/// An [`inset()`](https://www.w3.org/TR/css-shapes-1/#funcdef-inset) rectangle shape.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct InsetRect {
  /// The rectangle.
  pub rect: Rect<LengthPercentage>,
  /// A corner radius for the rectangle.
  pub radius: BorderRadius,
}

/// A [`circle()`](https://www.w3.org/TR/css-shapes-1/#funcdef-circle) shape.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Circle {
  /// The radius of the circle.
  pub radius: ShapeRadius,
  /// The position of the center of the circle.
  pub position: Position,
}

/// A [`<shape-radius>`](https://www.w3.org/TR/css-shapes-1/#typedef-shape-radius) value
/// that defines the radius of a `circle()` or `ellipse()` shape.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ShapeRadius {
  /// An explicit length or percentage.
  LengthPercentage(LengthPercentage),
  /// The length from the center to the closest side of the box.
  ClosestSide,
  /// The length from the center to the farthest side of the box.
  FarthestSide,
}

/// An [`ellipse()`](https://www.w3.org/TR/css-shapes-1/#funcdef-ellipse) shape.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Ellipse {
  /// The x-radius of the ellipse.
  pub radius_x: ShapeRadius,
  /// The y-radius of the ellipse.
  pub radius_y: ShapeRadius,
  /// The position of the center of the ellipse.
  pub position: Position,
}

/// A [`polygon()`](https://www.w3.org/TR/css-shapes-1/#funcdef-polygon) shape.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Polygon {
  /// The fill rule used to determine the interior of the polygon.
  pub fill_rule: FillRule,
  /// The points of each vertex of the polygon.
  pub points: Vec<Point>,
}

/// A point within a `polygon()` shape.
///
/// See [Polygon](Polygon).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Point {
  /// The x position of the point.
  x: LengthPercentage,
  /// the y position of the point.
  y: LengthPercentage,
}

enum_property! {
  /// A [`<fill-rule>`](https://www.w3.org/TR/css-shapes-1/#typedef-fill-rule) used to
  /// determine the interior of a `polygon()` shape.
  ///
  /// See [Polygon](Polygon).
  pub enum FillRule {
    /// The `nonzero` fill rule.
    Nonzero,
    /// The `evenodd` fill rule.
    Evenodd,
  }
}

impl Default for FillRule {
  fn default() -> FillRule {
    FillRule::Nonzero
  }
}

impl<'i> Parse<'i> for BasicShape {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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

impl<'i> Parse<'i> for InsetRect {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let rect = Rect::parse(input)?;
    let radius = if input.try_parse(|input| input.expect_ident_matching("round")).is_ok() {
      BorderRadius::parse(input)?
    } else {
      BorderRadius::default()
    };
    Ok(InsetRect { rect, radius })
  }
}

impl<'i> Parse<'i> for Circle {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let radius = input.try_parse(ShapeRadius::parse).unwrap_or_default();
    let position = if input.try_parse(|input| input.expect_ident_matching("at")).is_ok() {
      Position::parse(input)?
    } else {
      Position::center()
    };

    Ok(Circle { radius, position })
  }
}

impl Default for ShapeRadius {
  fn default() -> ShapeRadius {
    ShapeRadius::ClosestSide
  }
}

impl<'i> Parse<'i> for Ellipse {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (x, y) = input
      .try_parse(|input| -> Result<_, ParseError<'i, ParserError<'i>>> {
        Ok((ShapeRadius::parse(input)?, ShapeRadius::parse(input)?))
      })
      .unwrap_or_default();

    let position = if input.try_parse(|input| input.expect_ident_matching("at")).is_ok() {
      Position::parse(input)?
    } else {
      Position::center()
    };

    Ok(Ellipse {
      radius_x: x,
      radius_y: y,
      position,
    })
  }
}

impl<'i> Parse<'i> for Polygon {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let fill_rule = input.try_parse(FillRule::parse);
    if fill_rule.is_ok() {
      input.expect_comma()?;
    }

    let points = input.parse_comma_separated(Point::parse)?;
    Ok(Polygon {
      fill_rule: fill_rule.unwrap_or_default(),
      points,
    })
  }
}

impl<'i> Parse<'i> for Point {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let x = LengthPercentage::parse(input)?;
    let y = LengthPercentage::parse(input)?;
    Ok(Point { x, y })
  }
}

impl ToCss for BasicShape {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.rect.to_css(dest)?;
    if self.radius != BorderRadius::default() {
      dest.write_str(" round ")?;
      self.radius.to_css(dest)?;
    }
    Ok(())
  }
}

impl ToCss for Circle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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

impl ToCss for Ellipse {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x.to_css(dest)?;
    dest.write_char(' ')?;
    self.y.to_css(dest)
  }
}
