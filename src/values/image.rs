use cssparser::*;
use super::angle::Angle;
use super::position::{HorizontalPositionKeyword, VerticalPositionKeyword};
use super::color::CssColor;
use super::length::{Length, LengthPercentage};
use super::percentage::Percentage;
use super::position::Position;
use crate::traits::{Parse, ToCss};
use crate::macros::enum_property;
use crate::printer::Printer;
use std::fmt::Write;

/// https://www.w3.org/TR/css-images-3/#typedef-image
#[derive(Debug, Clone, PartialEq)]
pub enum Image {
  None,
  Url(String),
  Gradient(Gradient)
}

impl Default for Image {
  fn default() -> Image {
    Image::None
  }
}

impl Parse for Image {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Image::None)
    }
    
    if let Ok(url) = input.try_parse(|input| input.expect_url()) {
      return Ok(Image::Url(url.as_ref().into()))
    }

    if let Ok(grad) = input.try_parse(Gradient::parse) {
      return Ok(Image::Gradient(grad))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for Image {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use Image::*;
    use cssparser::ToCss;
    match self {
      None => dest.write_str("none"),
      Url(url) => {
        Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(dest)
      }
      Gradient(grad) => grad.to_css(dest)
    }
  }
}

/// https://www.w3.org/TR/css-images-3/#gradients
#[derive(Debug, Clone, PartialEq)]
pub enum Gradient {
  Linear(LinearGradient),
  RepeatingLinear(LinearGradient),
  Radial(RadialGradient),
  RepeatingRadial(RadialGradient),
}

impl Parse for Gradient {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let func = input.expect_function()?.clone();
    input.parse_nested_block(|input| {
      match_ignore_ascii_case! { &func,
        "linear-gradient" => Ok(Gradient::Linear(LinearGradient::parse(input)?)),
        "repeating-linear-gradient" => Ok(Gradient::RepeatingLinear(LinearGradient::parse(input)?)),
        "radial-gradient" => Ok(Gradient::Radial(RadialGradient::parse(input)?)),
        "repeating-radial-gradient" => Ok(Gradient::RepeatingRadial(RadialGradient::parse(input)?)),
        _ => todo!()
      }
    })
  }
}

impl ToCss for Gradient {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let f = match self {
      Gradient::Linear(_) => "linear-gradient(",
      Gradient::RepeatingLinear(_) => "repeating-linear-gradient(",
      Gradient::Radial(_) => "radial-gradient(",
      Gradient::RepeatingRadial(_) => "repeating-radial-gradient("
    };

    dest.write_str(f)?;

    match self {
      Gradient::Linear(linear) | Gradient::RepeatingLinear(linear) => linear.to_css(dest)?,
      Gradient::Radial(radial) | Gradient::RepeatingRadial(radial) => radial.to_css(dest)?
    }

    dest.write_char(')')
  }
}

/// https://www.w3.org/TR/css-images-3/#linear-gradients
#[derive(Debug, Clone, PartialEq)]
pub struct LinearGradient {
  direction: LineDirection,
  items: Vec<GradientItem>,
}

impl Parse for LinearGradient {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<LinearGradient, ParseError<'i, ()>> {
    let direction = if let Ok(direction) = input.try_parse(LineDirection::parse) {
      input.expect_comma()?;
      direction
    } else {
      LineDirection::Vertical(VerticalPositionKeyword::Bottom)
    };
    let items = parse_items(input)?;
    Ok(LinearGradient {
      direction,
      items
    })
  }
}

impl ToCss for LinearGradient {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self.direction {
      // We can omit `to bottom` or `180deg` because it is the default.
      LineDirection::Vertical(VerticalPositionKeyword::Bottom) |
      LineDirection::Angle(Angle::Deg(180.0)) => serialize_items(&self.items, dest),
      // If we have `to top` or `0deg`, and all of the positions and hints are percentages,
      // we can flip the gradient the other direction and omit the direction.
      LineDirection::Vertical(VerticalPositionKeyword::Top) |
      LineDirection::Angle(Angle::Deg(0.0)) if dest.minify && self.items.iter().all(|item| matches!(item, GradientItem::Hint(LengthPercentage::Percentage(_)) | GradientItem::ColorStop(ColorStop { position: None | Some(LengthPercentage::Percentage(_)), .. }))) => {
        let items = self.items.iter().rev().map(|item| {
          // Flip percentages.
          match item {
            GradientItem::Hint(LengthPercentage::Percentage(p)) => GradientItem::Hint(LengthPercentage::Percentage(Percentage(1.0 - p.0))),
            GradientItem::ColorStop(ColorStop { color, position }) => {
              GradientItem::ColorStop(ColorStop {
                color: color.clone(),
                position: position.clone().map(|p| match p {
                  LengthPercentage::Percentage(p) => LengthPercentage::Percentage(Percentage(1.0 - p.0)),
                  _ => unreachable!()
                })
              })
            }
            _ => unreachable!()
          }
        }).collect();
        serialize_items(&items, dest)
      },
      _ => {
        if self.direction != LineDirection::Vertical(VerticalPositionKeyword::Bottom) && self.direction != LineDirection::Angle(Angle::Deg(180.0)) {
          self.direction.to_css(dest)?;
          dest.delim(',', false)?;
        }

        serialize_items(&self.items, dest)
      }
    }
  }
}

/// https://www.w3.org/TR/css-images-3/#radial-gradients
#[derive(Debug, Clone, PartialEq)]
pub struct RadialGradient {
  shape: EndingShape,
  position: Position,
  items: Vec<GradientItem>,
}

impl Parse for RadialGradient {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<RadialGradient, ParseError<'i, ()>> {
    let shape = input.try_parse(EndingShape::parse).ok();
    let position = input.try_parse(|input| {
      input.expect_ident_matching("at")?;
      Position::parse(input)
    }).ok();

    if shape.is_some() || position.is_some() {
      input.expect_comma()?;
    }

    let items = parse_items(input)?;
    Ok(RadialGradient {
      shape: shape.unwrap_or_default(),
      position: position.unwrap_or(Position::center()),
      items
    })
  }
}

impl ToCss for RadialGradient {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if self.shape != EndingShape::default() {
      self.shape.to_css(dest)?;
      if self.position.is_center() {
        dest.delim(',', false)?;
      } else {
        dest.write_char(' ')?;
      }
    }
    
    if !self.position.is_center() {
      dest.write_str("at ")?;
      self.position.to_css(dest)?;
      dest.delim(',', false)?;
    }
    
    serialize_items(&self.items, dest)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LineDirection {
  Angle(Angle),
  Horizontal(HorizontalPositionKeyword),
  Vertical(VerticalPositionKeyword),
  Corner(HorizontalPositionKeyword, VerticalPositionKeyword)
}

impl Parse for LineDirection {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(angle) = input.try_parse(Angle::parse) {
      return Ok(LineDirection::Angle(angle))
    }

    input.expect_ident_matching("to")?;

    if let Ok(x) = input.try_parse(HorizontalPositionKeyword::parse) {
      if let Ok(y) = input.try_parse(VerticalPositionKeyword::parse) {
        return Ok(LineDirection::Corner(x, y));
      }
      return Ok(LineDirection::Horizontal(x));
    }

    let y = VerticalPositionKeyword::parse(input)?;
    if let Ok(x) = input.try_parse(HorizontalPositionKeyword::parse) {
      return Ok(LineDirection::Corner(x, y));
    }
    Ok(LineDirection::Vertical(y))
  }
}

impl ToCss for LineDirection {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      LineDirection::Angle(angle) => angle.to_css(dest),
      LineDirection::Horizontal(k) => {
        if dest.minify {
          match k {
            HorizontalPositionKeyword::Left => dest.write_str("270deg"),
            HorizontalPositionKeyword::Right => dest.write_str("90deg"),
          }
        } else {
          dest.write_str("to ")?;
          k.to_css(dest)
        }
      },
      LineDirection::Vertical(k) => {
        if dest.minify {
          match k {
            VerticalPositionKeyword::Top => dest.write_str("0deg"),
            VerticalPositionKeyword::Bottom => dest.write_str("180deg"),
          }
        } else {
          dest.write_str("to ")?;
          k.to_css(dest)
        }
      },
      LineDirection::Corner(x, y) => {
        dest.write_str("to ")?;
        y.to_css(dest)?;
        dest.write_char(' ')?;
        x.to_css(dest)
      }
    }
  }
}

/// https://www.w3.org/TR/css-images-3/#valdef-radial-gradient-ending-shape
#[derive(Debug, Clone, PartialEq)]
pub enum EndingShape {
  Circle(Circle),
  Ellipse(Ellipse)
}

impl Default for EndingShape {
  fn default() -> EndingShape {
    EndingShape::Ellipse(Ellipse::Extent(ShapeExtent::FarthestCorner))
  }
}

impl Parse for EndingShape {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    // Note: Ellipse::parse MUST run before Circle::parse for this to be correct. 
    if let Ok(ellipse) = input.try_parse(Ellipse::parse) {
      return Ok(EndingShape::Ellipse(ellipse))
    }

    if let Ok(circle) = input.try_parse(Circle::parse) {
      return Ok(EndingShape::Circle(circle))
    }
    
    return Err(input.new_error_for_next_token())
  }
}

impl ToCss for EndingShape {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      EndingShape::Circle(circle) => circle.to_css(dest),
      EndingShape::Ellipse(ellipse) => ellipse.to_css(dest),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Circle {
  Radius(Length),
  Extent(ShapeExtent)
}

impl Parse for Circle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
      // The `circle` keyword is required. If it's not there, then it's an ellipse.
      input.expect_ident_matching("circle")?;
      return Ok(Circle::Extent(extent))
    }

    if let Ok(length) = input.try_parse(Length::parse) {
      // The `circle` keyword is optional if there is only a single length.
      // We are assuming here that Ellipse::parse ran first.
      let _ = input.try_parse(|input| input.expect_ident_matching("circle"));
      return Ok(Circle::Radius(length))
    }

    if input.try_parse(|input| input.expect_ident_matching("circle")).is_ok() {
      if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
        return Ok(Circle::Extent(extent))
      }

      if let Ok(length) = input.try_parse(Length::parse) {
        return Ok(Circle::Radius(length))
      }

      // If only the `circle` keyword was given, default to `farthest-corner`.
      return Ok(Circle::Extent(ShapeExtent::FarthestCorner))
    }

    return Err(input.new_error_for_next_token())
  }
}

impl ToCss for Circle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      Circle::Radius(r) => r.to_css(dest),
      Circle::Extent(extent) => {
        dest.write_str("circle")?;
        if *extent != ShapeExtent::FarthestCorner {
          dest.write_char(' ')?;
          extent.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ellipse {
  Size(LengthPercentage, LengthPercentage),
  Extent(ShapeExtent)
}

impl Parse for Ellipse {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
      // The `ellipse` keyword is optional, but only if the `circle` keyword is not present.
      // If it is, then we'll re-parse as a circle.
      if input.try_parse(|input| input.expect_ident_matching("circle")).is_ok() {
        return Err(input.new_error_for_next_token())
      }
      let _ = input.try_parse(|input| input.expect_ident_matching("ellipse"));
      return Ok(Ellipse::Extent(extent))
    }

    if let Ok(x) = input.try_parse(LengthPercentage::parse) {
      let y = LengthPercentage::parse(input)?;
      // The `ellipse` keyword is optional if there are two lengths.
      let _ = input.try_parse(|input| input.expect_ident_matching("ellipse"));
      return Ok(Ellipse::Size(x, y))
    }

    if input.try_parse(|input| input.expect_ident_matching("ellipse")).is_ok() {
      if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
        return Ok(Ellipse::Extent(extent))
      }

      if let Ok(x) = input.try_parse(LengthPercentage::parse) {
        let y = LengthPercentage::parse(input)?;
        return Ok(Ellipse::Size(x, y))
      }

      // Assume `farthest-corner` if only the `ellipse` keyword is present.
      return Ok(Ellipse::Extent(ShapeExtent::FarthestCorner))
    }

    return Err(input.new_error_for_next_token())
  }
}

impl ToCss for Ellipse {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    // The `ellipse` keyword is optional, so we don't emit it.
    match self {
      Ellipse::Size(x, y) => {
        x.to_css(dest)?;
        dest.write_char(' ')?;
        y.to_css(dest)
      },
      Ellipse::Extent(extent) => extent.to_css(dest)
    }
  }
}

enum_property!(ShapeExtent,
  ("closest-side", ClosestSide),
  ("farthest-side", FarthestSide),
  ("closest-corner", ClosestCorner),
  ("farthest-corner", FarthestCorner)
);

/// https://www.w3.org/TR/css-images-4/#color-stop-syntax
#[derive(Debug, Clone, PartialEq)]
pub struct ColorStop {
  color: CssColor,
  position: Option<LengthPercentage>
}

impl Parse for ColorStop {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let color = CssColor::parse(input)?;
    let position = input.try_parse(LengthPercentage::parse).ok();
    Ok(ColorStop {color, position })
  }
}

impl ToCss for ColorStop {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.color.to_css(dest)?;
    if let Some(position) = &self.position {
      dest.write_char(' ')?;
      position.to_css(dest)?;
    }
    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GradientItem {
  ColorStop(ColorStop),
  Hint(LengthPercentage)
}

impl ToCss for GradientItem {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      GradientItem::ColorStop(stop) => stop.to_css(dest),
      GradientItem::Hint(hint) => hint.to_css(dest)
    }
  }
}

fn parse_items<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Vec<GradientItem>, ParseError<'i, ()>> {
  let mut items = Vec::new();
  let mut seen_stop = false;

  loop {
    input.parse_until_before(Delimiter::Comma, |input| {
      if seen_stop {
        if let Ok(hint) = input.try_parse(LengthPercentage::parse) {
          seen_stop = false;
          items.push(GradientItem::Hint(hint));
          return Ok(())
        }
      }

      let stop = ColorStop::parse(input)?;

      if let Ok(position) = input.try_parse(LengthPercentage::parse) {
        let color = stop.color.clone();
        items.push(GradientItem::ColorStop(stop));

        items.push(GradientItem::ColorStop(ColorStop {
          color,
          position: Some(position)
        }))
      } else {
        items.push(GradientItem::ColorStop(stop));
      }

      seen_stop = true;
      Ok(())
    })?;

    match input.next() {
      Err(_) => break,
      Ok(Token::Comma) => continue,
      _ => unreachable!(),
    }
  }

  Ok(items)
}

fn serialize_items<W>(items: &Vec<GradientItem>, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
  let mut first = true;
  let mut last: Option<&GradientItem> = None;
  for item in items {
    // Skip useless hints
    if *item == GradientItem::Hint(LengthPercentage::Percentage(Percentage(0.5))) {
      continue
    }
    
    if let Some(prev) = last {
      match (prev, item) {
        (
          GradientItem::ColorStop(ColorStop { position: Some(_), color: ca }),
          GradientItem::ColorStop(ColorStop { position: Some(p), color: cb })
        ) if ca == cb => {
          dest.write_char(' ')?;
          p.to_css(dest)?;
          last = None;
          continue
        }
        _ => {}
      }
    }
    
    if first {
      first = false;
    } else {
      dest.delim(',', false)?;
    }
    item.to_css(dest)?;
    last = Some(item)
  }
  Ok(())
}
