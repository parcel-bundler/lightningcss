use cssparser::*;
use super::angle::Angle;
use super::position::{HorizontalPositionKeyword, VerticalPositionKeyword};
use super::color::CssColor;
use super::length::LengthPercentage;
use super::percentage::Percentage;
use crate::traits::{Parse, ToCss};
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

#[derive(Debug, Clone, PartialEq)]
pub enum Gradient {
  Linear(LinearGradient),
  RepeatingLinear(LinearGradient)
}

#[derive(Debug, Clone, PartialEq)]
pub struct LinearGradient {
  direction: LineDirection,
  items: Vec<GradientItem>,
}

impl Parse for Gradient {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let func = input.expect_function()?.clone();
    input.parse_nested_block(|input| {
      match_ignore_ascii_case! { &func,
        "linear-gradient" => Ok(Gradient::Linear(Gradient::parse_linear(false, input)?)),
        "repeating-linear-gradient" => Ok(Gradient::RepeatingLinear(Gradient::parse_linear(true, input)?)),
        _ => todo!()
      }
    })
  }
}

impl Gradient {
  fn parse_linear<'i, 't>(repeating: bool, input: &mut Parser<'i, 't>) -> Result<LinearGradient, ParseError<'i, ()>> {
    let direction = if let Ok(direction) = input.try_parse(LineDirection::parse) {
      input.expect_comma()?;
      direction
    } else {
      LineDirection::Vertical(VerticalPositionKeyword::Bottom)
    };
    let items = Self::parse_items(input)?;
    Ok(LinearGradient {
      direction,
      items
    })
  }
}

impl ToCss for Gradient {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let f = match self {
      Gradient::Linear(_) => "linear-gradient(",
      Gradient::RepeatingLinear(_) => "repeating-linear-gradient("
    };

    match self {
      Gradient::Linear(LinearGradient { direction, items }) |
      Gradient::RepeatingLinear(LinearGradient { direction, items }) => {
        dest.write_str(f)?;

        match direction {
          // We can omit `to bottom` or `180deg` because it is the default.
          LineDirection::Vertical(VerticalPositionKeyword::Bottom) |
          LineDirection::Angle(Angle::Deg(180.0)) => serialize_items(items, dest)?,
          // If we have `to top` or `0deg`, and all of the positions and hints are percentages,
          // we can flip the gradient the other direction and omit the direction.
          LineDirection::Vertical(VerticalPositionKeyword::Top) |
          LineDirection::Angle(Angle::Deg(0.0)) if dest.minify && items.iter().all(|item| matches!(item, GradientItem::Hint(LengthPercentage::Percentage(_)) | GradientItem::ColorStop(ColorStop { position: None | Some(LengthPercentage::Percentage(_)), .. }))) => {
            let items = items.iter().rev().map(|item| {
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
            serialize_items(&items, dest)?;
          },
          _ => {
            if *direction != LineDirection::Vertical(VerticalPositionKeyword::Bottom) && *direction != LineDirection::Angle(Angle::Deg(180.0)) {
              direction.to_css(dest)?;
              dest.delim(',', false)?;
            }

            serialize_items(items, dest)?;
          }
        }
      }
    }

    dest.write_char(')')
  }
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

impl Gradient {
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
