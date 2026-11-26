//! CSS position values.

use super::length::LengthPercentage;
use super::percentage::Percentage;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{IsCompatible, Parse, ToCss, Zero};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

#[cfg(feature = "serde")]
use crate::serialization::ValueWrapper;

/// A CSS [`<position>`](https://www.w3.org/TR/css3-values/#position) value,
/// as used in the `background-position` property, gradients, masks, etc.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct Position {
  /// The x-position.
  pub x: HorizontalPosition,
  /// The y-position.
  pub y: VerticalPosition,
}

impl Position {
  /// Returns a `Position` with both the x and y set to `center`.
  pub fn center() -> Position {
    Position {
      x: HorizontalPosition::Center,
      y: VerticalPosition::Center,
    }
  }

  /// Returns whether both the x and y positions are centered.
  pub fn is_center(&self) -> bool {
    self.x.is_center() && self.y.is_center()
  }

  /// Returns whether both the x and y positions are zero.
  pub fn is_zero(&self) -> bool {
    self.x.is_zero() && self.y.is_zero()
  }
}

impl Default for Position {
  fn default() -> Position {
    Position {
      x: HorizontalPosition::Length(LengthPercentage::Percentage(Percentage(0.0))),
      y: VerticalPosition::Length(LengthPercentage::Percentage(Percentage(0.0))),
    }
  }
}

impl<'i> Parse<'i> for Position {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(HorizontalPosition::parse) {
      Ok(HorizontalPosition::Center) => {
        // Try parsing a vertical position next.
        if let Ok(y) = input.try_parse(VerticalPosition::parse) {
          return Ok(Position {
            x: HorizontalPosition::Center,
            y,
          });
        }

        // If it didn't work, assume the first actually represents a y position,
        // and the next is an x position. e.g. `center left` rather than `left center`.
        let x = input.try_parse(HorizontalPosition::parse).unwrap_or(HorizontalPosition::Center);
        let y = VerticalPosition::Center;
        return Ok(Position { x, y });
      }
      Ok(x @ HorizontalPosition::Length(_)) => {
        // If we got a length as the first component, then the second must
        // be a keyword or length (not a side offset).
        if let Ok(y_keyword) = input.try_parse(VerticalPositionKeyword::parse) {
          let y = VerticalPosition::Side {
            side: y_keyword,
            offset: None,
          };
          return Ok(Position { x, y });
        }
        if let Ok(y_lp) = input.try_parse(LengthPercentage::parse) {
          let y = VerticalPosition::Length(y_lp);
          return Ok(Position { x, y });
        }
        let y = VerticalPosition::Center;
        let _ = input.try_parse(|i| i.expect_ident_matching("center"));
        return Ok(Position { x, y });
      }
      Ok(HorizontalPosition::Side {
        side: x_keyword,
        offset: lp,
      }) => {
        // If we got a horizontal side keyword (and optional offset), expect another for the vertical side.
        // e.g. `left center` or `left 20px center`
        if input.try_parse(|i| i.expect_ident_matching("center")).is_ok() {
          let x = HorizontalPosition::Side {
            side: x_keyword,
            offset: lp,
          };
          let y = VerticalPosition::Center;
          return Ok(Position { x, y });
        }

        // e.g. `left top`, `left top 20px`, `left 20px top`, or `left 20px top 20px`
        if let Ok(y_keyword) = input.try_parse(VerticalPositionKeyword::parse) {
          let y_lp = input.try_parse(LengthPercentage::parse).ok();
          let x = HorizontalPosition::Side {
            side: x_keyword,
            offset: lp,
          };
          let y = VerticalPosition::Side {
            side: y_keyword,
            offset: y_lp,
          };
          return Ok(Position { x, y });
        }

        // If we didn't get a vertical side keyword (e.g. `left 20px`), then apply the offset to the vertical side.
        let x = HorizontalPosition::Side {
          side: x_keyword,
          offset: None,
        };
        let y = lp.map_or(VerticalPosition::Center, VerticalPosition::Length);
        return Ok(Position { x, y });
      }
      _ => {}
    }

    // If the horizontal position didn't parse, then it must be out of order. Try vertical position keyword.
    let y_keyword = VerticalPositionKeyword::parse(input)?;
    let lp_and_x_pos: Result<_, ParseError<()>> = input.try_parse(|i| {
      let y_lp = i.try_parse(LengthPercentage::parse).ok();
      if let Ok(x_keyword) = i.try_parse(HorizontalPositionKeyword::parse) {
        let x_lp = i.try_parse(LengthPercentage::parse).ok();
        let x_pos = HorizontalPosition::Side {
          side: x_keyword,
          offset: x_lp,
        };
        return Ok((y_lp, x_pos));
      }
      i.expect_ident_matching("center")?;
      let x_pos = HorizontalPosition::Center;
      Ok((y_lp, x_pos))
    });

    if let Ok((y_lp, x)) = lp_and_x_pos {
      let y = VerticalPosition::Side {
        side: y_keyword,
        offset: y_lp,
      };
      return Ok(Position { x, y });
    }

    let x = HorizontalPosition::Center;
    let y = VerticalPosition::Side {
      side: y_keyword,
      offset: None,
    };
    Ok(Position { x, y })
  }
}

impl ToCss for Position {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match (&self.x, &self.y) {
      (x_pos @ &HorizontalPosition::Side { side, offset: Some(_) }, &VerticalPosition::Length(ref y_lp))
        if side != HorizontalPositionKeyword::Left =>
      {
        x_pos.to_css(dest)?;
        dest.write_str(" top ")?;
        y_lp.to_css(dest)
      }
      (x_pos @ &HorizontalPosition::Side { side, offset: Some(_) }, y)
        if side != HorizontalPositionKeyword::Left && y.is_center() =>
      {
        // If there is a side keyword with an offset, "center" must be a keyword not a percentage.
        x_pos.to_css(dest)?;
        dest.write_str(" center")
      }
      (&HorizontalPosition::Length(ref x_lp), y_pos @ &VerticalPosition::Side { side, offset: Some(_) })
        if side != VerticalPositionKeyword::Top =>
      {
        dest.write_str("left ")?;
        x_lp.to_css(dest)?;
        dest.write_str(" ")?;
        y_pos.to_css(dest)
      }
      (x, y) if x.is_center() && y.is_center() => {
        // `center center` => 50%
        x.to_css(dest)
      }
      (&HorizontalPosition::Length(ref x_lp), y) if y.is_center() => {
        // `center` is assumed if omitted.
        x_lp.to_css(dest)
      }
      (
        &HorizontalPosition::Side {
          side: HorizontalPositionKeyword::Left,
          offset: Some(ref x_lp),
        },
        y,
      ) if y.is_center() => {
        // `left 10px center` => `10px` (omit Y when center)
        x_lp.to_css(dest)
      }
      (&HorizontalPosition::Side { side, offset: None }, y) if y.is_center() => {
        let p: LengthPercentage = side.into();
        p.to_css(dest)
      }
      (x, y_pos @ &VerticalPosition::Side { offset: None, .. }) if x.is_center() => y_pos.to_css(dest),
      (
        &HorizontalPosition::Center,
        y_pos @ &VerticalPosition::Side {
          side: VerticalPositionKeyword::Bottom,
          offset: Some(_),
        },
      ) => {
        // `center bottom 10px` must keep the keyword form
        dest.write_str("center ")?;
        y_pos.to_css(dest)
      }
      (&HorizontalPosition::Side { side: x, offset: None }, &VerticalPosition::Side { side: y, offset: None }) => {
        let x: LengthPercentage = x.into();
        let y: LengthPercentage = y.into();
        x.to_css(dest)?;
        dest.write_str(" ")?;
        y.to_css(dest)
      }
      (x_pos, y_pos) => {
        let zero = LengthPercentage::zero();
        let fifty = LengthPercentage::Percentage(Percentage(0.5));
        let x_len = match &x_pos {
          HorizontalPosition::Side {
            side: HorizontalPositionKeyword::Left,
            offset,
          } => {
            if let Some(len) = offset {
              if len.is_zero() {
                Some(&zero)
              } else {
                Some(len)
              }
            } else {
              Some(&zero)
            }
          }
          HorizontalPosition::Length(len) if len.is_zero() => Some(&zero),
          HorizontalPosition::Length(len) => Some(len),
          HorizontalPosition::Center => Some(&fifty),
          _ => None,
        };

        let y_len = match &y_pos {
          VerticalPosition::Side {
            side: VerticalPositionKeyword::Top,
            offset,
          } => {
            if let Some(len) = offset {
              if len.is_zero() {
                Some(&zero)
              } else {
                Some(len)
              }
            } else {
              Some(&zero)
            }
          }
          VerticalPosition::Length(len) if len.is_zero() => Some(&zero),
          VerticalPosition::Length(len) => Some(len),
          VerticalPosition::Center => Some(&fifty),
          _ => None,
        };

        if let (Some(x), Some(y)) = (x_len, y_len) {
          x.to_css(dest)?;
          dest.write_str(" ")?;
          y.to_css(dest)
        } else {
          x_pos.to_css(dest)?;
          dest.write_str(" ")?;
          y_pos.to_css(dest)
        }
      }
    }
  }
}

impl IsCompatible for Position {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

/// A component within a [Position](Position) value, representing a position
/// along either the horizontal or vertical axis of a box.
///
/// This type is generic over side keywords.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum PositionComponent<S> {
  /// The `center` keyword.
  Center,
  /// A length or percentage from the top-left corner of the box.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LengthPercentage>"))]
  Length(LengthPercentage),
  /// A side keyword with an optional offset.
  Side {
    /// A side keyword.
    side: S,
    /// Offset from the side.
    offset: Option<LengthPercentage>,
  },
}

impl<S> PositionComponent<S> {
  fn is_center(&self) -> bool {
    match self {
      PositionComponent::Center => true,
      PositionComponent::Length(LengthPercentage::Percentage(Percentage(p))) => *p == 0.5,
      _ => false,
    }
  }

  fn is_zero(&self) -> bool {
    matches!(self, PositionComponent::Length(len) if len.is_zero())
  }
}

impl<'i, S: Parse<'i>> Parse<'i> for PositionComponent<S> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("center")).is_ok() {
      return Ok(PositionComponent::Center);
    }

    if let Ok(lp) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(PositionComponent::Length(lp));
    }

    let side = S::parse(input)?;
    let offset = input.try_parse(|input| LengthPercentage::parse(input)).ok();
    Ok(PositionComponent::Side { side, offset })
  }
}

impl<S: ToCss> ToCss for PositionComponent<S> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use PositionComponent::*;
    match &self {
      Center => {
        if dest.minify {
          dest.write_str("50%")
        } else {
          dest.write_str("center")
        }
      }
      Length(lp) => lp.to_css(dest),
      Side { side, offset } => {
        side.to_css(dest)?;
        if let Some(lp) = offset {
          dest.write_str(" ")?;
          lp.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

enum_property! {
  /// A horizontal position keyword.
  pub enum HorizontalPositionKeyword {
    /// The `left` keyword.
    Left,
    /// The `right` keyword.
    Right,
  }
}

impl Into<LengthPercentage> for HorizontalPositionKeyword {
  fn into(self) -> LengthPercentage {
    match self {
      HorizontalPositionKeyword::Left => LengthPercentage::zero(),
      HorizontalPositionKeyword::Right => LengthPercentage::Percentage(Percentage(1.0)),
    }
  }
}

enum_property! {
  /// A vertical position keyword.
  pub enum VerticalPositionKeyword {
    /// The `top` keyword.
    Top,
    /// The `bottom` keyword.
    Bottom,
  }
}

impl Into<LengthPercentage> for VerticalPositionKeyword {
  fn into(self) -> LengthPercentage {
    match self {
      VerticalPositionKeyword::Top => LengthPercentage::zero(),
      VerticalPositionKeyword::Bottom => LengthPercentage::Percentage(Percentage(1.0)),
    }
  }
}

/// A horizontal position component.
pub type HorizontalPosition = PositionComponent<HorizontalPositionKeyword>;

/// A vertical position component.
pub type VerticalPosition = PositionComponent<VerticalPositionKeyword>;
