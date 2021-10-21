use cssparser::*;
use crate::values::length::{HorizontalPosition, HorizontalPositionKeyword, VerticalPosition, VerticalPositionKeyword, LengthPercentage};
use crate::values::traits::{Parse, ToCss};

/// https://www.w3.org/TR/css-backgrounds-3/#background-position
#[derive(Debug, Clone, PartialEq)]
pub struct BackgroundPosition {
  x: HorizontalPosition,
  y: VerticalPosition
}

impl Parse for BackgroundPosition {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    match input.try_parse(HorizontalPosition::parse) {
      Ok(HorizontalPosition::Center) => {
        // Try parsing a vertical position next.
        if let Ok(y) = input.try_parse(VerticalPosition::parse) {
          return Ok(BackgroundPosition {
            x: HorizontalPosition::Center,
            y
          })
        }

        // If it didn't work, assume the first actually represents a y position,
        // and the next is an x position. e.g. `center left` rather than `left center`.
        let x = input
          .try_parse(HorizontalPosition::parse)
          .unwrap_or(HorizontalPosition::Center);
        let y = VerticalPosition::Center;
        return Ok(BackgroundPosition { x, y })
      },
      Ok(x @ HorizontalPosition::Length(_)) => {
        // If we got a length as the first component, then the second must 
        // be a keyword or length (not a side offset).
        if let Ok(y_keyword) = input.try_parse(VerticalPositionKeyword::parse) {
          let y = VerticalPosition::Side(y_keyword, None);
          return Ok(BackgroundPosition { x, y });
        }
        if let Ok(y_lp) = input.try_parse(LengthPercentage::parse) {
            let y = VerticalPosition::Length(y_lp);
            return Ok(BackgroundPosition { x, y });
        }
        let y = VerticalPosition::Center;
        let _ = input.try_parse(|i| i.expect_ident_matching("center"));
        return Ok(BackgroundPosition { x, y });
      }
      Ok(HorizontalPosition::Side(x_keyword, lp)) => {
        // If we got a horizontal side keyword (and optional offset), expect another for the vertical side.
        // e.g. `left center` or `left 20px center`
        if input.try_parse(|i| i.expect_ident_matching("center")).is_ok() {
          let x = HorizontalPosition::Side(x_keyword, lp);
          let y = VerticalPosition::Center;
          return Ok(BackgroundPosition { x, y });
        }

        // e.g. `left top`, `left top 20px`, `left 20px top`, or `left 20px top 20px`
        if let Ok(y_keyword) = input.try_parse(VerticalPositionKeyword::parse) {
          let y_lp = input.try_parse(LengthPercentage::parse).ok();
          let x = HorizontalPosition::Side(x_keyword, lp);
          let y = VerticalPosition::Side(y_keyword, y_lp);
          return Ok(BackgroundPosition { x, y });
        }

        // If we didn't get a vertical side keyword (e.g. `left 20px`), then apply the offset to the vertical side.
        let x = HorizontalPosition::Side(x_keyword, None);
        let y = lp.map_or(VerticalPosition::Center, VerticalPosition::Length);
        return Ok(BackgroundPosition { x, y });
      }
      _ => {}
    }

    // If the horizontal position didn't parse, then it must be out of order. Try vertical position keyword.
    let y_keyword = VerticalPositionKeyword::parse(input)?;
    let lp_and_x_pos: Result<_, ParseError<()>> = input.try_parse(|i| {
      let y_lp = i.try_parse(LengthPercentage::parse).ok();
      if let Ok(x_keyword) = i.try_parse(HorizontalPositionKeyword::parse) {
        let x_lp = i.try_parse(LengthPercentage::parse).ok();
        let x_pos = HorizontalPosition::Side(x_keyword, x_lp);
        return Ok((y_lp, x_pos));
      }
      i.expect_ident_matching("center")?;
      let x_pos = HorizontalPosition::Center;
      Ok((y_lp, x_pos))
    });

    if let Ok((y_lp, x)) = lp_and_x_pos {
      let y = VerticalPosition::Side(y_keyword, y_lp);
      return Ok(BackgroundPosition { x, y });
    }

    let x = HorizontalPosition::Center;
    let y = VerticalPosition::Side(y_keyword, None);
    Ok(BackgroundPosition { x, y })
  }
}

impl ToCss for BackgroundPosition {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    match (&self.x, &self.y) {
      (
        x_pos @ &HorizontalPosition::Side(_, Some(_)),
        &VerticalPosition::Length(ref y_lp),
      ) => {
        x_pos.to_css(dest)?;
        dest.write_str(" top ")?;
        y_lp.to_css(dest)
      },
      (
        &HorizontalPosition::Length(ref x_lp),
        y_pos @ &VerticalPosition::Side(_, Some(_)),
      ) => {
        dest.write_str("left ")?;
        x_lp.to_css(dest)?;
        dest.write_str(" ")?;
        y_pos.to_css(dest)
      },
      (x_pos, y_pos) => {
        x_pos.to_css(dest)?;
        dest.write_str(" ")?;
        y_pos.to_css(dest)
      },
    }
  }
}
