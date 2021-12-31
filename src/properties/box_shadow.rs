use cssparser::*;
use crate::values::length::Length;
use crate::traits::{Parse, ToCss};
use crate::values::color::CssColor;
use crate::printer::Printer;
use crate::error::ParserError;

#[derive(Debug, Clone, PartialEq)]
pub struct BoxShadow {
  pub color: CssColor,
  pub x_offset: Length,
  pub y_offset: Length,
  pub blur: Length,
  pub spread: Length,
  pub inset: bool
}

impl Parse for BoxShadow {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color = None;
    let mut lengths = None;
    let mut inset = false;

    loop {
      if !inset {
        if input.try_parse(|input| input.expect_ident_matching("inset")).is_ok(){
          inset = true;
          continue;
        }
      }

      if lengths.is_none() {
        let value = input.try_parse::<_, _, ParseError<ParserError<'i>>>(|input| {
          let horizontal = Length::parse(input)?;
          let vertical = Length::parse(input)?;
          let blur = input.try_parse(Length::parse).unwrap_or(Length::zero());
          let spread = input.try_parse(Length::parse).unwrap_or(Length::zero());
          Ok((horizontal, vertical, blur, spread))
        });

        if let Ok(value) = value {
          lengths = Some(value);
          continue;
        }
      }

      if color.is_none() {
        if let Ok(value) = input.try_parse(CssColor::parse) {
          color = Some(value);
          continue;
        }
      }

      break
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(BoxShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2,
      spread: lengths.3,
      inset
    })
  }
}

impl ToCss for BoxShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if self.inset {
      dest.write_str("inset ")?;
    }

    self.x_offset.to_css(dest)?;
    dest.write_char(' ')?;
    self.y_offset.to_css(dest)?;
    
    if self.blur != Length::zero() || self.spread != Length::zero() {
      dest.write_char(' ')?;
      self.blur.to_css(dest)?;

      if self.spread != Length::zero() {
        dest.write_char(' ')?;
        self.spread.to_css(dest)?;
      }  
    }

    if self.color != CssColor::current_color() {
      dest.write_char(' ')?;
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}
