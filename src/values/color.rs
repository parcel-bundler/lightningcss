use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;
use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};

#[derive(Debug, Clone, PartialEq)]
pub struct CssColor(pub Color);

impl CssColor {
  pub fn current_color() -> CssColor {
    CssColor(Color::CurrentColor)
  }

  pub fn transparent() -> CssColor {
    CssColor(Color::RGBA(RGBA::transparent()))
  }
}

impl Default for CssColor {
  fn default() -> CssColor {
    CssColor::transparent()
  }
}

impl Parse for CssColor {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(color) = input.try_parse(Color::parse) {
      return Ok(CssColor(color))
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for CssColor {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self.0 {
      Color::CurrentColor => dest.write_str("currentColor"),
      Color::RGBA(color) => {
        if color.alpha == 255 {
          let hex: u32 = ((color.red as u32) << 16) | ((color.green as u32) << 8) | (color.blue as u32);
          if let Some(name) = short_color_name(hex) {
            return dest.write_str(name)
          }

          let compact = compact_hex(hex);
          if hex == expand_hex(compact) {
            write!(dest, "#{:03x}", compact)?;
          } else {
            write!(dest, "#{:06x}", hex)?;
          }
        } else {
          // If the #rrggbbaa syntax is not supported by the browser targets, output rgba()
          if let Some(targets) = dest.targets {
            if !Feature::CssRrggbbaa.is_compatible(targets) {
              dest.write_str("rgba(")?;
              write!(dest, "{}", color.red)?;
              dest.delim(',', false)?;
              write!(dest, "{}", color.green)?;
              dest.delim(',', false)?;
              write!(dest, "{}", color.blue)?;
              dest.delim(',', false)?;

              // Try first with two decimal places, then with three.
              let mut rounded_alpha = (color.alpha_f32() * 100.0).round() / 100.0;
              let clamped = (rounded_alpha * 255.0).round().max(0.).min(255.0) as u8;
              if clamped != color.alpha {
                rounded_alpha = (color.alpha_f32() * 1000.).round() / 1000.;
              }

              rounded_alpha.to_css(dest)?;
              dest.write_char(')')?;
              return Ok(())
            }
          }
  
          let hex: u32 = ((color.red as u32) << 24) | ((color.green as u32) << 16) | ((color.blue as u32) << 8) | (color.alpha as u32);
          let compact = compact_hex(hex);
          if hex == expand_hex(compact) {
            write!(dest, "#{:04x}", compact)?;
          } else {
            write!(dest, "#{:08x}", hex)?;
          }
        }
        Ok(())
      }
    }
  }
}

// From esbuild: https://github.com/evanw/esbuild/blob/18e13bdfdca5cd3c7a2fae1a8bd739f8f891572c/internal/css_parser/css_decls_color.go#L218
// 0xAABBCCDD => 0xABCD
fn compact_hex(v: u32) -> u32 {
	return ((v & 0x0FF00000) >> 12) | ((v & 0x00000FF0) >> 4)
}

// 0xABCD => 0xAABBCCDD
fn expand_hex(v: u32) -> u32 {
	return ((v & 0xF000) << 16) | ((v & 0xFF00) << 12) | ((v & 0x0FF0) << 8) | ((v & 0x00FF) << 4) | (v & 0x000F)
}

fn short_color_name(v: u32) -> Option<&'static str> {
  // These names are shorter than their hex codes
  let s = match v {
    0x000080 => "navy",
    0x008000 => "green",
    0x008080 => "teal",
    0x4b0082 => "indigo",
    0x800000 => "maroon",
    0x800080 => "purple",
    0x808000 => "olive",
    0x808080 => "gray",
    0xa0522d => "sienna",
    0xa52a2a => "brown",
    0xc0c0c0 => "silver",
    0xcd853f => "peru",
    0xd2b48c => "tan",
    0xda70d6 => "orchid",
    0xdda0dd => "plum",
    0xee82ee => "violet",
    0xf0e68c => "khaki",
    0xf0ffff => "azure",
    0xf5deb3 => "wheat",
    0xf5f5dc => "beige",
    0xfa8072 => "salmon",
    0xfaf0e6 => "linen",
    0xff0000 => "red",
    0xff6347 => "tomato",
    0xff7f50 => "coral",
    0xffa500 => "orange",
    0xffc0cb => "pink",
    0xffd700 => "gold",
    0xffe4c4 => "bisque",
    0xfffafa => "snow",
    0xfffff0 => "ivory",
    _ => return None
  };

  Some(s)
}
