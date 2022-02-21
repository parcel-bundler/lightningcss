use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;
use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};

use super::percentage::Percentage;

#[derive(Debug, Clone, PartialEq)]
pub enum CssColor {
  CurrentColor,
  RGBA(RGBA),
  Lab(f32, f32, f32, f32),
  Lch(f32, f32, f32, f32),
  Oklab(f32, f32, f32, f32),
  Oklch(f32, f32, f32, f32)
}

impl CssColor {
  pub fn current_color() -> CssColor {
    CssColor::CurrentColor
  }

  pub fn transparent() -> CssColor {
    CssColor::RGBA(RGBA::transparent())
  }
}

impl Default for CssColor {
  fn default() -> CssColor {
    CssColor::transparent()
  }
}

impl From<Color> for CssColor {
  fn from(color: Color) -> Self {
    match color {
      Color::CurrentColor => CssColor::CurrentColor,
      Color::RGBA(rgba) => CssColor::RGBA(rgba)
    }
  }
}

impl<'i> Parse<'i> for CssColor {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(color) = input.try_parse(Color::parse) {
      return Ok(color.into())
    }

    parse_color_function(input)
  }
}

impl ToCss for CssColor {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      CssColor::CurrentColor => dest.write_str("currentColor"),
      CssColor::RGBA(color) => {
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
      },
      CssColor::Lab(l, a, b, alpha) => write_components("lab", *l, *a, *b, *alpha, dest),
      CssColor::Lch(l, c, h, alpha) => write_components("lch", *l, *c, *h, *alpha, dest),
      CssColor::Oklab(l, a, b, alpha) => write_components("oklab", *l, *a, *b, *alpha, dest),
      CssColor::Oklch(l, c, h, alpha) => write_components("oklch", *l, *c, *h, *alpha, dest)
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

struct ComponentParser;
impl<'i> ColorComponentParser<'i> for ComponentParser {
  type Error = ParserError<'i>;
}

// https://www.w3.org/TR/css-color-4/#lab-colors
fn parse_color_function<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CssColor, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  let function = input.expect_function()?;

  match_ignore_ascii_case! {&*function,
    "lab" => {
      let (l, c, h, alpha) = parse_lab(input)?;
      Ok(CssColor::Lab(l, c, h, alpha))
    },
    "oklab" => {
      let (l, c, h, alpha) = parse_lab(input)?;
      Ok(CssColor::Oklab(l, c, h, alpha))
    },
    "lch" => {
      let (l, c, h, alpha) = parse_lch(input)?;
      Ok(CssColor::Lch(l, c, h, alpha))
    },
    "oklch" => {
      let (l, c, h, alpha) = parse_lch(input)?;
      Ok(CssColor::Oklch(l, c, h, alpha))
    },
    _ => Err(location.new_unexpected_token_error(
      cssparser::Token::Ident(function.clone())
    ))
  }
}

/// Parses the lab() and oklab() functions.
#[inline]
fn parse_lab<'i, 't>(input: &mut Parser<'i, 't>) -> Result<(f32, f32, f32, f32), ParseError<'i, ParserError<'i>>> {
  // https://www.w3.org/TR/css-color-4/#funcdef-lab
  let res = input.parse_nested_block(|input| {
    let l = input.expect_percentage()?;
    let a = input.expect_number()?;
    let b = input.expect_number()?;
    let alpha = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      parse_number_or_percentage(input)?
    } else {
      1.0
    };

    Ok((l, a, b, alpha))
  })?;

  Ok(res)
}

/// Parses the lch() and oklch() functions.
#[inline]
fn parse_lch<'i, 't>(input: &mut Parser<'i, 't>) -> Result<(f32, f32, f32, f32), ParseError<'i, ParserError<'i>>> {
  // https://www.w3.org/TR/css-color-4/#funcdef-lch
  let parser = ComponentParser;
  let res = input.parse_nested_block(|input| {
    let l = input.expect_percentage()?;
    let c = input.expect_number()?;
    let h = match parser.parse_angle_or_number(input)? {
      AngleOrNumber::Number { value } => value,
      AngleOrNumber::Angle { degrees } => degrees
    };
    let alpha = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      parse_number_or_percentage(input)?
    } else {
      1.0
    };

    Ok((l, c, h, alpha))
  })?;

  Ok(res)
}

#[inline]
fn parse_number_or_percentage<'i, 't>(input: &mut Parser<'i, 't>) -> Result<f32, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  Ok(match *input.next()? {
    Token::Number { value, .. } => value,
    Token::Percentage { unit_value, .. } => unit_value,
    ref t => return Err(location.new_unexpected_token_error(t.clone())),
  })
}

#[inline]
fn write_components<W>(name: &str, a: f32, b: f32, c: f32, alpha: f32, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
  dest.write_str(name)?;
  dest.write_char('(')?;
  Percentage(a).to_css(dest)?;
  dest.write_char(' ')?;
  b.to_css(dest)?;
  dest.write_char(' ')?;
  c.to_css(dest)?;
  if alpha != 1.0 {
    dest.delim('/', true)?;
    alpha.to_css(dest)?;
  }

  dest.write_char(')')
}
