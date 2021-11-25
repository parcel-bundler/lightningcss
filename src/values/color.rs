use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::fmt::Write;
use crate::compat::Feature;

#[derive(Debug, Clone, PartialEq)]
pub struct CssColor(Color);

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
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(color) = input.try_parse(Color::parse) {
      return Ok(CssColor(color))
    }

    if let Ok(color) = input.try_parse(parse_hwb) {
      return Ok(CssColor(color))
    }

    Err(input.new_error_for_next_token())
  }
}

struct DefaultComponentParser;
impl<'i> ColorComponentParser<'i> for DefaultComponentParser {
  type Error = ();
}

fn parse_hwb<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Color, ParseError<'i, ()>> {
  input.expect_function_matching("hwb")?;

  let component_parser = DefaultComponentParser;
  input.parse_nested_block(|input| {
    parse_rgb_components_hwb(&component_parser, input)
  })
}

#[inline]
fn parse_rgb_components_hwb<'i, 't, ComponentParser>(
  component_parser: &ComponentParser,
  arguments: &mut Parser<'i, 't>,
) -> Result<Color, ParseError<'i, ComponentParser::Error>>
where
  ComponentParser: ColorComponentParser<'i>,
{
  let hue_degrees = degrees(component_parser.parse_angle_or_number(arguments)?);

  // Subtract an integer before rounding, to avoid some rounding errors:
  let hue_normalized_degrees = hue_degrees - 360. * (hue_degrees / 360.).floor();
  let hue = hue_normalized_degrees / 360.;

  let uses_commas = arguments.try_parse(|i| i.expect_comma()).is_ok();

  let whiteness = component_parser.parse_percentage(arguments)?;
  let whiteness = whiteness.max(0.).min(1.);

  if uses_commas {
    arguments.expect_comma()?;
  }

  let blackness = component_parser.parse_percentage(arguments)?;
  let blackness = blackness.max(0.).min(1.);

  let (r, g, b) = hwb_to_rgb(hue, whiteness, blackness);

  let alpha = if !arguments.is_exhausted() {
    if uses_commas {
      arguments.expect_comma()?;
    } else {
      arguments.expect_delim('/')?;
    };
    unit_value(component_parser.parse_number_or_percentage(arguments)?)
  } else {
    1.0
  };

  arguments.expect_exhausted()?;
  Ok(Color::RGBA(RGBA::from_floats(r, g, b, alpha)))
}

fn unit_value(val: NumberOrPercentage) -> f32 {
  match val {
    NumberOrPercentage::Number { value } => value,
    NumberOrPercentage::Percentage { unit_value } => unit_value,
  }
}

fn degrees(val: AngleOrNumber) -> f32 {
  match val {
    AngleOrNumber::Number { value } => value,
    AngleOrNumber::Angle { degrees } => degrees,
  }
}

/// https://drafts.csswg.org/css-color-4/#hwb-to-rgb
fn hwb_to_rgb(h: f32, w: f32, b: f32) -> (f32, f32, f32) {
  if w + b >= 1.0 {
    let gray = w / (w + b);
    return (gray, gray, gray)
  }

  let (mut red, mut green, mut blue) = hsl_to_rgb(h, 1.0, 0.5);
  let x = 1.0 - w - b;
  red = red * x + w;
  green = green * x + w;
  blue = blue * x + w;
  (red, green, blue)
}

fn hsl_to_rgb(hue: f32, saturation: f32, lightness: f32) -> (f32, f32, f32) {
  // https://drafts.csswg.org/css-color/#hsl-color
  // except with h pre-multiplied by 3, to avoid some rounding errors.
  fn hue_to_rgb(m1: f32, m2: f32, mut h3: f32) -> f32 {
    if h3 < 0. {
      h3 += 3.
    }
    if h3 > 3. {
      h3 -= 3.
    }

    if h3 * 2. < 1. {
      m1 + (m2 - m1) * h3 * 2.
    } else if h3 * 2. < 3. {
      m2
    } else if h3 < 2. {
      m1 + (m2 - m1) * (2. - h3) * 2.
    } else {
      m1
    }
  }
  let m2 = if lightness <= 0.5 {
    lightness * (saturation + 1.)
  } else {
    lightness + saturation - lightness * saturation
  };
  let m1 = lightness * 2. - m2;
  let hue_times_3 = hue * 3.;
  let red = hue_to_rgb(m1, m2, hue_times_3 + 1.);
  let green = hue_to_rgb(m1, m2, hue_times_3);
  let blue = hue_to_rgb(m1, m2, hue_times_3 - 1.);
  (red, green, blue)
}

impl ToCss for CssColor {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
