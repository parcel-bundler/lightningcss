use cssparser::*;
use bitflags::bitflags;
use crate::rules::supports::SupportsCondition;
use crate::targets::Browsers;
use crate::traits::{Parse, ToCss, FallbackValues};
use crate::printer::Printer;
use std::f32::consts::PI;
use std::fmt::Write;
use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};
use super::percentage::Percentage;
use crate::macros::enum_property;

#[derive(Debug, Clone, PartialEq)]
pub enum CssColor {
  CurrentColor,
  RGBA(RGBA),
  Lab(Box<LabColor>),
  Predefined(Box<PredefinedColor>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum LabColor {
  Lab(f32, f32, f32, f32),
  Lch(f32, f32, f32, f32),
  Oklab(f32, f32, f32, f32),
  Oklch(f32, f32, f32, f32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PredefinedColor {
  Srgb(f32, f32, f32, f32),
  SrgbLinear(f32, f32, f32, f32),
  DisplayP3(f32, f32, f32, f32),
  A98Rgb(f32, f32, f32, f32),
  ProphotoRgb(f32, f32, f32, f32),
  Rec2020(f32, f32, f32, f32),
  XyzD50(f32, f32, f32, f32),
  XyzD65(f32, f32, f32, f32)
}

bitflags! {
  pub struct ColorFallbackKind: u8 {
    const RGB    = 0b01;
    const P3     = 0b10;
    const LAB    = 0b100;
  }
}

enum_property! {
  enum ColorSpace {
    "srgb": Srgb,
    "srgb-linear": SrgbLinear,
    "lab": Lab,
    "oklab": Oklab,
    "xyz": Xyz,
    "xyz-d50": XyzD50,
    "xyz-d65": XyzD65,
    "hsl": Hsl,
    "hwb": Hwb,
    "lch": Lch,
    "oklch": Oklch,
  }
}

enum_property! {
  enum HueInterpolationMethod {
    Shorter,
    Longer,
    Increasing,
    Decreasing,
    Specified,
  }
}

impl ColorFallbackKind {
  pub fn lowest(&self) -> ColorFallbackKind {
    // This finds the lowest set bit.
    *self & ColorFallbackKind::from_bits_truncate(self.bits().wrapping_neg())
  }

  pub fn supports_condition<'i>(&self) -> SupportsCondition<'i> {
    let s = match *self {
      ColorFallbackKind::P3 => "color: color(display-p3 0 0 0)",
      ColorFallbackKind::LAB => "color: lab(0% 0 0)",
      _ => unreachable!()
    };

    SupportsCondition::Declaration(s.into())
  }
}

impl CssColor {
  pub fn current_color() -> CssColor {
    CssColor::CurrentColor
  }

  pub fn transparent() -> CssColor {
    CssColor::RGBA(RGBA::transparent())
  }

  fn to_xyza_d65(&self) -> (f32, f32, f32, f32) {
    match self {
      CssColor::RGBA(rgba) => {
        let (x, y, z) = srgb_to_xyz_d65(rgba.red_f32(), rgba.green_f32(), rgba.blue_f32());
        (x, y, z, rgba.alpha_f32())
      }
      CssColor::Lab(lab) => lab.to_xyza_d65(),
      CssColor::Predefined(predefined) => predefined.to_xyza_d65(),
      CssColor::CurrentColor => unreachable!()
    }
  }

  fn to_color_space(&self, space: ColorSpace) -> Result<(f32, f32, f32, f32), ()> {
    match self {
      CssColor::CurrentColor => return Err(()),
      CssColor::RGBA(rgba) if space == ColorSpace::Srgb => {
        match space {
          ColorSpace::Srgb => {
            return Ok((rgba.red_f32(), rgba.green_f32(), rgba.blue_f32(), rgba.alpha_f32()))
          }
          ColorSpace::SrgbLinear => {
            let (r, g, b) = lin_srgb(rgba.red_f32(), rgba.green_f32(), rgba.blue_f32());
            return Ok((r, g, b, rgba.alpha_f32()))
          }
          ColorSpace::Hsl => {
            let (h, s, l) = rgb_to_hsl(rgba.red_f32(), rgba.green_f32(), rgba.blue_f32());
            return Ok((h, s, l, rgba.alpha_f32()))
          }
          ColorSpace::Hwb => {
            let (h, w, b) = rgb_to_hwb(rgba.red_f32(), rgba.green_f32(), rgba.blue_f32());
            return Ok((h, w, b, rgba.alpha_f32()))
          }
          _ => {}
        }
      }
      CssColor::Lab(lab) => {
        match &**lab {
          LabColor::Lab(l, a, b, alpha) => {
            match space {
              ColorSpace::Lab => {
                return Ok((*l, *a, *b, *alpha))
              }
              ColorSpace::Lch => {
                let (l, c, h) = lab_to_lch(*l, *a, *b);
                return Ok((l, c, h, *alpha))
              }
              _ => {}
            }
          },
          LabColor::Lch(l, c, h, alpha) => {
            match space {
              ColorSpace::Lch => {
                return Ok((*l, *c, *h, *alpha))
              }
              ColorSpace::Lab => {
                let (l, a, b) = lch_to_lab(*l, *c, *h);
                return Ok((l, a, b, *alpha))
              }
              _ => {}
            }
          }
          LabColor::Oklab(l, a, b, alpha) => {
            match space {
              ColorSpace::Oklab => {
                return Ok((*l, *a, *b, *alpha))
              }
              ColorSpace::Oklch => {
                let (l, c, h) = lab_to_lch(*l, *a, *b);
                return Ok((l, c, h, *alpha))
              }
              _ => {}
            }
          },
          LabColor::Oklch(l, c, h, alpha) => {
            match space {
              ColorSpace::Oklch => {
                return Ok((*l, *c, *h, *alpha))
              }
              ColorSpace::Oklab => {
                let (l, a, b) = lch_to_lab(*l, *c, *h);
                return Ok((l, a, b, *alpha))
              }
              _ => {}
            }
          }
          _ => {}
        }
      }
      CssColor::Predefined(predefined) => {
        match &**predefined {
          PredefinedColor::XyzD50(x, y, z, a) => {
            match space {
              ColorSpace::XyzD50 => {
                return Ok((*x, *y, *z, *a))
              }
              ColorSpace::XyzD65 => {
                let (x, y, z) = d50_to_d65(*x, *y, *z);
                return Ok((x, y, z, *a))
              }
              _ => {}
            }
          }
          PredefinedColor::XyzD65(x, y, z, a) => {
            match space {
              ColorSpace::XyzD65 => {
                return Ok((*x, *y, *z, *a))
              }
              ColorSpace::XyzD50 => {
                let (x, y, z) = d65_to_d50(*x, *y, *z);
                return Ok((x, y, z, *a))
              }
              _ => {}
            }
          }
          _ => {}
        }
      },
      _ => {}
    }

    let (x, y, z, alpha) = self.to_xyza_d65();
    match space {
      ColorSpace::Srgb => {
        let (r, g, b) = xyz_d65_to_srgb(x, y, z);
        Ok((r, g, b, alpha))
      }
      ColorSpace::SrgbLinear => {
        let (r, g, b) = xyz_d65_to_lin_srgb(x, y, z);
        Ok((r, g, b, alpha))
      }
      ColorSpace::Hsl => {
        let (r, g, b) = xyz_d65_to_srgb(x, y, z);
        let (h, s, l) = rgb_to_hsl(r, g, b);
        Ok((h, s, l, alpha))
      }
      ColorSpace::Hwb => {
        let (r, g, b) = xyz_d65_to_srgb(x, y, z);
        let (h, w, b) = rgb_to_hwb(r, g, b);
        Ok((h, w, b, alpha))
      }
      ColorSpace::Lab => {
        let (l, a, b) = xyz_d65_to_lab(x, y, z);
        Ok((l, a, b, alpha))
      }
      ColorSpace::Lch => {
        let (l, a, b) = xyz_d65_to_lab(x, y, z);
        let (l, c, h) = lab_to_lch(l, a, b);
        Ok((l, c, h, alpha))
      }
      ColorSpace::Oklab => {
        let (l, a, b) = xyz_d65_to_oklab(x, y, z);
        Ok((l, a, b, alpha))
      }
      ColorSpace::Oklch => {
        let (l, a, b) = xyz_d65_to_oklab(x, y, z);
        let (l, c, h) = lab_to_lch(l, a, b);
        Ok((l, c, h, alpha))
      }
      ColorSpace::Xyz | ColorSpace::XyzD65 => {
        Ok((x, y, z, alpha))
      }
      ColorSpace::XyzD50 => {
        let (x, y, z) = d65_to_d50(x, y, z);
        Ok((x, y, z, alpha))
      }
    }
  }

  pub fn to_rgb(&self) -> CssColor {
    let (x, y, z, a) = match self {
      CssColor::RGBA(..) | CssColor::CurrentColor => return self.clone(),
      _ => self.to_xyza_d65()
    };

    let (r, g, b) = xyz_d65_to_srgb(x, y, z);
    CssColor::RGBA(RGBA::from_floats(r, g, b, a))
  }

  pub fn to_lab(&self) -> CssColor {
    let (x, y, z, alpha) = match self {
      CssColor::CurrentColor => return self.clone(),
      CssColor::Lab(lab) => {
        match &**lab {
          LabColor::Lab(..) => return self.clone(),
          LabColor::Lch(l, c, h, alpha) => {
            let (l, a, b) = lch_to_lab(*l, *c, *h);
            return CssColor::Lab(Box::new(LabColor::Lab(l, a, b, *alpha)))
          }
          _ => lab.to_xyza_d65()
        }
      }
      _ => self.to_xyza_d65()
    };

    let (l, a, b) = xyz_d65_to_lab(x, y, z);
    CssColor::Lab(Box::new(LabColor::Lab(l, a, b, alpha)))
  }

  pub fn to_p3(&self) -> CssColor {
    let (x, y, z, a) = match self {
      CssColor::CurrentColor => return self.clone(),
      CssColor::Predefined(predefined) => {
        match &**predefined {
          PredefinedColor::DisplayP3(..) => return self.clone(),
          _ => predefined.to_xyza_d65()
        }
      },
      _ => self.to_xyza_d65()
    };

    let (r, g, b) = xyz_d65_to_p3(x, y, z);
    CssColor::Predefined(Box::new(PredefinedColor::DisplayP3(r, g, b, a)))
  }

  pub fn get_necessary_fallbacks(&self, targets: Browsers) -> ColorFallbackKind {
    let feature = match self {
      CssColor::CurrentColor | CssColor::RGBA(_) => return ColorFallbackKind::empty(),
      CssColor::Lab(lab) => {
        match &**lab {
          LabColor::Lab(..) => Feature::LabColors,
          LabColor::Lch(..) => Feature::LchColors,
          LabColor::Oklab(..) => Feature::OklabColors,
          LabColor::Oklch(..) => Feature::OklchColors
        }
      },
      CssColor::Predefined(..) => Feature::ColorFunction
    };

    let mut fallbacks = ColorFallbackKind::empty();
    if !feature.is_compatible(targets) {
      // Convert to lab if compatible. This should not affect interpolation
      // since this is always done in oklab by default, except for legacy
      // srgb syntaxes. See https://www.w3.org/TR/css-color-4/#interpolation-space.
      // If lab is not compatible with all targets, try P3 as some browsers
      // implemented this before other color spaces. Finally, fall back to
      // legacy sRGB if none of these are fully compatible with the targets.
      if Feature::LabColors.is_compatible(targets) {
        fallbacks |= ColorFallbackKind::LAB;
      } else if Feature::P3Colors.is_compatible(targets) {
        fallbacks |= ColorFallbackKind::P3;
        if feature == Feature::OklabColors || feature == Feature::OklchColors {
          fallbacks |= ColorFallbackKind::LAB;
        }
      } else {
        fallbacks |= ColorFallbackKind::RGB;

        // Also include either lab or P3 if partially compatible, as these
        // support a much wider color gamut than sRGB. Lab will replace the
        // original color (e.g. oklab), whereas P3 will be in addition.
        if Feature::LabColors.is_partially_compatible(targets) {
          fallbacks |= ColorFallbackKind::LAB;
        } else if Feature::P3Colors.is_partially_compatible(targets) {
          fallbacks |= ColorFallbackKind::P3;

          // Convert oklab to lab because it has better compatibility.
          if feature == Feature::OklabColors || feature == Feature::OklchColors {
            fallbacks |= ColorFallbackKind::LAB;
          }
        }
      }
    }

    fallbacks
  }

  pub(crate) fn get_fallback(&self, kind: ColorFallbackKind) -> CssColor {
    if matches!(self, CssColor::RGBA(_)) {
      return self.clone();
    }

    match kind {
      ColorFallbackKind::RGB => self.to_rgb(),
      ColorFallbackKind::P3 => self.to_p3(),
      ColorFallbackKind::LAB => self.to_lab(),
      _ => unreachable!()
    }
  }
}

impl FallbackValues for CssColor {
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<CssColor> {
    let fallbacks = self.get_necessary_fallbacks(targets);

    let mut res = Vec::new();
    if fallbacks.contains(ColorFallbackKind::RGB) {
      res.push(self.to_rgb());
    }

    if fallbacks.contains(ColorFallbackKind::P3) {
      res.push(self.to_p3());
    }

    if fallbacks.contains(ColorFallbackKind::LAB) {
      *self = self.to_lab();
    }

    res
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
      CssColor::Lab(lab) => {
        match &**lab {
          LabColor::Lab(l, a, b, alpha) => write_components("lab", *l, *a, *b, *alpha, dest),
          LabColor::Lch(l, c, h, alpha) => write_components("lch", *l, *c, *h, *alpha, dest),
          LabColor::Oklab(l, a, b, alpha) => write_components("oklab", *l, *a, *b, *alpha, dest),
          LabColor::Oklch(l, c, h, alpha) => write_components("oklch", *l, *c, *h, *alpha, dest),    
        }
      }
      CssColor::Predefined(predefined) => write_predefined(predefined, dest)
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
      let (l, a, b, alpha) = parse_lab(input)?;
      let lab = LabColor::Lab(l, a, b, alpha);
      Ok(CssColor::Lab(Box::new(lab)))
    },
    "oklab" => {
      let (l, a, b, alpha) = parse_lab(input)?;
      let lab = LabColor::Oklab(l, a, b, alpha);
      Ok(CssColor::Lab(Box::new(lab)))
    },
    "lch" => {
      let (l, c, h, alpha) = parse_lch(input)?;
      let lab = LabColor::Lch(l, c, h, alpha);
      Ok(CssColor::Lab(Box::new(lab)))
    },
    "oklch" => {
      let (l, c, h, alpha) = parse_lch(input)?;
      let lab = LabColor::Oklch(l, c, h, alpha);
      Ok(CssColor::Lab(Box::new(lab)))
    },
    "color" => {
      let predefined = parse_predefined(input)?;
      Ok(CssColor::Predefined(Box::new(predefined)))
    },
    "color-mix" => {
      input.parse_nested_block(parse_color_mix)
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
    let l = input.expect_percentage()?.max(0.0);
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
    let l = input.expect_percentage()?.max(0.0);
    let c = input.expect_number()?.max(0.0);
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
fn parse_predefined<'i, 't>(input: &mut Parser<'i, 't>) -> Result<PredefinedColor, ParseError<'i, ParserError<'i>>> {
  // https://www.w3.org/TR/css-color-4/#color-function
  let res = input.parse_nested_block(|input| {
    input
      .try_parse(parse_predefined_rgb)
      .or_else(|_| input.try_parse(parse_predefined_xyz))
  })?;

  Ok(res)
}

#[inline]
fn parse_predefined_rgb<'i, 't>(input: &mut Parser<'i, 't>) -> Result<PredefinedColor, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  let colorspace = input.expect_ident_cloned()?;
  
  // Out of gamut values should not be clamped, i.e. values < 0 or > 1 should be preserved.
  // The browser will gamut-map the color for the target device that it is rendered on.
  let r = input.try_parse(|input| parse_number_or_percentage(input)).unwrap_or(0.0);
  let g = input.try_parse(|input| parse_number_or_percentage(input)).unwrap_or(0.0);
  let b = input.try_parse(|input| parse_number_or_percentage(input)).unwrap_or(0.0);
  let alpha = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
    parse_number_or_percentage(input)?
  } else {
    1.0
  };

  let res = match_ignore_ascii_case! { &*&colorspace,
    "srgb" => PredefinedColor::Srgb(r, g, b, alpha),
    "srgb-linear" => PredefinedColor::SrgbLinear(r, g, b, alpha),
    "display-p3" => PredefinedColor::DisplayP3(r, g, b, alpha),
    "a98-rgb" => PredefinedColor::A98Rgb(r, g, b, alpha),
    "prophoto-rgb" => PredefinedColor::ProphotoRgb(r, g, b, alpha),
    "rec2020" => PredefinedColor::Rec2020(r, g, b, alpha),
    _ => return Err(location.new_unexpected_token_error(
      cssparser::Token::Ident(colorspace.clone())
    ))
  };

  Ok(res)
}

#[inline]
fn parse_predefined_xyz<'i, 't>(input: &mut Parser<'i, 't>) -> Result<PredefinedColor, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  let colorspace = input.expect_ident_cloned()?;

  // XYZ color spaces only accept numbers, not percentages.
  // Out of gamut values should not be clamped, i.e. values < 0 or > 1 should be preserved.
  // The browser will gamut-map the color for the target device that it is rendered on.
  let x = input.try_parse(|input| input.expect_number()).unwrap_or(0.0);
  let y = input.try_parse(|input| input.expect_number()).unwrap_or(0.0);
  let z = input.try_parse(|input| input.expect_number()).unwrap_or(0.0);
  let alpha = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
    parse_number_or_percentage(input)?
  } else {
    1.0
  };

  let res = match_ignore_ascii_case! { &*&colorspace,
    "xyz-d50" => PredefinedColor::XyzD50(x, y, z, alpha),
    "xyz" | "xyz-d65" => PredefinedColor::XyzD65(x, y, z, alpha),
    _ => return Err(location.new_unexpected_token_error(
      cssparser::Token::Ident(colorspace.clone())
    ))
  };

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

#[inline]
fn write_predefined<W>(predefined: &PredefinedColor, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
  use PredefinedColor::*;
  
  let (name, a, b, c, alpha) = match predefined {
    Srgb(a, b, c, alpha) => ("srgb", a, b, c, alpha),
    SrgbLinear(a, b, c, alpha) => ("srgb-linear", a, b, c, alpha),
    DisplayP3(a, b, c, alpha) => ("display-p3", a, b, c, alpha),
    A98Rgb(a, b, c, alpha) => ("a98-rgb", a, b, c, alpha),
    ProphotoRgb(a, b, c, alpha) => ("prophoto-rgb", a, b, c, alpha),
    Rec2020(a, b, c, alpha) => ("rec2020", a, b, c, alpha),
    XyzD50(a, b, c, alpha) => ("xyz-d50", a, b, c, alpha),
    // "xyz" has better compatibility (Safari 15) than "xyz-d65", and it is shorter.
    XyzD65(a, b, c, alpha) => ("xyz", a, b, c, alpha),
  };

  dest.write_str("color(")?;
  dest.write_str(name)?;
  if !dest.minify || *a != 0.0 || *b != 0.0 || *c != 0.0 {
    dest.write_char(' ')?;
    a.to_css(dest)?;
    if !dest.minify || *b != 0.0 || *c != 0.0 {
      dest.write_char(' ')?;
      b.to_css(dest)?;
      if !dest.minify || *c != 0.0 {
        dest.write_char(' ')?;
        c.to_css(dest)?;
      }
    }
  }

  if *alpha != 1.0 {
    dest.delim('/', true)?;
    alpha.to_css(dest)?;
  }

  dest.write_char(')')
}

#[inline]
fn lch_to_lab(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L385
  let a = c * (h * PI / 180.0).cos();
  let b = c * (h * PI / 180.0).sin();
  (l, a, b)
}

#[inline]
fn lab_to_lch(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L375
  let mut h = b.atan2(a) * 180.0 / PI;
  if h < 0.0 {
    h += 360.0;
  }
  let c = (a.powi(2) + b.powi(2)).sqrt();
  (l, c, h)
}

const D50: &[f32] = &[0.3457 / 0.3585, 1.00000, (1.0 - 0.3457 - 0.3585) / 0.3585];

fn lab_to_xyz_d50(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L352
  const K: f32 = 24389.0 / 27.0;   // 29^3/3^3
  const E: f32 = 216.0 / 24389.0;  // 6^3/29^3

  // compute f, starting with the luminance-related term
  let f1 = (l + 16.0) / 116.0;
  let f0 = a / 500.0 + f1;
  let f2 = f1 - b / 200.0;

  // compute xyz
  let x = if f0.powi(3) > E {
    f0.powi(3)
  } else {
    (116.0 * f0 - 16.0) / K
  };

  let y = if l > K * E {
    ((l + 16.0) / 116.0).powi(3)
  } else {
    l / K
  };

  let z = if f2.powi(3) > E {
    f2.powi(3)
  } else {
    (116.0 * f2 - 16.0) / K
  };

  // Compute XYZ by scaling xyz by reference white
  let x = x * D50[0];
  let y = y * D50[1];
  let z = z * D50[2];
  (x, y, z)
}

fn d50_to_d65(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L319
  const MATRIX: &[f32] = &[
    0.9554734527042182,    -0.023098536874261423, 0.0632593086610217,
    -0.028369706963208136,  1.0099954580058226,   0.021041398966943008,
    0.012314001688319899,  -0.020507696433477912, 1.3303659366080753
  ];

  multiply_matrix(MATRIX, x, y, z)
}

fn d65_to_d50(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L319
  const MATRIX: &[f32] = &[
     1.0479298208405488,    0.022946793341019088,  -0.05019222954313557,
     0.029627815688159344,  0.990434484573249,     -0.01707382502938514,
    -0.009243058152591178,  0.015055144896577895,   0.7518742899580008
  ];

  multiply_matrix(MATRIX, x, y, z)
}

#[inline]
fn xyz_d65_to_lin_srgb(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L62
  const MATRIX: &[f32] = &[
     3.2409699419045226,  -1.537383177570094,   -0.4986107602930034,
    -0.9692436362808796,   1.8759675015077202,   0.04155505740717559,
     0.05563007969699366, -0.20397695888897652,  1.0569715142428786
  ];

  multiply_matrix(MATRIX, x, y, z)
}

fn xyz_d65_to_srgb(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let (r, g, b) = xyz_d65_to_lin_srgb(x, y, z);
  gam_srgb(r, g, b)
}

#[inline]
fn multiply_matrix(m: &[f32], x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let a = m[0] * x + m[1] * y + m[2] * z;
  let b  = m[3] * x + m[4] * y + m[5] * z;
  let c = m[6] * x + m[7] * y + m[8] * z;
  (a, b, c)
}

#[inline]
fn gam_srgb(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L31
  // convert an array of linear-light sRGB values in the range 0.0-1.0
  // to gamma corrected form
  // https://en.wikipedia.org/wiki/SRGB
  // Extended transfer function:
  // For negative values, linear portion extends on reflection
  // of axis, then uses reflected pow below that
  let r = gam_srgb_component(r);
  let g = gam_srgb_component(g);
  let b = gam_srgb_component(b);
  (r, g, b)
}

#[inline]
fn gam_srgb_component(c: f32) -> f32 {
  let abs = c.abs();
  if abs > 0.0031308 {
    let sign = if c < 0.0 { -1.0 } else { 1.0 };
    return sign * (1.055 * abs.powf(1.0 / 2.4) - 0.055);
  }

  return 12.92 * c;
}

fn oklab_to_xyz_d65(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L418
  const LMS_TO_XYZ: &[f32] = &[
     1.2268798733741557,  -0.5578149965554813,   0.28139105017721583,
    -0.04057576262431372,  1.1122868293970594,  -0.07171106666151701,
    -0.07637294974672142, -0.4214933239627914,   1.5869240244272418
  ];

  const OKLAB_TO_LMS: &[f32] = &[
    0.99999999845051981432,  0.39633779217376785678,   0.21580375806075880339,
    1.0000000088817607767,  -0.1055613423236563494,   -0.063854174771705903402,
    1.0000000546724109177,  -0.089484182094965759684, -1.2914855378640917399
  ];

  let (a, b, c) = multiply_matrix(OKLAB_TO_LMS, l, a, b);
  multiply_matrix(LMS_TO_XYZ, a.powi(3), b.powi(3), c.powi(3))
}

#[inline]
fn xyz_d65_to_oklab(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L400
  const XYZ_TO_LMS: &[f32] = &[
		0.8190224432164319,    0.3619062562801221,    -0.12887378261216414,
		0.0329836671980271,    0.9292868468965546,     0.03614466816999844,
		0.048177199566046255,  0.26423952494422764,    0.6335478258136937
	];

	const LMS_TO_OKLAB: &[f32] = &[
		0.2104542553,   0.7936177850,  -0.0040720468,
		1.9779984951,  -2.4285922050,   0.4505937099,
		0.0259040371,   0.7827717662,  -0.8086757660
	];

  let (a, b, c) = multiply_matrix(XYZ_TO_LMS, x, y, z);
  multiply_matrix(LMS_TO_OKLAB, a.cbrt(), b.cbrt(), c.cbrt())
}

fn xyz_d50_to_lab(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L332
  // Assuming XYZ is relative to D50, convert to CIE Lab
  // from CIE standard, which now defines these as a rational fraction
  const E: f32 = 216.0 / 24389.0;  // 6^3/29^3
  const K: f32 = 24389.0 / 27.0;   // 29^3/3^3

  // compute xyz, which is XYZ scaled relative to reference white
  // var xyz = XYZ.map((value, i) => value / D50[i]);
  let x = x / D50[0];
  let y = y / D50[1];
  let z = z / D50[2];

  // now compute f
  // var f = xyz.map(value => value > ε ? Math.cbrt(value) : (κ * value + 16)/116);
  let f0 = if x > E {
    x.cbrt()
  } else {
    (K * x + 16.0) / 116.0
  };

  let f1 = if y > E {
    y.cbrt()
  } else {
    (K * y + 16.0) / 116.0
  };

  let f2 = if z > E {
    z.cbrt()
  } else {
    (K * z + 16.0) / 116.0
  };

  let l = ((116.0 * f1) - 16.0) / 100.0;
  let a = 500.0 * (f0 - f1);
  let b = 200.0 * (f1 - f2);
  (l, a, b)
}

fn lin_srgb(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L11
  // convert sRGB values where in-gamut values are in the range [0 - 1]
  // to linear light (un-companded) form.
  // https://en.wikipedia.org/wiki/SRGB
  // Extended transfer function:
  // for negative values, linear portion is extended on reflection of axis,
  // then reflected power function is used.
  let r = lin_srgb_component(r);
  let g = lin_srgb_component(g);
  let b = lin_srgb_component(b);
  (r, g, b)
}

#[inline]
fn lin_srgb_component(c: f32) -> f32 {
  let abs = c.abs();
  if abs < 0.04045 {
    return c / 12.92;
  }
  
  let sign = if c < 0.0 { -1.0 } else { 1.0 };
  sign * ((abs + 0.055) / 1.055).powf(2.4)
}

fn lin_srgb_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L50
  // convert an array of linear-light sRGB values to CIE XYZ
  // using sRGB's own white, D65 (no chromatic adaptation)
  const MATRIX: &[f32] = &[
    0.41239079926595934, 0.357584339383878,   0.1804807884018343,
    0.21263900587151027, 0.715168678767756,   0.07219231536073371,
    0.01933081871559182, 0.11919477979462598, 0.9505321522496607
  ];

  multiply_matrix(MATRIX, r, g, b)
}

fn srgb_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // convert gamma-corrected sRGB values in the 0.0 to 1.0 range
  // to linear-light sRGB, then to CIE XYZ.
  let (r, g, b) = lin_srgb(r, g, b);
  lin_srgb_to_xyz_d65(r, g, b)
}

#[inline]
fn xyz_d65_to_lab(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let (x, y, z) = d65_to_d50(x, y, z);
  xyz_d50_to_lab(x, y, z)
}

#[inline]
fn xyz_d65_to_lin_p3(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L105
  const MATRIX: &[f32] = &[
     2.493496911941425,   -0.9313836179191239, -0.40271078445071684,
    -0.8294889695615747,   1.7626640603183463,  0.023624685841943577,
     0.03584583024378447, -0.07617238926804182, 0.9568845240076872
  ];

  multiply_matrix(MATRIX, x, y, z)
}

#[inline]
fn xyz_d65_to_p3(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let (r, g, b) = xyz_d65_to_lin_p3(x, y, z);
  gam_srgb(r, g, b) // same as sRGB
}

#[inline]
fn lin_p3_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L91
  // convert linear-light display-p3 values to CIE XYZ
  // using D65 (no chromatic adaptation)
  // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
  const MATRIX: &[f32] = &[
    0.4865709486482162, 0.26566769316909306, 0.1982172852343625,
    0.2289745640697488, 0.6917385218365064,  0.079286914093745,
    0.0000000000000000, 0.04511338185890264, 1.043944368900976,
  ];

  multiply_matrix(MATRIX, r, g, b)
}

#[inline]
fn lin_a98rgb(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L181
  // convert an array of a98-rgb values in the range 0.0 - 1.0
  // to linear light (un-companded) form.
  // negative values are also now accepted
  let r = lin_a98rgb_component(r);
  let g = lin_a98rgb_component(g);
  let b = lin_a98rgb_component(b);
  (r, g, b)
}

#[inline]
fn lin_a98rgb_component(c: f32) -> f32 {
  let sign = if c < 0.0 { -1.0 } else { 1.0 };
  sign * c.abs().powf(563.0 / 256.0)
}

#[inline]
fn lin_a98rgb_to_xyz(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // convert an array of linear-light a98-rgb values to CIE XYZ
  // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
  // has greater numerical precision than section 4.3.5.3 of
  // https://www.adobe.com/digitalimag/pdfs/AdobeRGB1998.pdf
  // but the values below were calculated from first principles
  // from the chromaticity coordinates of R G B W
  // see matrixmaker.html
  const MATRIX: &[f32] = &[
    0.5766690429101305,   0.1855582379065463,   0.1882286462349947,
    0.29734497525053605,  0.6273635662554661,   0.07529145849399788,
    0.02703136138641234,  0.07068885253582723,  0.9913375368376388
  ];

  multiply_matrix(MATRIX, r, g, b)
}

#[inline]
fn lin_prophoto(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L118
  // convert an array of prophoto-rgb values
  // where in-gamut colors are in the range [0.0 - 1.0]
  // to linear light (un-companded) form.
  // Transfer curve is gamma 1.8 with a small linear portion
  // Extended transfer function
  let r = lin_prophoto_component(r);
  let g = lin_prophoto_component(g);
  let b = lin_prophoto_component(b);
  (r, g, b)
}

#[inline]
fn lin_prophoto_component(c: f32) -> f32 {
  const ET2: f32 = 16.0 / 512.0;
  let abs = c.abs();
  if abs <= ET2 {
    return c / 16.0;
  }

  let sign = if c < 0.0 { -1.0 } else { 1.0 };
  sign * c.powf(1.8)
}

#[inline]
fn lin_prophoto_to_xyz_d50(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L155
  // convert an array of linear-light prophoto-rgb values to CIE XYZ
  // using  D50 (so no chromatic adaptation needed afterwards)
  // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
  const MATRIX: &[f32] = &[
    0.7977604896723027,  0.13518583717574031,  0.0313493495815248,
    0.2880711282292934,  0.7118432178101014,   0.00008565396060525902,
    0.0,                 0.0,                  0.8251046025104601
  ];

  multiply_matrix(MATRIX, r, g, b)
}

#[inline]
fn lin_rec2020(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L235
  // convert an array of rec2020 RGB values in the range 0.0 - 1.0
  // to linear light (un-companded) form.
  // ITU-R BT.2020-2 p.4
  let r = lin_rec2020_component(r);
  let g = lin_rec2020_component(g);
  let b = lin_rec2020_component(b);
  (r, g, b)
}

#[inline]
fn lin_rec2020_component(c: f32) -> f32 {
  const A: f32 = 1.09929682680944;
  const B: f32 = 0.018053968510807;

  let abs = c.abs();
  if abs < B * 4.5 {
    return c / 4.5;
  }

  let sign = if c < 0.0 { -1.0 } else { 1.0 };
  sign * ((abs + A - 1.0) / A).powf(1.0 / 0.45)
}

#[inline]
fn lin_rec2020_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L276
  // convert an array of linear-light rec2020 values to CIE XYZ
  // using  D65 (no chromatic adaptation)
  // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
  const MATRIX: &[f32] = &[
    0.6369580483012914, 0.14461690358620832,  0.1688809751641721,
    0.2627002120112671, 0.6779980715188708,   0.05930171646986196,
    0.000000000000000,  0.028072693049087428, 1.060985057710791
  ];

  multiply_matrix(MATRIX, r, g, b)
}

impl LabColor {
  pub fn to_xyza_d65(&self) -> (f32, f32, f32, f32) {
    use LabColor::*;
    match self {
      Lab(l, a, b, alpha) => {
        let (x, y, z) = lab_to_xyz_d50(*l * 100.0, *a, *b);
        let (x, y, z) = d50_to_d65(x, y, z);
        (x, y, z, *alpha)
      }
      Lch(l, c, h, alpha) => {
        let (l, a, b) = lch_to_lab(*l * 100.0, *c, *h);
        let (x, y, z) = lab_to_xyz_d50(l, a, b);
        let (x, y, z) = d50_to_d65(x, y, z);
        (x, y, z, *alpha)
      }
      Oklab(l, a, b, alpha) => {
        let  (x, y, z) = oklab_to_xyz_d65(*l, *a, *b);
        (x, y, z, *alpha)
      }
      Oklch(l, c, h, alpha) => {
        let (l, a, b) = lch_to_lab(*l, *c, *h);
        let (x, y, z) = oklab_to_xyz_d65(l, a, b);
        (x, y, z, *alpha)
      }
    }
  }
}

impl PredefinedColor {
  pub fn to_xyza_d65(&self) -> (f32, f32, f32, f32) {
    use PredefinedColor::*;

    match self {
      Srgb(r, g, b, a) => {
        let (x, y, z) = srgb_to_xyz_d65(*r, *g, *b);
        (x, y, z, *a)
      }
      SrgbLinear(r, g, b, a) => {
        let (x, y, z) = lin_srgb_to_xyz_d65(*r, *g, *b);
        (x, y, z, *a)
      }
      DisplayP3(r, g, b, a) => {
        let (r, g, b) = lin_srgb(*r, *g, *b); // same as sRGB
        let (x, y, z) = lin_p3_to_xyz_d65(r, g, b);
        (x, y, z, *a)
      }
      A98Rgb(r, g, b, a) => {
        let (r, g, b) = lin_a98rgb(*r, *g, *b);
        let (x, y, z) = lin_a98rgb_to_xyz(r, g, b);
        (x, y, z, *a)
      }
      ProphotoRgb(r, g, b, a) => {
        let (r, g, b) = lin_prophoto(*r, *g, *b);
        let (x, y, z) = lin_prophoto_to_xyz_d50(r, g, b);
        let (x, y, z) = d50_to_d65(x, y, z);
        (x, y, z, *a)
      }
      Rec2020(r, g, b, a) => {
        let (r, g, b) = lin_rec2020(*r, *g, *b);
        let (x, y, z) = lin_rec2020_to_xyz_d65(r, g, b);
        (x, y, z, *a)
      }
      XyzD50(x, y, z, a) => {
        let (x, y, z) = d50_to_d65(*x, *y, *z);
        (x, y, z, *a)
      }
      XyzD65(x, y, z, a) => {
        (*x, *y, *z, *a)
      }
    }
  }
}

#[inline]
fn rgb_to_hsl(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://drafts.csswg.org/css-color/#rgb-to-hsl
  let max = r.max(g).max(b);
  let min = r.min(g).min(b);
  let mut hue = f32::NAN;
  let mut sat: f32 = 0.0;
  let light = (min + max) / 2.0;
  let d = max - min;

  if d != 0.0 {
    sat = if light == 0.0 || light == 1.0 {
      0.0
    } else {
      (max - light) / light.min(1.0 - light)
    };

    if max == r {
      hue = (g - b) / d + (if g < b { 6.0 } else { 0.0 });
    } else if max == g {
      hue = (b - r) / d + 2.0;
    } else if max == b {
      hue = (r - g) / d + 4.0;
    }

    hue = hue * 60.0;
  }

  (hue, sat, light)
}

#[inline]
fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (f32, f32, f32) {
  // https://drafts.csswg.org/css-color/#hsl-to-rgb
  let mut h = h % 360.0;
  if h < 0.0 {
    h += 360.0;
  }

  fn hue_to_rgb(n: f32, h: f32, s: f32, l: f32) -> f32 {
    let k = (n + h / 30.0) % 12.0;
    let a = s * l.min(1.0 - l);
    l - a * (k - 3.0).min(9.0 - k).clamp(-1.0, 1.0)
  }

  let r = hue_to_rgb(0.0, h, s, l);
  let g = hue_to_rgb(8.0, h, s, l);
  let b = hue_to_rgb(4.0, h, s, l);
  (r, g, b)
}

#[inline]
fn hwb_to_rgb(h: f32, w: f32, b: f32) -> (f32, f32, f32) {
  // https://drafts.csswg.org/css-color/#hwb-to-rgb
  if w + b >= 1.0 {
    let gray = w / (w + b);
    return (gray, gray, gray);
  }

  let (mut red, mut green, mut blue) = hsl_to_rgb(h, 1.0, 0.5);
  let x = 1.0 - w - b;
  red = red * x + w;
  green = green * x + w;
  blue = blue * x + w;
  (red, green, blue)
}

#[inline]
fn rgb_to_hwb(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  let (h, _, _) = rgb_to_hsl(r, g, b);
  let w = r.min(g).min(b);
  let b = 1.0 - r.max(g).max(b);
  (h, w, b)
}

fn parse_color_mix<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CssColor, ParseError<'i, ParserError<'i>>> {
  input.expect_ident_matching("in")?;
  let method = ColorSpace::parse(input)?;
  input.expect_comma()?;

  let hue_method = HueInterpolationMethod::Shorter; // TODO

  let first_color = CssColor::parse(input)?;
  let first_percent = input.try_parse(|input| input.expect_percentage()).ok();
  input.expect_comma()?;

  let second_color = CssColor::parse(input)?;
  let second_percent = input.try_parse(|input| input.expect_percentage()).ok();

  // https://drafts.csswg.org/css-color-5/#color-mix-percent-norm
  let (mut p1, mut p2) = if first_percent.is_none() && second_percent.is_none() {
    (0.5, 0.5)
  } else {
    let p2 = second_percent.unwrap_or_else(|| 1.0 - first_percent.unwrap());
    let p1 = first_percent.unwrap_or_else(|| 1.0 - second_percent.unwrap());
    (p1, p2)
  };

  if (p1 + p2) == 0.0 {
    return Err(input.new_custom_error(ParserError::InvalidValue));
  }

  let alpha_multiplier = p1 + p2;
  if alpha_multiplier != 1.0 {
    p1 = p1 / alpha_multiplier;
    p2 = p2 / alpha_multiplier;
  }

  // https://drafts.csswg.org/css-color-5/#color-mix-result
  let mut first_color = first_color.to_color_space(method)
    .map_err(|_| input.new_custom_error(ParserError::InvalidValue))?;
  let mut second_color = second_color.to_color_space(method)
    .map_err(|_| input.new_custom_error(ParserError::InvalidValue))?;

  println!("{:?} {:?}", first_color, second_color);

  // https://www.w3.org/TR/css-color-4/#powerless
  adjust_powerless_components(method, &mut first_color, &mut second_color);
  adjust_powerless_components(method, &mut second_color, &mut first_color);

  match method {
    ColorSpace::Lch | ColorSpace::Oklch => {
      (first_color.3, second_color.3) = hue_method.interpolate(first_color.3, second_color.3);
    }
    ColorSpace::Hsl | ColorSpace::Hwb => {
      (first_color.0, second_color.0) = hue_method.interpolate(first_color.0, second_color.0);
    }
    _ => {}
  }

  if first_color.3 != 1.0 {
    // https://drafts.csswg.org/css-color-4/#interpolation-alpha
    premultiply(method, &mut first_color);
  }

  if second_color.3 != 1.0 {
    premultiply(method, &mut second_color);
  }

  let mut result_color = (
    first_color.0 * p1 + second_color.0 * p2,
    first_color.1 * p1 + second_color.1 * p2,
    first_color.2 * p1 + second_color.2 * p2,
    first_color.3 * p1 + second_color.3 * p2,
  );

  if result_color.3 != 0.0 {
    un_premultiply(method, &mut result_color);
  }

  if alpha_multiplier < 1.0 {
    result_color.3 *= alpha_multiplier;
  }

  let result = match method {
    ColorSpace::Srgb => {
      CssColor::RGBA(RGBA::from_floats(result_color.0, result_color.1, result_color.2, result_color.3))
    }
    ColorSpace::SrgbLinear => {
      let (r, g, b) = gam_srgb(result_color.0, result_color.1, result_color.2);
      CssColor::RGBA(RGBA::from_floats(r, g, b, result_color.3))
    }
    ColorSpace::Hsl => {
      let (r, g, b) = hsl_to_rgb(result_color.0, result_color.1, result_color.2);
      CssColor::RGBA(RGBA::from_floats(r, g, b, result_color.3))
    }
    ColorSpace::Hwb => {
      let (r, g, b) = hwb_to_rgb(result_color.0, result_color.1, result_color.2);
      CssColor::RGBA(RGBA::from_floats(r, g, b, result_color.3))
    }
    ColorSpace::Lab => {
      CssColor::Lab(Box::new(LabColor::Lab(result_color.0, result_color.1, result_color.2, result_color.3)))
    }
    ColorSpace::Lch => {
      CssColor::Lab(Box::new(LabColor::Lch(result_color.0, result_color.1, result_color.2, result_color.3)))
    }
    ColorSpace::Oklab => {
      CssColor::Lab(Box::new(LabColor::Oklab(result_color.0, result_color.1, result_color.2, result_color.3)))
    }
    ColorSpace::Oklch => {
      CssColor::Lab(Box::new(LabColor::Oklch(result_color.0, result_color.1, result_color.2, result_color.3)))
    }
    ColorSpace::Xyz | ColorSpace::XyzD65 => {
      CssColor::Predefined(Box::new(PredefinedColor::XyzD65(result_color.0, result_color.1, result_color.2, result_color.3)))
    }
    ColorSpace::XyzD50 => {
      CssColor::Predefined(Box::new(PredefinedColor::XyzD50(result_color.0, result_color.1, result_color.2, result_color.3)))
    }
  };
  
  Ok(result)
}

fn premultiply(space: ColorSpace, color: &mut (f32, f32, f32, f32)) {
  // https://drafts.csswg.org/css-color-4/#interpolation-alpha
  match space {
    ColorSpace::Srgb |
    ColorSpace::SrgbLinear |
    ColorSpace::Lab |
    ColorSpace::Oklab |
    ColorSpace::Xyz |
    ColorSpace::XyzD50 |
    ColorSpace::XyzD65 => {
      // For rectangular orthogonal color coordinate systems,
      // all component values are multiplied by the alpha value.
      color.0 *= color.3;
      color.1 *= color.3;
      color.2 *= color.3;
    }
    ColorSpace::Hsl |
    ColorSpace::Hwb => {
      // For cylindrical polar color coordinate systems, the hue angle
      // is not premultiplied, but the other two axes are premultiplied.
      color.1 *= color.3;
      color.2 *= color.3;
    }
    ColorSpace::Lch |
    ColorSpace::Oklch => {
      color.0 *= color.3;
      color.1 *= color.3;
    }
  }
}

fn un_premultiply(space: ColorSpace, color: &mut (f32, f32, f32, f32)) {
  match space {
    ColorSpace::Srgb |
    ColorSpace::SrgbLinear |
    ColorSpace::Lab |
    ColorSpace::Oklab |
    ColorSpace::Xyz |
    ColorSpace::XyzD50 |
    ColorSpace::XyzD65 => {
      // For rectangular orthogonal color coordinate systems,
      // all component values are multiplied by the alpha value.
      color.0 /= color.3;
      color.1 /= color.3;
      color.2 /= color.3;
    }
    ColorSpace::Hsl |
    ColorSpace::Hwb => {
      // For cylindrical polar color coordinate systems, the hue angle
      // is not premultiplied, but the other two axes are premultiplied.
      color.1 /= color.3;
      color.2 /= color.3;
    }
    ColorSpace::Lch |
    ColorSpace::Oklch => {
      color.0 /= color.3;
      color.1 /= color.3;
    }
  }
}

fn adjust_powerless_components(space: ColorSpace, first_color: &mut (f32, f32, f32, f32), second_color: &mut (f32, f32, f32, f32)) {
  // https://www.w3.org/TR/css-color-4/#powerless
  match space {
    ColorSpace::Lab | ColorSpace::Oklab => {
      // If the lightness of a Lab color is 0%, both the a and b components are powerless.
      if first_color.0.abs() < f32::EPSILON {
        first_color.1 = second_color.1;
        first_color.2 = second_color.2;
      }
    }
    ColorSpace::Lch | ColorSpace::Oklch => {
      // If the chroma of an LCH color is 0%, the hue component is powerless. 
      // If the lightness of an LCH color is 0%, both the hue and chroma components are powerless.
      if first_color.1.abs() < f32::EPSILON {
        first_color.2 = second_color.2;
      }

      if first_color.0.abs() < f32::EPSILON {
        first_color.1 = second_color.1;
        first_color.2 = second_color.2;
      }
    }
    ColorSpace::Hsl => {
      // If the saturation of an HSL color is 0%, then the hue component is powerless.
      // If the lightness of an HSL color is 0% or 100%, both the saturation and hue components are powerless.
      if first_color.1.abs() < f32::EPSILON {
        first_color.0 = second_color.0;
      }

      if first_color.2.abs() < f32::EPSILON || (first_color.2 - 1.0).abs() < f32::EPSILON {
        first_color.0 = second_color.0;
        first_color.1 = second_color.1;
      }
    }
    ColorSpace::Hwb => {
      if (first_color.1 + first_color.2 - 1.0).abs() < f32::EPSILON {
        first_color.0 = second_color.0;
      }
    }
    _ => {}
  }
}

impl HueInterpolationMethod {
  fn interpolate(&self, mut a: f32, mut b: f32) -> (f32, f32) {
    // https://drafts.csswg.org/css-color/#hue-interpolation
    if *self != HueInterpolationMethod::Specified {
      a = ((a % 360.0) + 360.0) % 360.0;
      b = ((b % 360.0) + 360.0) % 360.0;
    }

    match self {
      HueInterpolationMethod::Shorter => {
        // https://www.w3.org/TR/css-color-4/#hue-shorter
        let delta = b - a;
        if delta > 180.0 {
          a += 360.0;
        } else if delta < -180.0 {
          b += 360.0;
        }
      }
      HueInterpolationMethod::Longer => {
        // https://www.w3.org/TR/css-color-4/#hue-longer
        let delta = b - a;
        if 0.0 < delta && delta < 180.0 {
          a += 360.0;
        } else if -180.0 < delta && delta < 0.0 {
          b += 360.0;
        }
      }
      HueInterpolationMethod::Increasing => {
        // https://www.w3.org/TR/css-color-4/#hue-increasing
        if b < a {
          b += 360.0;
        }
      }
      HueInterpolationMethod::Decreasing => {
        // https://www.w3.org/TR/css-color-4/#hue-decreasing
        if a < b {
          a += 360.0;
        }
      }
      HueInterpolationMethod::Specified => {}
    }

    (a, b)
  }
}
