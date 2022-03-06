use cssparser::*;
use bitflags::bitflags;
use crate::rules::supports::SupportsCondition;
use crate::targets::Browsers;
use crate::traits::{Parse, ToCss, FallbackValues};
use crate::printer::Printer;
use std::any::TypeId;
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
  LAB(Box<LABColor>),
  Predefined(Box<PredefinedColor>)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LABColor {
  LAB(LAB),
  LCH(LCH),
  OKLAB(OKLAB),
  OKLCH(OKLCH),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PredefinedColor {
  SRGB(SRGB),
  SRGBLinear(SRGBLinear),
  DisplayP3(P3),
  A98(A98),
  ProPhoto(ProPhoto),
  Rec2020(Rec2020),
  XYZd50(XYZd50),
  XYZd65(XYZd65)
}

bitflags! {
  pub struct ColorFallbackKind: u8 {
    const RGB    = 0b01;
    const P3     = 0b10;
    const LAB    = 0b100;
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-color-4/#interpolation-space
  enum ColorSpace {
    "srgb": SRGB,
    "srgb-linear": SRGBLinear,
    "lab": LAB,
    "oklab": OKLAB,
    "xyz": XYZ,
    "xyz-d50": XYZd50,
    "xyz-d65": XYZd65,
    "hsl": Hsl,
    "hwb": Hwb,
    "lch": LCH,
    "oklch": OKLCH,
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-color-4/#typedef-hue-interpolation-method
  pub enum HueInterpolationMethod {
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

  pub fn to_rgb(&self) -> CssColor {
    RGBA::from(self).into()
  }

  pub fn to_lab(&self) -> CssColor {
    LAB::from(self).into()
  }

  pub fn to_p3(&self) -> CssColor {
    P3::from(self).into()
  }

  pub fn get_necessary_fallbacks(&self, targets: Browsers) -> ColorFallbackKind {
    let feature = match self {
      CssColor::CurrentColor | CssColor::RGBA(_) => return ColorFallbackKind::empty(),
      CssColor::LAB(lab) => {
        match &**lab {
          LABColor::LAB(..) => Feature::LabColors,
          LABColor::LCH(..) => Feature::LchColors,
          LABColor::OKLAB(..) => Feature::OklabColors,
          LABColor::OKLCH(..) => Feature::OklchColors
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
        // support a much wider color gamut than sRGB. LAB will replace the
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
      CssColor::LAB(lab) => {
        match &**lab {
          LABColor::LAB(lab) => write_components("lab", lab.l, lab.a, lab.b, lab.alpha, dest),
          LABColor::LCH(lch) => write_components("lch", lch.l, lch.c, lch.h, lch.alpha, dest),
          LABColor::OKLAB(lab) => write_components("oklab", lab.l, lab.a, lab.b, lab.alpha, dest),
          LABColor::OKLCH(lch) => write_components("oklch", lch.l, lch.c, lch.h, lch.alpha, dest),    
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
      let lab = LABColor::LAB(LAB { l, a, b, alpha });
      Ok(CssColor::LAB(Box::new(lab)))
    },
    "oklab" => {
      let (l, a, b, alpha) = parse_lab(input)?;
      let lab = LABColor::OKLAB(OKLAB { l, a, b, alpha });
      Ok(CssColor::LAB(Box::new(lab)))
    },
    "lch" => {
      let (l, c, h, alpha) = parse_lch(input)?;
      let lab = LABColor::LCH(LCH { l, c, h, alpha });
      Ok(CssColor::LAB(Box::new(lab)))
    },
    "oklch" => {
      let (l, c, h, alpha) = parse_lch(input)?;
      let lab = LABColor::OKLCH(OKLCH { l, c, h, alpha });
      Ok(CssColor::LAB(Box::new(lab)))
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
      parse_number_or_percentage(input)?.clamp(0.0, 1.0)
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
      parse_number_or_percentage(input)?.clamp(0.0, 1.0)
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
    parse_number_or_percentage(input)?.clamp(0.0, 1.0)
  } else {
    1.0
  };

  let res = match_ignore_ascii_case! { &*&colorspace,
    "srgb" => PredefinedColor::SRGB(SRGB { r, g, b, alpha }),
    "srgb-linear" => PredefinedColor::SRGBLinear(SRGBLinear { r, g, b, alpha }),
    "display-p3" => PredefinedColor::DisplayP3(P3 { r, g, b, alpha }),
    "a98-rgb" => PredefinedColor::A98(A98 { r, g, b, alpha }),
    "prophoto-rgb" => PredefinedColor::ProPhoto(ProPhoto { r, g, b, alpha }),
    "rec2020" => PredefinedColor::Rec2020(Rec2020 { r, g, b, alpha }),
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
    parse_number_or_percentage(input)?.clamp(0.0, 1.0)
  } else {
    1.0
  };

  let res = match_ignore_ascii_case! { &*&colorspace,
    "xyz-d50" => PredefinedColor::XYZd50(XYZd50 { x, y, z, alpha}),
    "xyz" | "xyz-d65" => PredefinedColor::XYZd65(XYZd65 { x, y, z, alpha }),
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
  if (alpha - 1.0).abs() > f32::EPSILON {
    dest.delim('/', true)?;
    alpha.to_css(dest)?;
  }

  dest.write_char(')')
}

#[inline]
fn write_predefined<W>(predefined: &PredefinedColor, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
  use PredefinedColor::*;
  
  let (name, a, b, c, alpha) = match predefined {
    SRGB(rgb) => ("srgb", rgb.r, rgb.g, rgb.b, rgb.alpha),
    SRGBLinear(rgb) => ("srgb-linear", rgb.r, rgb.g, rgb.b, rgb.alpha),
    DisplayP3(rgb) => ("display-p3", rgb.r, rgb.g, rgb.b, rgb.alpha),
    A98(rgb) => ("a98-rgb", rgb.r, rgb.g, rgb.b, rgb.alpha),
    ProPhoto(rgb) => ("prophoto-rgb", rgb.r, rgb.g, rgb.b, rgb.alpha),
    Rec2020(rgb) => ("rec2020", rgb.r, rgb.g, rgb.b, rgb.alpha),
    XYZd50(xyz) => ("xyz-d50", xyz.x, xyz.y, xyz.z, xyz.alpha),
    // "xyz" has better compatibility (Safari 15) than "xyz-d65", and it is shorter.
    XYZd65(xyz) => ("xyz", xyz.x, xyz.y, xyz.z, xyz.alpha),
  };

  dest.write_str("color(")?;
  dest.write_str(name)?;
  if !dest.minify || a != 0.0 || b != 0.0 || c != 0.0 {
    dest.write_char(' ')?;
    a.to_css(dest)?;
    if !dest.minify || b != 0.0 || c != 0.0 {
      dest.write_char(' ')?;
      b.to_css(dest)?;
      if !dest.minify || c != 0.0 {
        dest.write_char(' ')?;
        c.to_css(dest)?;
      }
    }
  }

  if (alpha - 1.0).abs() > f32::EPSILON {
    dest.delim('/', true)?;
    alpha.to_css(dest)?;
  }

  dest.write_char(')')
}

macro_rules! define_colorspace {
  ($name: ident, $a: ident, $b: ident, $c: ident) => {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct $name {
      pub $a: f32,
      pub $b: f32,
      pub $c: f32,
      pub alpha: f32
    }
  }
}

define_colorspace!(SRGB, r, g, b);
define_colorspace!(SRGBLinear, r, g, b);
define_colorspace!(P3, r, g, b);
define_colorspace!(A98, r, g, b);
define_colorspace!(ProPhoto, r, g, b);
define_colorspace!(Rec2020, r, g,b);
define_colorspace!(LAB, l, a, b);
define_colorspace!(OKLAB, l, a, b);
define_colorspace!(XYZd50, x, y, z);
define_colorspace!(XYZd65, x, y, z);
define_colorspace!(LCH, l, c, h);
define_colorspace!(OKLCH, l, c, h);
define_colorspace!(HSL, h, s, l);
define_colorspace!(HWB, h, w, b);

macro_rules! via {
  ($t: ident -> $u: ident -> $v: ident) => {
    impl From<$t> for $v {
      #[inline]
      fn from(t: $t) -> $v {
        let xyz: $u = t.into();
        xyz.into()
      }
    }

    impl From<$v> for $t {
      #[inline]
      fn from(t: $v) -> $t {
        let xyz: $u = t.into();
        xyz.into()
      }
    }
  };
}

#[inline]
fn rectangular_to_polar(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L375
  let mut h = b.atan2(a) * 180.0 / PI;
  if h < 0.0 {
    h += 360.0;
  }
  let c = (a.powi(2) + b.powi(2)).sqrt();
  h = h % 360.0;
  (l, c, h)
}

#[inline]
fn polar_to_rectangular(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L385
  let a = c * (h * PI / 180.0).cos();
  let b = c * (h * PI / 180.0).sin();
  (l, a, b)
}

impl From<LCH> for LAB {
  fn from(lch: LCH) -> LAB {
    let (l, a, b) = polar_to_rectangular(lch.l, lch.c, lch.h);
    LAB { l, a, b, alpha: lch.alpha }
  }
}

impl From<LAB> for LCH {
  fn from(lab: LAB) -> LCH {
    let (l, c, h) = rectangular_to_polar(lab.l, lab.a, lab.b);
    LCH { l, c, h, alpha: lab.alpha }
  }
}

impl From<OKLCH> for OKLAB {
  fn from(lch: OKLCH) -> OKLAB {
    let (l, a, b) = polar_to_rectangular(lch.l, lch.c, lch.h);
    OKLAB { l, a, b, alpha: lch.alpha }
  }
}

impl From<OKLAB> for OKLCH {
  fn from(lab: OKLAB) -> OKLCH {
    let (l, c, h) = rectangular_to_polar(lab.l, lab.a, lab.b);
    OKLCH { l, c, h, alpha: lab.alpha }
  }
}

const D50: &[f32] = &[0.3457 / 0.3585, 1.00000, (1.0 - 0.3457 - 0.3585) / 0.3585];

impl From<LAB> for XYZd50 {
  fn from(lab: LAB) -> XYZd50 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L352
    const K: f32 = 24389.0 / 27.0;   // 29^3/3^3
    const E: f32 = 216.0 / 24389.0;  // 6^3/29^3

    let l = lab.l * 100.0;
    let a = lab.a;
    let b = lab.b;

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
    XYZd50 {
      x: x * D50[0],
      y: y * D50[1],
      z: z * D50[2],
      alpha: lab.alpha
    }
  }
}

impl From<XYZd50> for XYZd65 {
  fn from(xyz: XYZd50) -> XYZd65 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L319
    const MATRIX: &[f32] = &[
      0.9554734527042182,    -0.023098536874261423, 0.0632593086610217,
      -0.028369706963208136,  1.0099954580058226,   0.021041398966943008,
      0.012314001688319899,  -0.020507696433477912, 1.3303659366080753
    ];

    let (x, y, z) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    XYZd65 { x, y, z, alpha: xyz.alpha }
  }
}

impl From<XYZd65> for XYZd50 {
  fn from(xyz: XYZd65) -> XYZd50 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L319
    const MATRIX: &[f32] = &[
       1.0479298208405488,    0.022946793341019088,  -0.05019222954313557,
       0.029627815688159344,  0.990434484573249,     -0.01707382502938514,
      -0.009243058152591178,  0.015055144896577895,   0.7518742899580008
    ];

    let (x, y, z) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    XYZd50 { x, y, z, alpha: xyz.alpha }
  }
}

impl From<XYZd65> for SRGBLinear {
  fn from(xyz: XYZd65) -> SRGBLinear {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L62
    const MATRIX: &[f32] = &[
      3.2409699419045226,  -1.537383177570094,   -0.4986107602930034,
      -0.9692436362808796,   1.8759675015077202,   0.04155505740717559,
      0.05563007969699366, -0.20397695888897652,  1.0569715142428786
    ];

    let (r, g, b) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    SRGBLinear { r, g, b, alpha: xyz.alpha }
  }
}

#[inline]
fn multiply_matrix(m: &[f32], x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let a = m[0] * x + m[1] * y + m[2] * z;
  let b  = m[3] * x + m[4] * y + m[5] * z;
  let c = m[6] * x + m[7] * y + m[8] * z;
  (a, b, c)
}

impl From<SRGBLinear> for SRGB {
  #[inline]
  fn from(rgb: SRGBLinear) -> SRGB {
    let (r, g, b) = gam_srgb(rgb.r, rgb.g, rgb.b);
    SRGB { r, g, b, alpha: rgb.alpha }
  }
}

fn gam_srgb(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L31
  // convert an array of linear-light sRGB values in the range 0.0-1.0
  // to gamma corrected form
  // https://en.wikipedia.org/wiki/SRGB
  // Extended transfer function:
  // For negative values, linear portion extends on reflection
  // of axis, then uses reflected pow below that

  #[inline]
  fn gam_srgb_component(c: f32) -> f32 {
    let abs = c.abs();
    if abs > 0.0031308 {
      let sign = if c < 0.0 { -1.0 } else { 1.0 };
      return sign * (1.055 * abs.powf(1.0 / 2.4) - 0.055);
    }

    return 12.92 * c;
  }

  let r = gam_srgb_component(r);
  let g = gam_srgb_component(g);
  let b = gam_srgb_component(b);
  (r, g, b)
}

impl From<OKLAB> for XYZd65 {
  fn from(lab: OKLAB) -> XYZd65 {
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

    let (a, b, c) = multiply_matrix(OKLAB_TO_LMS, lab.l, lab.a, lab.b);
    let (x, y, z) = multiply_matrix(LMS_TO_XYZ, a.powi(3), b.powi(3), c.powi(3));
    XYZd65 { x, y, z, alpha: lab.alpha }
  }
}

impl From<XYZd65> for OKLAB {
  fn from(xyz: XYZd65) -> OKLAB {
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

    let (a, b, c) = multiply_matrix(XYZ_TO_LMS, xyz.x, xyz.y, xyz.z);
    let (l, a, b) = multiply_matrix(LMS_TO_OKLAB, a.cbrt(), b.cbrt(), c.cbrt());
    OKLAB { l, a, b, alpha: xyz.alpha }
  }
}

impl From<XYZd50> for LAB {
  fn from(xyz: XYZd50) -> LAB {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L332
    // Assuming XYZ is relative to D50, convert to CIE LAB
    // from CIE standard, which now defines these as a rational fraction
    const E: f32 = 216.0 / 24389.0;  // 6^3/29^3
    const K: f32 = 24389.0 / 27.0;   // 29^3/3^3

    // compute xyz, which is XYZ scaled relative to reference white
    let x = xyz.x / D50[0];
    let y = xyz.y / D50[1];
    let z = xyz.z / D50[2];

    // now compute f
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
    LAB { l, a, b, alpha: xyz.alpha }
  }
}

impl From<SRGB> for SRGBLinear {
  fn from(rgb: SRGB) -> SRGBLinear {
    let (r, g, b) = lin_srgb(rgb.r, rgb.g, rgb.b);
    SRGBLinear { r, g, b, alpha: rgb.alpha }
  }
}

fn lin_srgb(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L11
  // convert sRGB values where in-gamut values are in the range [0 - 1]
  // to linear light (un-companded) form.
  // https://en.wikipedia.org/wiki/SRGB
  // Extended transfer function:
  // for negative values, linear portion is extended on reflection of axis,
  // then reflected power function is used.

  #[inline]
  fn lin_srgb_component(c: f32) -> f32 {
    let abs = c.abs();
    if abs < 0.04045 {
      return c / 12.92;
    }
    
    let sign = if c < 0.0 { -1.0 } else { 1.0 };
    sign * ((abs + 0.055) / 1.055).powf(2.4)
  }

  let r = lin_srgb_component(r);
  let g = lin_srgb_component(g);
  let b = lin_srgb_component(b);
  (r, g, b)
}

impl From<SRGBLinear> for XYZd65 {
  fn from(rgb: SRGBLinear) -> XYZd65 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L50
    // convert an array of linear-light sRGB values to CIE XYZ
    // using sRGB's own white, D65 (no chromatic adaptation)
    const MATRIX: &[f32] = &[
      0.41239079926595934, 0.357584339383878,   0.1804807884018343,
      0.21263900587151027, 0.715168678767756,   0.07219231536073371,
      0.01933081871559182, 0.11919477979462598, 0.9505321522496607
    ];

    let (x, y, z) = multiply_matrix(MATRIX, rgb.r, rgb.g, rgb.b);
    XYZd65 { x, y, z, alpha: rgb.alpha }
  }
}

impl From<XYZd65> for P3 {
  fn from(xyz: XYZd65) -> P3 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L105
    const MATRIX: &[f32] = &[
      2.493496911941425,   -0.9313836179191239, -0.40271078445071684,
      -0.8294889695615747,   1.7626640603183463,  0.023624685841943577,
      0.03584583024378447, -0.07617238926804182, 0.9568845240076872
    ];

    let (r, g, b) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    let (r, g, b) = gam_srgb(r, g, b); // same as sRGB
    P3 { r, g, b, alpha: xyz.alpha }
  }
}

impl From<P3> for XYZd65 {
  fn from(p3: P3) -> XYZd65 {
    let (r, g, b) = lin_srgb(p3.r, p3.g, p3.b);

    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L91
    // convert linear-light display-p3 values to CIE XYZ
    // using D65 (no chromatic adaptation)
    // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    const MATRIX: &[f32] = &[
      0.4865709486482162, 0.26566769316909306, 0.1982172852343625,
      0.2289745640697488, 0.6917385218365064,  0.079286914093745,
      0.0000000000000000, 0.04511338185890264, 1.043944368900976,
    ];

    let (x, y, z) = multiply_matrix(MATRIX, r, g, b);
    XYZd65 { x, y, z, alpha: p3.alpha }
  }
}

impl From<A98> for XYZd65 {
  fn from(a98: A98) -> XYZd65 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L181
    #[inline]
    fn lin_a98rgb_component(c: f32) -> f32 {
      let sign = if c < 0.0 { -1.0 } else { 1.0 };
      sign * c.abs().powf(563.0 / 256.0)
    }

    // convert an array of a98-rgb values in the range 0.0 - 1.0
    // to linear light (un-companded) form.
    // negative values are also now accepted
    let r = lin_a98rgb_component(a98.r);
    let g = lin_a98rgb_component(a98.g);
    let b = lin_a98rgb_component(a98.b);

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

    let (x, y, z) = multiply_matrix(MATRIX, r, g, b);
    XYZd65 { x, y, z, alpha: a98.alpha }
  }
}

impl From<XYZd65> for A98 {
  fn from(xyz: XYZd65) -> A98 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L222
    // convert XYZ to linear-light a98-rgb
    const MATRIX: &[f32] = &[
       2.0415879038107465,    -0.5650069742788596,   -0.34473135077832956,
      -0.9692436362808795,     1.8759675015077202,    0.04155505740717557,
       0.013444280632031142,  -0.11836239223101838,   1.0151749943912054
    ];

    #[inline]
    fn gam_a98_component(c: f32) -> f32 {
      // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L193
      // convert linear-light a98-rgb  in the range 0.0-1.0
      // to gamma corrected form
      // negative values are also now accepted
      let sign = if c < 0.0 { -1.0 } else { 1.0 };
      sign * c.abs().powf(256.0 / 563.0)
    }

    let (r, g, b) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    let r = gam_a98_component(r);
    let g = gam_a98_component(g);
    let b = gam_a98_component(b);
    A98 { r, g, b, alpha: xyz.alpha }
  }
}

impl From<ProPhoto> for XYZd50 {
  fn from(prophoto: ProPhoto) -> XYZd50 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L118
    // convert an array of prophoto-rgb values
    // where in-gamut colors are in the range [0.0 - 1.0]
    // to linear light (un-companded) form.
    // Transfer curve is gamma 1.8 with a small linear portion
    // Extended transfer function

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

    let r = lin_prophoto_component(prophoto.r);
    let g = lin_prophoto_component(prophoto.g);
    let b = lin_prophoto_component(prophoto.b);

    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L155
    // convert an array of linear-light prophoto-rgb values to CIE XYZ
    // using  D50 (so no chromatic adaptation needed afterwards)
    // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    const MATRIX: &[f32] = &[
      0.7977604896723027,  0.13518583717574031,  0.0313493495815248,
      0.2880711282292934,  0.7118432178101014,   0.00008565396060525902,
      0.0,                 0.0,                  0.8251046025104601
    ];

    let (x, y, z) = multiply_matrix(MATRIX, r, g, b);
    XYZd50 { x, y, z, alpha: prophoto.alpha }
  }
}

impl From<XYZd50> for ProPhoto {
  fn from(xyz: XYZd50) -> ProPhoto {
    // convert XYZ to linear-light prophoto-rgb
    const MATRIX: &[f32] = &[
       1.3457989731028281,  -0.25558010007997534,  -0.05110628506753401,
      -0.5446224939028347,   1.5082327413132781,    0.02053603239147973,
       0.0,                  0.0,                   1.2119675456389454
    ];

    #[inline]
    fn gam_prophoto_component(c: f32) -> f32 {
      // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L137
      // convert linear-light prophoto-rgb  in the range 0.0-1.0
      // to gamma corrected form
      // Transfer curve is gamma 1.8 with a small linear portion
      // TODO for negative values, extend linear portion on reflection of axis, then add pow below that
      const ET: f32 = 1.0 / 512.0;
      let abs = c.abs();
      if abs >= ET {
        let sign = if c < 0.0 { -1.0 } else { 1.0 };
        return sign * abs.powf(1.0 / 1.8);
      }

      16.0 * c
    }

    let (r, g, b) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    let r = gam_prophoto_component(r);
    let g = gam_prophoto_component(g);
    let b = gam_prophoto_component(b);
    ProPhoto { r, g, b, alpha: xyz.alpha }
  }
}

impl From<Rec2020> for XYZd65 {
  fn from(rec2020: Rec2020) -> XYZd65 {
    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L235
    // convert an array of rec2020 RGB values in the range 0.0 - 1.0
    // to linear light (un-companded) form.
    // ITU-R BT.2020-2 p.4

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

    let r = lin_rec2020_component(rec2020.r);
    let g = lin_rec2020_component(rec2020.g);
    let b = lin_rec2020_component(rec2020.b);

    // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L276
    // convert an array of linear-light rec2020 values to CIE XYZ
    // using  D65 (no chromatic adaptation)
    // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    const MATRIX: &[f32] = &[
      0.6369580483012914, 0.14461690358620832,  0.1688809751641721,
      0.2627002120112671, 0.6779980715188708,   0.05930171646986196,
      0.000000000000000,  0.028072693049087428, 1.060985057710791
    ];

    let (x, y, z) = multiply_matrix(MATRIX, r, g, b);
    XYZd65 { x, y, z, alpha: rec2020.alpha }
  }
}

impl From<XYZd65> for Rec2020 {
  fn from(xyz: XYZd65) -> Rec2020 {
    // convert XYZ to linear-light rec2020
    const MATRIX: &[f32] = &[
       1.7166511879712674,   -0.35567078377639233, -0.25336628137365974,
      -0.6666843518324892,    1.6164812366349395,   0.01576854581391113,
       0.017639857445310783, -0.042770613257808524, 0.9421031212354738
    ];

    #[inline]
    fn gam_rec2020_component(c: f32) -> f32 {
      // convert linear-light rec2020 RGB  in the range 0.0-1.0
      // to gamma corrected form
      // ITU-R BT.2020-2 p.4

      const A: f32 = 1.09929682680944;
      const B: f32 = 0.018053968510807;

      let abs = c.abs();
      if abs > B {
        let sign = if c < 0.0 { -1.0 } else { 1.0 };
        return sign * (A * abs.powf(0.45) - (A - 1.0));
      }

      4.5 * c
    }

    let (r, g, b) = multiply_matrix(MATRIX, xyz.x, xyz.y, xyz.z);
    let r = gam_rec2020_component(r);
    let g = gam_rec2020_component(g);
    let b = gam_rec2020_component(b);
    Rec2020 { r, g, b, alpha: xyz.alpha }
  }
}

impl From<SRGB> for HSL {
  fn from(rgb: SRGB) -> HSL {
    // https://drafts.csswg.org/css-color/#rgb-to-hsl
    let r = rgb.r;
    let g = rgb.g;
    let b = rgb.b;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let mut h = f32::NAN;
    let mut s: f32 = 0.0;
    let l = (min + max) / 2.0;
    let d = max - min;

    if d != 0.0 {
      s = if l == 0.0 || l == 1.0 {
        0.0
      } else {
        (max - l) / l.min(1.0 - l)
      };

      if max == r {
        h = (g - b) / d + (if g < b { 6.0 } else { 0.0 });
      } else if max == g {
        h = (b - r) / d + 2.0;
      } else if max == b {
        h = (r - g) / d + 4.0;
      }

      h = h * 60.0;
    }

    HSL { h, s, l, alpha: rgb.alpha }
  }
}

impl From<HSL> for SRGB {
  fn from(hsl: HSL) -> SRGB {
    // https://drafts.csswg.org/css-color/#hsl-to-rgb
    let mut h = hsl.h % 360.0;
    if h < 0.0 {
      h += 360.0;
    }

    #[inline]
    fn hue_to_rgb(n: f32, h: f32, s: f32, l: f32) -> f32 {
      let k = (n + h / 30.0) % 12.0;
      let a = s * l.min(1.0 - l);
      l - a * (k - 3.0).min(9.0 - k).clamp(-1.0, 1.0)
    }

    let r = hue_to_rgb(0.0, h, hsl.s, hsl.l);
    let g = hue_to_rgb(8.0, h, hsl.s, hsl.l);
    let b = hue_to_rgb(4.0, h, hsl.s, hsl.l);
    SRGB { r, g, b, alpha: hsl.alpha }
  }
}

impl From<SRGB> for HWB {
  fn from(rgb: SRGB) -> HWB {
    let hsl = HSL::from(rgb);
    let r = rgb.r;
    let g = rgb.g;
    let b = rgb.b;
    let w = r.min(g).min(b);
    let b = 1.0 - r.max(g).max(b);
    HWB { h: hsl.h, w, b, alpha: rgb.alpha } 
  }
}

impl From<HWB> for SRGB {
  fn from(hwb: HWB) -> SRGB {
    // https://drafts.csswg.org/css-color/#hwb-to-rgb
    let h = hwb.h;
    let w = hwb.w;
    let b = hwb.b;

    if w + b >= 1.0 {
      let gray = w / (w + b);
      return SRGB { r: gray, g: gray, b: gray, alpha: hwb.alpha }
    }

    let mut rgba = SRGB::from(HSL { h, s: 1.0, l: 0.5, alpha: hwb.alpha });
    let x = 1.0 - w - b;
    rgba.r = rgba.r * x + w;
    rgba.g = rgba.g * x + w;
    rgba.b = rgba.b * x + w;
    rgba
  }
}

impl From<RGBA> for SRGB {
  fn from(rgb: RGBA) -> SRGB {
    SRGB {
      r: rgb.red_f32(),
      g: rgb.green_f32(),
      b: rgb.blue_f32(),
      alpha: rgb.alpha_f32()
    }
  }
}

impl From<SRGB> for RGBA {
  fn from(rgb: SRGB) -> RGBA {
    RGBA::from_floats(rgb.r, rgb.g, rgb.b, rgb.alpha)
  }
}

// Once Rust specialization is stable, this could be simplified.
via!(LAB -> XYZd50 -> XYZd65);
via!(ProPhoto -> XYZd50 -> XYZd65);
via!(OKLCH -> OKLAB -> XYZd65);

via!(LAB -> XYZd65 -> OKLAB);
via!(LAB -> XYZd65 -> OKLCH);
via!(LAB -> XYZd65 -> SRGB);
via!(LAB -> XYZd65 -> SRGBLinear);
via!(LAB -> XYZd65 -> P3);
via!(LAB -> XYZd65 -> A98);
via!(LAB -> XYZd65 -> ProPhoto);
via!(LAB -> XYZd65 -> Rec2020);
via!(LAB -> XYZd65 -> HSL);
via!(LAB -> XYZd65 -> HWB);

via!(LCH -> LAB -> XYZd65);
via!(LCH -> XYZd65 -> OKLAB);
via!(LCH -> XYZd65 -> OKLCH);
via!(LCH -> XYZd65 -> SRGB);
via!(LCH -> XYZd65 -> SRGBLinear);
via!(LCH -> XYZd65 -> P3);
via!(LCH -> XYZd65 -> A98);
via!(LCH -> XYZd65 -> ProPhoto);
via!(LCH -> XYZd65 -> Rec2020);
via!(LCH -> XYZd65 -> XYZd50);
via!(LCH -> XYZd65 -> HSL);
via!(LCH -> XYZd65 -> HWB);

via!(SRGB -> SRGBLinear -> XYZd65);
via!(SRGB -> XYZd65 -> OKLAB);
via!(SRGB -> XYZd65 -> OKLCH);
via!(SRGB -> XYZd65 -> P3);
via!(SRGB -> XYZd65 -> A98);
via!(SRGB -> XYZd65 -> ProPhoto);
via!(SRGB -> XYZd65 -> Rec2020);
via!(SRGB -> XYZd65 -> XYZd50);

via!(P3 -> XYZd65 -> SRGBLinear);
via!(P3 -> XYZd65 -> OKLAB);
via!(P3 -> XYZd65 -> OKLCH);
via!(P3 -> XYZd65 -> A98);
via!(P3 -> XYZd65 -> ProPhoto);
via!(P3 -> XYZd65 -> Rec2020);
via!(P3 -> XYZd65 -> XYZd50);
via!(P3 -> XYZd65 -> HSL);
via!(P3 -> XYZd65 -> HWB);

via!(SRGBLinear -> XYZd65 -> OKLAB);
via!(SRGBLinear -> XYZd65 -> OKLCH);
via!(SRGBLinear -> XYZd65 -> A98);
via!(SRGBLinear -> XYZd65 -> ProPhoto);
via!(SRGBLinear -> XYZd65 -> Rec2020);
via!(SRGBLinear -> XYZd65 -> XYZd50);
via!(SRGBLinear -> XYZd65 -> HSL);
via!(SRGBLinear -> XYZd65 -> HWB);

via!(A98 -> XYZd65 -> OKLAB);
via!(A98 -> XYZd65 -> OKLCH);
via!(A98 -> XYZd65 -> ProPhoto);
via!(A98 -> XYZd65 -> Rec2020);
via!(A98 -> XYZd65 -> XYZd50);
via!(A98 -> XYZd65 -> HSL);
via!(A98 -> XYZd65 -> HWB);

via!(ProPhoto -> XYZd65 -> OKLAB);
via!(ProPhoto -> XYZd65 -> OKLCH);
via!(ProPhoto -> XYZd65 -> Rec2020);
via!(ProPhoto -> XYZd65 -> HSL);
via!(ProPhoto -> XYZd65 -> HWB);

via!(XYZd50 -> XYZd65 -> OKLAB);
via!(XYZd50 -> XYZd65 -> OKLCH);
via!(XYZd50 -> XYZd65 -> Rec2020);
via!(XYZd50 -> XYZd65 -> HSL);
via!(XYZd50 -> XYZd65 -> HWB);

via!(Rec2020 -> XYZd65 -> OKLAB);
via!(Rec2020 -> XYZd65 -> OKLCH);
via!(Rec2020 -> XYZd65 -> HSL);
via!(Rec2020 -> XYZd65 -> HWB);

via!(HSL -> XYZd65 -> OKLAB);
via!(HSL -> XYZd65 -> OKLCH);
via!(HSL -> SRGB -> XYZd65);
via!(HSL -> SRGB -> HWB);

via!(HWB -> SRGB -> XYZd65);
via!(HWB -> XYZd65 -> OKLAB);
via!(HWB -> XYZd65 -> OKLCH);

// RGBA is an 8-bit version. Convert to SRGB, which is a 
// more accurate floating point representation for all operations.
via!(RGBA -> SRGB -> LAB);
via!(RGBA -> SRGB -> LCH);
via!(RGBA -> SRGB -> OKLAB);
via!(RGBA -> SRGB -> OKLCH);
via!(RGBA -> SRGB -> P3);
via!(RGBA -> SRGB -> SRGBLinear);
via!(RGBA -> SRGB -> A98);
via!(RGBA -> SRGB -> ProPhoto);
via!(RGBA -> SRGB -> XYZd50);
via!(RGBA -> SRGB -> XYZd65);
via!(RGBA -> SRGB -> Rec2020);
via!(RGBA -> SRGB -> HSL);
via!(RGBA -> SRGB -> HWB);

macro_rules! color_space {
  ($space: ty) => {
    impl From<LABColor> for $space {
      fn from(color: LABColor) -> $space {
        use LABColor::*;
    
        match color {
          LAB(v) => v.into(),
          LCH(v) => v.into(),
          OKLAB(v) => v.into(),
          OKLCH(v) => v.into()
        }
      }
    }

    impl From<PredefinedColor> for $space {
      fn from(color: PredefinedColor) -> $space {
        use PredefinedColor::*;

        match color {
          SRGB(v) => v.into(),
          SRGBLinear(v) => v.into(),
          DisplayP3(v) => v.into(),
          A98(v) => v.into(),
          ProPhoto(v) => v.into(),
          Rec2020(v) => v.into(),
          XYZd50(v) => v.into(),
          XYZd65(v) => v.into()
        }
      }
    }

    impl From<&CssColor> for $space {
      fn from(color: &CssColor) -> $space {
        match color {
          CssColor::RGBA(rgba) => (*rgba).into(),
          CssColor::LAB(lab) => (**lab).into(),
          CssColor::Predefined(predefined) => (**predefined).into(),
          CssColor::CurrentColor => unreachable!()
        }
      }
    }
  };
}

color_space!(LAB);
color_space!(LCH);
color_space!(OKLAB);
color_space!(OKLCH);
color_space!(SRGB);
color_space!(SRGBLinear);
color_space!(XYZd50);
color_space!(XYZd65);
color_space!(P3);
color_space!(A98);
color_space!(ProPhoto);
color_space!(Rec2020);
color_space!(HSL);
color_space!(HWB);
color_space!(RGBA);

macro_rules! predefined {
  ($key: ident, $t: ty) => {
    impl From<$t> for PredefinedColor {
      fn from(color: $t) -> PredefinedColor {
        PredefinedColor::$key(color)
      }
    }

    impl From<$t> for CssColor {
      fn from(color: $t) -> CssColor {
        CssColor::Predefined(Box::new(PredefinedColor::$key(color)))
      }
    }
  }
}

predefined!(SRGBLinear, SRGBLinear);
predefined!(XYZd50, XYZd50);
predefined!(XYZd65, XYZd65);
predefined!(DisplayP3, P3);
predefined!(A98, A98);
predefined!(ProPhoto, ProPhoto);
predefined!(Rec2020, Rec2020);

macro_rules! lab {
  ($key: ident, $t: ty) => {
    impl From<$t> for LABColor {
      fn from(color: $t) -> LABColor {
        LABColor::$key(color)
      }
    }

    impl From<$t> for CssColor {
      fn from(color: $t) -> CssColor {
        CssColor::LAB(Box::new(LABColor::$key(color)))
      }
    }
  }
}

lab!(LAB, LAB);
lab!(LCH, LCH);
lab!(OKLAB, OKLAB);
lab!(OKLCH, OKLCH);

macro_rules! rgb {
  ($t: ty) => {
    impl From<$t> for CssColor {
      fn from(color: $t) -> CssColor {
        // TODO: should we serialize as color(srgb, ...)?
        // would be more precise than 8-bit color.
        CssColor::RGBA(color.into())
      }
    }
  }
}

rgb!(SRGB);
rgb!(HSL);
rgb!(HWB);

impl From<RGBA> for CssColor {
  fn from(color: RGBA) -> CssColor {
    CssColor::RGBA(color)
  }
}

pub trait ColorGamut {
  fn in_gamut(&self) -> bool;
  fn clip(&self) -> Self;
}

macro_rules! bounded_color_gamut {
  ($t: ty, $a: ident, $b: ident, $c: ident) => {
    impl ColorGamut for $t {
      fn in_gamut(&self) -> bool {
        self.$a >= 0.0 && self.$a <= 1.0 &&
        self.$b >= 0.0 && self.$b <= 1.0 &&
        self.$c >= 0.0 && self.$c <= 1.0
      }

      fn clip(&self) -> Self {
        Self {
          $a: self.$a.clamp(0.0, 1.0),
          $b: self.$b.clamp(0.0, 1.0),
          $c: self.$c.clamp(0.0, 1.0),
          alpha: self.alpha.clamp(0.0, 1.0)
        }
      }
    }
  }
}

macro_rules! unbounded_color_gamut {
  ($t: ty, $a: ident, $b: ident, $c: ident) => {
    impl ColorGamut for $t {
      fn in_gamut(&self) -> bool {
        true
      }

      fn clip(&self) -> Self {
        *self
      }
    }
  }
}

macro_rules! hsl_hwb_color_gamut {
  ($t: ty, $a: ident, $b: ident) => {
    impl ColorGamut for $t {
      fn in_gamut(&self) -> bool {
        // self.h >= 0.0 && self.h <= 360.0 &&
        self.$a >= 0.0 && self.$a <= 1.0 &&
        self.$b >= 0.0 && self.$b <= 1.0
      }

      fn clip(&self) -> Self {
        Self {
          h: self.h,//.clamp(0.0, 360.0),
          $a: self.$a.clamp(0.0, 1.0),
          $b: self.$b.clamp(0.0, 1.0),
          alpha: self.alpha.clamp(0.0, 1.0)
        }
      }
    }
  }
}

bounded_color_gamut!(SRGB, r, g, b);
bounded_color_gamut!(SRGBLinear, r, g, b);
bounded_color_gamut!(P3, r, g, b);
bounded_color_gamut!(A98, r, g, b);
bounded_color_gamut!(ProPhoto, r, g, b);
bounded_color_gamut!(Rec2020, r, g,b);
unbounded_color_gamut!(LAB, l, a, b);
unbounded_color_gamut!(OKLAB, l, a, b);
unbounded_color_gamut!(XYZd50, x, y, z);
unbounded_color_gamut!(XYZd65, x, y, z);
unbounded_color_gamut!(LCH, l, c, h);
unbounded_color_gamut!(OKLCH, l, c, h);
hsl_hwb_color_gamut!(HSL, s, l);
hsl_hwb_color_gamut!(HWB, w, b);

fn delta_eok<T: Into<OKLAB>>(a: T, b: OKLCH) -> f32 {
  // https://www.w3.org/TR/css-color-4/#color-difference-OK
  let a: OKLAB = a.into();
  let b: OKLAB = b.into();
  let delta_l = a.l - b.l;
  let delta_a = a.a - b.a;
  let delta_b = a.b - b.b;
  
  (delta_l.powi(2) + delta_a.powi(2) + delta_b.powi(2)).sqrt()
}

fn map_gamut<T>(color: T) -> T
where
  T: 'static + Into<OKLCH> + ColorGamut + Into<OKLAB> + From<OKLCH> + Copy + Into<SRGB> + From<SRGB>
{
  const JND: f32 = 0.02;
  const EPSILON: f32 = 0.00001;

  // The web platform tests seem to rely on gamut mapping for HSL and HSB occurring in the sRGB space.
  // Not sure if this is correct, but for now convert to sRGB first, and back after.
  // https://github.com/w3c/csswg-drafts/issues/7107
  let type_id = TypeId::of::<T>();
  if type_id == TypeId::of::<HSL>() || type_id == TypeId::of::<HWB>() {
    let srgb: SRGB = color.into();
    return map_gamut(srgb).into();
  }

  // https://www.w3.org/TR/css-color-4/#binsearch
  let mut current: OKLCH = color.into();

  // If lightness is >= 100%, return pure white.
  if (current.l - 1.0).abs() < EPSILON || current.l > 1.0 {
    return OKLCH { l: 1.0, c: 0.0, h: 0.0, alpha: current.alpha }.into();
  }

  // If lightness <= 0%, return pure black.
  if current.l < EPSILON {
    return OKLCH { l: 0.0, c: 0.0, h: 0.0, alpha: current.alpha }.into();
  }

  let mut min = 0.0;
  let mut max = current.c;

  while min < max {
    let chroma = (min + max) / 2.0;
    current.c = chroma;

    let converted = T::from(current);
    if converted.in_gamut() {
      min = chroma;
      continue
    }

    let clipped = converted.clip();
    let delta_e = delta_eok(clipped, current);
    if delta_e < JND {
      return clipped.into();
    }

    max = chroma;
  }

  current.into()
}

fn parse_color_mix<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CssColor, ParseError<'i, ParserError<'i>>> {
  input.expect_ident_matching("in")?;
  let method = ColorSpace::parse(input)?;

  let hue_method = if matches!(method, ColorSpace::Hsl | ColorSpace::Hwb | ColorSpace::LCH | ColorSpace::OKLCH) {
    let hue_method = input.try_parse(HueInterpolationMethod::parse);
    if hue_method.is_ok() {
      input.expect_ident_matching("hue")?;
    }
    hue_method
  } else {
    Ok(HueInterpolationMethod::Shorter)
  };

  let hue_method = hue_method.unwrap_or(HueInterpolationMethod::Shorter);
  input.expect_comma()?;

  let first_percent = input.try_parse(|input| input.expect_percentage());
  let first_color = CssColor::parse(input)?;
  let first_percent = first_percent.or_else(|_| input.try_parse(|input| input.expect_percentage())).ok();
  input.expect_comma()?;

  let second_percent = input.try_parse(|input| input.expect_percentage());
  let second_color = CssColor::parse(input)?;
  let second_percent = second_percent.or_else(|_| input.try_parse(|input| input.expect_percentage())).ok();

  // https://drafts.csswg.org/css-color-5/#color-mix-percent-norm
  let (p1, p2) = if first_percent.is_none() && second_percent.is_none() {
    (0.5, 0.5)
  } else {
    let p2 = second_percent.unwrap_or_else(|| 1.0 - first_percent.unwrap());
    let p1 = first_percent.unwrap_or_else(|| 1.0 - second_percent.unwrap());
    (p1, p2)
  };

  if (p1 + p2) == 0.0 {
    return Err(input.new_custom_error(ParserError::InvalidValue));
  }

  Ok(match method {
    ColorSpace::SRGB => first_color.interpolate::<SRGB>(p1, &second_color, p2, hue_method),
    ColorSpace::SRGBLinear => first_color.interpolate::<SRGBLinear>(p1, &second_color, p2, hue_method),
    ColorSpace::Hsl => first_color.interpolate::<HSL>(p1, &second_color, p2, hue_method),
    ColorSpace::Hwb => first_color.interpolate::<HWB>(p1, &second_color, p2, hue_method),
    ColorSpace::LAB => first_color.interpolate::<LAB>(p1, &second_color, p2, hue_method),
    ColorSpace::LCH => first_color.interpolate::<LCH>(p1, &second_color, p2, hue_method),
    ColorSpace::OKLAB => first_color.interpolate::<OKLAB>(p1, &second_color, p2, hue_method),
    ColorSpace::OKLCH => first_color.interpolate::<OKLCH>(p1, &second_color, p2, hue_method),
    ColorSpace::XYZ | ColorSpace::XYZd65 => first_color.interpolate::<XYZd65>(p1, &second_color, p2, hue_method),
    ColorSpace::XYZd50 => first_color.interpolate::<XYZd50>(p1, &second_color, p2, hue_method),
  })
}

impl CssColor {
  fn get_type_id(&self) -> TypeId {
    match self {
      CssColor::RGBA(..) => TypeId::of::<SRGB>(),
      CssColor::LAB(lab) => {
        match &**lab {
          LABColor::LAB(..) => TypeId::of::<LAB>(),
          LABColor::LCH(..) => TypeId::of::<LCH>(),
          LABColor::OKLAB(..) => TypeId::of::<OKLAB>(),
          LABColor::OKLCH(..) => TypeId::of::<OKLCH>()
        }
      }
      CssColor::Predefined(predefined) => {
        match &**predefined {
          PredefinedColor::SRGB(..) => TypeId::of::<SRGB>(),
          PredefinedColor::SRGBLinear(..) => TypeId::of::<SRGBLinear>(),
          PredefinedColor::DisplayP3(..) => TypeId::of::<P3>(),
          PredefinedColor::A98(..) => TypeId::of::<A98>(),
          PredefinedColor::ProPhoto(..) => TypeId::of::<ProPhoto>(),
          PredefinedColor::Rec2020(..) => TypeId::of::<Rec2020>(),
          PredefinedColor::XYZd50(..) => TypeId::of::<XYZd50>(),
          PredefinedColor::XYZd65(..) => TypeId::of::<XYZd65>(),
        }
      }
      _ => unreachable!()
    }
  }

  pub fn interpolate<'a, T>(
    &'a self,
    mut p1: f32,
    second_color: &'a CssColor,
    mut p2: f32,
    method: HueInterpolationMethod
  ) -> CssColor
    where
      T: 'static + From<&'a CssColor> + Interpolate + Into<CssColor>
        + Into<OKLCH> + ColorGamut + Into<OKLAB> + From<OKLCH> + Copy
        + Into<SRGB> + From<SRGB>
  {
    let type_id = TypeId::of::<T>();
    let is_converted = self.get_type_id() != type_id || second_color.get_type_id() != type_id;

    // https://drafts.csswg.org/css-color-5/#color-mix-result
    let mut first_color = T::from(self);
    let mut second_color = T::from(second_color);

    if !first_color.in_gamut() {
      first_color = map_gamut(first_color);
    }

    if !second_color.in_gamut() {
      second_color = map_gamut(second_color);
    }

    // https://www.w3.org/TR/css-color-4/#powerless
    if is_converted {
      first_color.adjust_powerless_components(&second_color);
      second_color.adjust_powerless_components(&first_color);
    }

    // https://www.w3.org/TR/css-color-4/#hue-interpolation
    first_color.adjust_hue(&mut second_color, method);

    // https://www.w3.org/TR/css-color-4/#interpolation-alpha
    first_color.premultiply();
    second_color.premultiply();

    // https://drafts.csswg.org/css-color-5/#color-mix-percent-norm
    let mut alpha_multiplier = p1 + p2;
    if alpha_multiplier != 1.0 {
      p1 = p1 / alpha_multiplier;
      p2 = p2 / alpha_multiplier;
      if alpha_multiplier > 1.0 {
        alpha_multiplier = 1.0;
      }
    }

    let mut result_color = first_color.interpolate(p1, &second_color, p2);
    result_color.unpremultiply(alpha_multiplier);

    result_color.into()
  }
}

pub trait Interpolate {
  fn adjust_powerless_components(&mut self, _: &Self) {}
  fn adjust_hue(&mut self, _: &mut Self, _: HueInterpolationMethod) {}
  fn premultiply(&mut self);
  fn unpremultiply(&mut self, alpha_multiplier: f32);
  fn interpolate(&self, p1: f32, other: &Self, p2: f32) -> Self;
}

macro_rules! interpolate {
  ($a: ident, $b: ident, $c: ident) => {
    fn interpolate(&self, p1: f32, other: &Self, p2: f32) -> Self {
      Self {
        $a: self.$a * p1 + other.$a * p2,
        $b: self.$b * p1 + other.$b * p2,
        $c: self.$c * p1 + other.$c * p2,
        alpha: self.alpha * p1 + other.alpha * p2,
      }
    }
  }
}

macro_rules! rectangular_premultiply {
  ($a: ident, $b: ident, $c: ident) => {
    fn premultiply(&mut self) {
      self.$a *= self.alpha;
      self.$b *= self.alpha;
      self.$c *= self.alpha;
    }

    fn unpremultiply(&mut self, alpha_multiplier: f32) {
      self.$a /= self.alpha;
      self.$b /= self.alpha;
      self.$c /= self.alpha;
      self.alpha *= alpha_multiplier;
    }
  }
}

macro_rules! polar_premultiply {
  ($a: ident, $b: ident) => {
    fn premultiply(&mut self) {
      self.$a *= self.alpha;
      self.$b *= self.alpha;
    }

    fn unpremultiply(&mut self, alpha_multiplier: f32) {
      self.$a /= self.alpha;
      self.$b /= self.alpha;
      self.h %= 360.0;
      self.alpha *= alpha_multiplier;
    }
  }
}

macro_rules! adjust_powerless_lab {
  () => {
    fn adjust_powerless_components(&mut self, other: &Self) {
      // If the lightness of a LAB color is 0%, both the a and b components are powerless.
      if self.l.abs() < f32::EPSILON {
        self.a = other.a;
        self.b = other.b;
      }
    }
  }
}

macro_rules! adjust_powerless_lch {
  () => {
    fn adjust_powerless_components(&mut self, other: &Self) {
      // If the chroma of an LCH color is 0%, the hue component is powerless. 
      // If the lightness of an LCH color is 0%, both the hue and chroma components are powerless.
      if self.c.abs() < f32::EPSILON {
        self.h = other.h;
      }

      if self.l.abs() < f32::EPSILON {
        self.c = other.c;
        self.h = other.h;
      }
    }

    fn adjust_hue(&mut self, other: &mut Self, method: HueInterpolationMethod) {
      method.interpolate(&mut self.h, &mut other.h);
    }
  }
}

impl Interpolate for SRGB {
  rectangular_premultiply!(r, g, b);
  interpolate!(r, g, b);
}

impl Interpolate for SRGBLinear {
  rectangular_premultiply!(r, g, b);
  interpolate!(r, g, b);
}

impl Interpolate for XYZd50 {
  rectangular_premultiply!(x, y, z);
  interpolate!(x, y, z);
}

impl Interpolate for XYZd65 {
  rectangular_premultiply!(x, y, z);
  interpolate!(x, y, z);
}

impl Interpolate for LAB {
  adjust_powerless_lab!();
  rectangular_premultiply!(l, a, b);
  interpolate!(l, a, b);
}

impl Interpolate for OKLAB {
  adjust_powerless_lab!();
  rectangular_premultiply!(l, a, b);
  interpolate!(l, a, b);
}

impl Interpolate for LCH {
  adjust_powerless_lch!();
  polar_premultiply!(l, c);
  interpolate!(l, c, h);
}

impl Interpolate for OKLCH {
  adjust_powerless_lch!();
  polar_premultiply!(l, c);
  interpolate!(l, c, h);
}

impl Interpolate for HSL {
  polar_premultiply!(s, l);

  fn adjust_powerless_components(&mut self, other: &Self) {
    // If the saturation of an HSL color is 0%, then the hue component is powerless.
    // If the lightness of an HSL color is 0% or 100%, both the saturation and hue components are powerless.
    if self.s.abs() < f32::EPSILON {
      self.h = other.h;
    }

    if self.l.abs() < f32::EPSILON || (self.l - 1.0).abs() < f32::EPSILON {
      self.h = other.h;
      self.s = other.s;
    }
  }

  fn adjust_hue(&mut self, other: &mut Self, method: HueInterpolationMethod) {
    method.interpolate(&mut self.h, &mut other.h);
  }

  interpolate!(h, s, l);
}

impl Interpolate for HWB {
  polar_premultiply!(w, b);

  fn adjust_powerless_components(&mut self, other: &Self) {
    // If white+black is equal to 100% (after normalization), it defines an achromatic color, 
    // i.e. some shade of gray, without any hint of the chosen hue. In this case, the hue component is powerless.
    if (self.w + self.b - 1.0).abs() < f32::EPSILON {
      self.h = other.h;
    }
  }

  fn adjust_hue(&mut self, other: &mut Self, method: HueInterpolationMethod) {
    method.interpolate(&mut self.h, &mut other.h);
  }

  interpolate!(h, w, b);
}

impl HueInterpolationMethod {
  fn interpolate(&self, a: &mut f32, b: &mut f32) {
    // https://drafts.csswg.org/css-color/#hue-interpolation
    if *self != HueInterpolationMethod::Specified {
      *a = ((*a % 360.0) + 360.0) % 360.0;
      *b = ((*b % 360.0) + 360.0) % 360.0;
    }

    match self {
      HueInterpolationMethod::Shorter => {
        // https://www.w3.org/TR/css-color-4/#hue-shorter
        let delta = *b - *a;
        if delta > 180.0 {
          *a += 360.0;
        } else if delta < -180.0 {
          *b += 360.0;
        }
      }
      HueInterpolationMethod::Longer => {
        // https://www.w3.org/TR/css-color-4/#hue-longer
        let delta = *b - *a;
        if 0.0 < delta && delta < 180.0 {
          *a += 360.0;
        } else if -180.0 < delta && delta < 0.0 {
          *b += 360.0;
        }
      }
      HueInterpolationMethod::Increasing => {
        // https://www.w3.org/TR/css-color-4/#hue-increasing
        if *b < *a {
          *b += 360.0;
        }
      }
      HueInterpolationMethod::Decreasing => {
        // https://www.w3.org/TR/css-color-4/#hue-decreasing
        if *a < *b {
          *a += 360.0;
        }
      }
      HueInterpolationMethod::Specified => {}
    }
  }
}
