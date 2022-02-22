use cssparser::*;
use crate::targets::Browsers;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use std::f32::consts::PI;
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
  Oklch(f32, f32, f32, f32),
  Predefined(PredefinedColor)
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

impl CssColor {
  pub fn current_color() -> CssColor {
    CssColor::CurrentColor
  }

  pub fn transparent() -> CssColor {
    CssColor::RGBA(RGBA::transparent())
  }

  pub fn to_rgb(&self) -> CssColor {
    match self {
      CssColor::Lab(l, a, b, alpha) => {
        let (r, g, b) = lab_to_srgb(*l * 100.0, *a, *b);
        CssColor::RGBA(RGBA::from_floats(r, g, b, *alpha))
      },
      CssColor::Lch(l, c, h, alpha) => {
        let (r, g, b) = lch_to_srgb(*l * 100.0, *c, *h);
        CssColor::RGBA(RGBA::from_floats(r, g, b, *alpha))
      },
      CssColor::Oklab(l, a, b, alpha) => {
        let (r, g, b) = oklab_to_srgb(*l, *a, *b);
        CssColor::RGBA(RGBA::from_floats(r, g, b, *alpha))
      },
      CssColor::Oklch(l, c, h, alpha) => {
        let (r, g, b) = oklch_to_srgb(*l, *c, *h);
        CssColor::RGBA(RGBA::from_floats(r, g, b, *alpha))
      },
      CssColor::RGBA(..) | CssColor::CurrentColor => self.clone(),
      _ => todo!()
    }
  }

  pub fn to_lab(&self) -> CssColor {
    match self {
      CssColor::RGBA(rgba) => {
        let (l, a, b) = srgb_to_lab(rgba.red_f32(), rgba.green_f32(), rgba.blue_f32());
        CssColor::Lab(l, a, b, rgba.alpha_f32())
      },
      CssColor::Lch(l, c, h, alpha) => {
        let (l, a, b) = lch_to_lab(*l, *c, *h);
        CssColor::Lab(l, a, b, *alpha)
      },
      CssColor::Oklab(l, a, b, alpha) => {
        let (l, a, b) = oklab_to_lab(*l, *a, *b);
        CssColor::Lab(l, a, b, *alpha)
      },
      CssColor::Oklch(l, c, h, alpha) => {
        let (l, a, b) = oklch_to_lab(*l, *c, *h);
        CssColor::Lab(l, a, b, *alpha)
      },
      CssColor::Lab(..) | CssColor::CurrentColor => self.clone(),
      _ => todo!()
    }
  }

  pub fn get_fallbacks(&mut self, targets: Browsers) -> Vec<CssColor> {
    let is_compatible = match self {
      CssColor::CurrentColor | CssColor::RGBA(_) => true,
      CssColor::Lab(..) => Feature::LabColors.is_compatible(targets),
      CssColor::Lch(..) => Feature::LchColors.is_compatible(targets),
      CssColor::Oklab(..) => Feature::OklabColors.is_compatible(targets),
      CssColor::Oklch(..) => Feature::OklchColors.is_compatible(targets),
      CssColor::Predefined(..) => Feature::ColorFunction.is_compatible(targets)
    };

    let mut res = Vec::new();
    if !is_compatible {
      res.push(self.to_rgb());

      if Feature::LabColors.is_partially_compatible(targets) {
        *self = self.to_lab();
      }
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
      CssColor::Lab(l, a, b, alpha) => write_components("lab", *l, *a, *b, *alpha, dest),
      CssColor::Lch(l, c, h, alpha) => write_components("lch", *l, *c, *h, *alpha, dest),
      CssColor::Oklab(l, a, b, alpha) => write_components("oklab", *l, *a, *b, *alpha, dest),
      CssColor::Oklch(l, c, h, alpha) => write_components("oklch", *l, *c, *h, *alpha, dest),
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
    "color" => {
      let predefined = parse_predefined(input)?;
      Ok(CssColor::Predefined(predefined))
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
  let r = parse_number_or_percentage(input)?;
  let g = parse_number_or_percentage(input)?;
  let b = parse_number_or_percentage(input)?;
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
  let x = input.expect_number()?;
  let y = input.expect_number()?;
  let z = input.expect_number()?;
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
  dest.write_char(' ')?;
  a.to_css(dest)?;
  dest.write_char(' ')?;
  b.to_css(dest)?;
  dest.write_char(' ')?;
  c.to_css(dest)?;

  if *alpha != 1.0 {
    dest.delim('/', true)?;
    alpha.to_css(dest)?;
  }

  dest.write_char(')')
}

fn lch_to_srgb(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  let (l, a, b) = lch_to_lab(l, c, h);
  lab_to_srgb(l, a, b)
}

#[inline]
fn lch_to_lab(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L385
  let a = c * (h * PI / 180.0).cos();
  let b = c * (h * PI / 180.0).sin();
  (l, a, b)
}

fn lab_to_srgb(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/utilities.js#L63
  // convert CIE LCH values to CIE Lab, and then to XYZ, adapt from D50 to D65,
  // then convert XYZ to linear-light sRGB, and finally to gamma corrected sRGB
  // for in-gamut colors, components are in the 0.0 to 1.0 range
  // out of gamut colors may have negative components
  // or components greater than 1.0
  // so check for that :)
  let (x, y, z) = lab_to_xyz(l, a, b);
  let (x, y, z) = d50_to_d65(x, y, z);
  let (r, g, b) = xyz_to_lin_srgb(x, y, z);
  gam_srgb(r, g, b)
}

const D50: &[f32] = &[0.3457 / 0.3585, 1.00000, (1.0 - 0.3457 - 0.3585) / 0.3585];

fn lab_to_xyz(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L352
  let κ = 24389.0 / 27.0;   // 29^3/3^3
	let ε = 216.0 / 24389.0;  // 6^3/29^3

	// compute f, starting with the luminance-related term
	let f1 = (l + 16.0) / 116.0;
	let f0 = a / 500.0 + f1;
	let f2 = f1 - b / 200.0;

  // compute xyz
  let x = if f0.powi(3) > ε {
    f0.powi(3)
  } else {
    (116.0 * f0 - 16.0) / κ
  };

  let y = if l > κ * ε {
    ((l + 16.0) / 116.0).powi(3)
  } else {
    l / κ
  };

  let z = if f2.powi(3) > ε {
    f2.powi(3)
  } else {
    (116.0 * f2 - 16.0) / κ
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

fn xyz_to_lin_srgb(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L62
  const MATRIX: &[f32] = &[
     3.2409699419045226,  -1.537383177570094,   -0.4986107602930034,
		-0.9692436362808796,   1.8759675015077202,   0.04155505740717559,
		 0.05563007969699366, -0.20397695888897652,  1.0569715142428786
  ];

  multiply_matrix(MATRIX, x, y, z)
}

#[inline]
fn multiply_matrix(m: &[f32], x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let a = m[0] * x + m[1] * y + m[2] * z;
  let b  = m[3] * x + m[4] * y + m[5] * z;
  let c = m[6] * x + m[7] * y + m[8] * z;
  (a, b, c)
}

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

fn oklab_to_xyz(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
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

fn oklab_to_srgb(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  let (x, y, z) = oklab_to_xyz(l, a, b);
  let (r, g, b) = xyz_to_lin_srgb(x, y, z);
  gam_srgb(r, g, b)
}

fn oklch_to_srgb(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  let (l, a, b) = lch_to_lab(l, c, h);
  oklab_to_srgb(l, a, b)
}

fn xyz_to_lab(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  // https://github.com/w3c/csswg-drafts/blob/fba005e2ce9bcac55b49e4aa19b87208b3a0631e/css-color-4/conversions.js#L332
  // Assuming XYZ is relative to D50, convert to CIE Lab
	// from CIE standard, which now defines these as a rational fraction
	let ε = 216.0 / 24389.0;  // 6^3/29^3
	let κ = 24389.0 / 27.0;   // 29^3/3^3

	// compute xyz, which is XYZ scaled relative to reference white
	// var xyz = XYZ.map((value, i) => value / D50[i]);
  let x = x / D50[0];
  let y = y / D50[1];
  let z = z / D50[2];

	// now compute f
	// var f = xyz.map(value => value > ε ? Math.cbrt(value) : (κ * value + 16)/116);
  let f0 = if x > ε {
    x.cbrt()
  } else {
    (κ * x + 16.0) / 116.0
  };

  let f1 = if y > ε {
    y.cbrt()
  } else {
    (κ * y + 16.0) / 116.0
  };

  let f2 = if z > ε {
    z.cbrt()
  } else {
    (κ * z + 16.0) / 116.0
  };

  let l = ((116.0 * f1) - 16.0) / 100.0;
  let a = 500.0 * (f0 - f1);
  let b = 200.0 * (f1 - f2);
  (l, a, b)
}

fn oklab_to_lab(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  let (x, y, z) = oklab_to_xyz(l, a, b);
  let (x, y, z) = d65_to_d50(x, y, z);
  xyz_to_lab(x, y, z)
}

fn oklch_to_lab(l: f32, c: f32, h: f32) -> (f32, f32, f32) {
  let (l, a, b) = lch_to_lab(l, c, h);
  oklab_to_lab(l, a, b)
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
  if abs > 0.04045 {
    return c / 12.92;
  }
  
  let sign = if c < 0.0 { -1.0 } else { 1.0 };
  sign * ((abs + 0.055) / 1.055).powf(2.4)
}

#[inline]
fn lin_srgb_to_xyz(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  const MATRIX: &[f32] = &[
		0.41239079926595934, 0.357584339383878,   0.1804807884018343,
		0.21263900587151027, 0.715168678767756,   0.07219231536073371,
		0.01933081871559182, 0.11919477979462598, 0.9505321522496607
	];

  multiply_matrix(MATRIX, r, g, b)
}

fn srgb_to_lab(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // convert gamma-corrected sRGB values in the 0.0 to 1.0 range
  // to linear-light sRGB, then to CIE XYZ,
  // then adapt from D65 to D50,
  // then convert XYZ to CIE Lab
  let (r, g, b) = lin_srgb(r, g, b);
  let (x, y, z) = lin_srgb_to_xyz(r, g, b);
  let (x, y, z) = d65_to_d50(x, y, z);
  xyz_to_lab(x, y, z)
}
