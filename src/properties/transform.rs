use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{length::{LengthPercentage, NumberOrPercentage, Length, Angle}, ident::CustomIdent, time::Time, easing::EasingFunction};
use super::Property;
use crate::printer::Printer;
use std::fmt::Write;

/// https://www.w3.org/TR/2019/CR-css-transforms-1-20190214/#propdef-transform
#[derive(Debug, Clone, PartialEq)]
pub struct TransformList(pub Vec<Transform>);

impl Parse for TransformList {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    input.skip_whitespace();
    let mut results = vec![Transform::parse(input)?];
    loop {
      input.skip_whitespace();
      if let Ok(item) = input.try_parse(Transform::parse) {
        results.push(item);
      } else {
        return Ok(TransformList(results));
      }
    }
  }
}

impl ToCss for TransformList {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if dest.minify && self.0.len() > 1 {
      // Attempt to combine into a single matrix() or matrix3d() if shorter
      if let Some(matrix) = self.to_matrix() {
        let mut base = String::new();
        self.to_css_base(&mut Printer::new(&mut base, true))?;

        let mut mat = String::new();
        if let Some(matrix) = matrix.to_matrix2d() {
          Transform::Matrix(matrix).to_css(&mut Printer::new(&mut mat, true))?
        } else {
          Transform::Matrix3d(matrix).to_css(&mut Printer::new(&mut mat, true))?
        }

        if mat.len() < base.len() {
          dest.write_str(&mat)?;
        } else {
          dest.write_str(&base)?;
        }

        return Ok(())
      }
    }

    self.to_css_base(dest)
  }
}

impl TransformList {
  fn to_css_base<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    for item in &self.0 {
      item.to_css(dest)?;
    }
    Ok(())
  }
}

impl TransformList {
  pub fn to_matrix(&self) -> Option<Matrix3d<f32>> {
    let mut matrix = Matrix3d::identity();
    for transform in &self.0 {
      if let Some(m) = transform.to_matrix() {
        matrix = m.multiply(&matrix);
      } else {
        return None
      }
    }
    Some(matrix)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Transform {
  Translate(LengthPercentage, LengthPercentage),
  TranslateX(LengthPercentage),
  TranslateY(LengthPercentage),
  TranslateZ(Length),
  Translate3d(LengthPercentage, LengthPercentage, Length),
  Scale(NumberOrPercentage, NumberOrPercentage),
  ScaleX(NumberOrPercentage),
  ScaleY(NumberOrPercentage),
  ScaleZ(NumberOrPercentage),
  Scale3d(NumberOrPercentage, NumberOrPercentage, NumberOrPercentage),
  Rotate(Angle),
  RotateX(Angle),
  RotateY(Angle),
  RotateZ(Angle),
  Rotate3d(f32, f32, f32, Angle),
  Skew(Angle, Angle),
  SkewX(Angle),
  SkewY(Angle),
  Perspective(Length),
  Matrix(Matrix<f32>),
  Matrix3d(Matrix3d<f32>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Matrix<T> {
  pub a: T, pub b: T, pub c: T,
  pub d: T, pub e: T, pub f: T,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Matrix3d<T> {
  pub m11: T, pub m12: T, pub m13: T, pub m14: T,
  pub m21: T, pub m22: T, pub m23: T, pub m24: T,
  pub m31: T, pub m32: T, pub m33: T, pub m34: T,
  pub m41: T, pub m42: T, pub m43: T, pub m44: T,
}

/// https://drafts.csswg.org/css-transforms-2/#mathematical-description
impl Matrix3d<f32> {
  pub fn identity() -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0, m12: 0.0, m13: 0.0, m14: 0.0,
      m21: 0.0, m22: 1.0, m23: 0.0, m24: 0.0,
      m31: 0.0, m32: 0.0, m33: 1.0, m34: 0.0,
      m41: 0.0, m42: 0.0, m43: 0.0, m44: 1.0
    }
  }

  pub fn translate(x: f32, y: f32, z: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0, m12: 0.0, m13: 0.0, m14: 0.0,
      m21: 0.0, m22: 1.0, m23: 0.0, m24: 0.0,
      m31: 0.0, m32: 0.0, m33: 1.0, m34: 0.0,
      m41: x,   m42: y,   m43: z,   m44: 1.0
    }
  }

  pub fn scale(x: f32, y: f32, z: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: x,   m12: 0.0, m13: 0.0, m14: 0.0,
      m21: 0.0, m22: y,   m23: 0.0, m24: 0.0,
      m31: 0.0, m32: 0.0, m33: z,   m34: 0.0,
      m41: 0.0, m42: 0.0, m43: 0.0, m44: 1.0
    }
  }

  pub fn rotate(x: f32, y: f32, z: f32, angle: f32) -> Matrix3d<f32> {
    // Normalize the vector.
    let length = (x * x + y * y + z * z).sqrt();
    if length == 0.0 {
      // A direction vector that cannot be normalized, such as [0,0,0], will cause the rotation to not be applied.
      return Matrix3d::identity()
    }

    let x = x / length;
    let y = y / length;
    let z = z / length;

    let half_angle = angle / 2.0;
    let sin = half_angle.sin();
    let sc = sin * half_angle.cos();
    let sq = sin * sin;
    let m11 = 1.0 - 2.0 * (y * y + z * z) * sq;
    let m12 = 2.0 * (x * y * sq + z * sc);
    let m13 = 2.0 * (x * z * sq - y * sc);
    let m21 = 2.0 * (x * y * sq - z * sc);
    let m22 = 1.0 - 2.0 * (x * x + z * z) * sq;
    let m23 = 2.0 * (y * z * sq + x * sc);
    let m31 = 2.0 * (x * z * sq + y * sc);
    let m32 = 2.0 * (y * z * sq - x * sc);
    let m33 = 1.0 - 2.0 * (x * x + y * y) * sq;
    Matrix3d {
      m11, m12, m13, m14: 0.0,
      m21, m22, m23, m24: 0.0,
      m31, m32, m33, m34: 0.0,
      m41: 0.0, m42: 0.0, m43: 0.0, m44: 1.0
    }
  }

  pub fn skew(a: f32, b: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0, m12: b.tan(), m13: 0.0, m14: 0.0,
      m21: a.tan(), m22: 1.0, m23: 0.0, m24: 0.0,
      m31: 0.0, m32: 0.0, m33: 1.0, m34: 0.0,
      m41: 0.0, m42: 0.0, m43: 0.0, m44: 1.0
    }
  }

  pub fn perspective(d: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0, m12: 0.0, m13: 0.0, m14: 0.0,
      m21: 0.0, m22: 1.0, m23: 0.0, m24: 0.0,
      m31: 0.0, m32: 0.0, m33: 1.0, m34: -1.0 / d,
      m41: 0.0, m42: 0.0, m43: 0.0, m44: 1.0
    }
  }

  pub fn multiply(&self, other: &Self) -> Self {
    Matrix3d {
      m11: self.m11 * other.m11 + self.m12 * other.m21 +
           self.m13 * other.m31 + self.m14 * other.m41,
      m12: self.m11 * other.m12 + self.m12 * other.m22 +
           self.m13 * other.m32 + self.m14 * other.m42,
      m13: self.m11 * other.m13 + self.m12 * other.m23 +
           self.m13 * other.m33 + self.m14 * other.m43,
      m14: self.m11 * other.m14 + self.m12 * other.m24 +
           self.m13 * other.m34 + self.m14 * other.m44,
      m21: self.m21 * other.m11 + self.m22 * other.m21 +
           self.m23 * other.m31 + self.m24 * other.m41,
      m22: self.m21 * other.m12 + self.m22 * other.m22 +
           self.m23 * other.m32 + self.m24 * other.m42,
      m23: self.m21 * other.m13 + self.m22 * other.m23 +
           self.m23 * other.m33 + self.m24 * other.m43,
      m24: self.m21 * other.m14 + self.m22 * other.m24 +
           self.m23 * other.m34 + self.m24 * other.m44,
      m31: self.m31 * other.m11 + self.m32 * other.m21 +
           self.m33 * other.m31 + self.m34 * other.m41,
      m32: self.m31 * other.m12 + self.m32 * other.m22 +
           self.m33 * other.m32 + self.m34 * other.m42,
      m33: self.m31 * other.m13 + self.m32 * other.m23 +
           self.m33 * other.m33 + self.m34 * other.m43,
      m34: self.m31 * other.m14 + self.m32 * other.m24 +
           self.m33 * other.m34 + self.m34 * other.m44,
      m41: self.m41 * other.m11 + self.m42 * other.m21 +
           self.m43 * other.m31 + self.m44 * other.m41,
      m42: self.m41 * other.m12 + self.m42 * other.m22 +
           self.m43 * other.m32 + self.m44 * other.m42,
      m43: self.m41 * other.m13 + self.m42 * other.m23 +
           self.m43 * other.m33 + self.m44 * other.m43,
      m44: self.m41 * other.m14 + self.m42 * other.m24 +
           self.m43 * other.m34 + self.m44 * other.m44,
    }
  }

  pub fn is_2d(&self) -> bool {
    self.m31 == 0.0 && self.m32 == 0.0 &&
    self.m13 == 0.0 && self.m23 == 0.0 &&
    self.m43 == 0.0 && self.m14 == 0.0 &&
    self.m24 == 0.0 && self.m34 == 0.0 &&
    self.m33 == 1.0 && self.m44 == 1.0
  }

  pub fn to_matrix2d(&self) -> Option<Matrix<f32>> {
    if self.is_2d() {
      return Some(Matrix {
        a: self.m11, b: self.m12,
        c: self.m21, d: self.m22,
        e: self.m41, f: self.m42
      })
    }
    None
  }
}

impl Parse for Transform {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let function = input.expect_function()?.clone();
    input.parse_nested_block(|input| {
      let location = input.current_source_location();
      match_ignore_ascii_case! { &function,
        "matrix" => {
          let a = f32::parse(input)?;
          input.expect_comma()?;
          let b = f32::parse(input)?;
          input.expect_comma()?;
          let c = f32::parse(input)?;
          input.expect_comma()?;
          let d = f32::parse(input)?;
          input.expect_comma()?;
          let e = f32::parse(input)?;
          input.expect_comma()?;
          let f = f32::parse(input)?;
          Ok(Transform::Matrix(Matrix { a, b, c, d, e, f }))
        },
        "matrix3d" => {
          let m11 = f32::parse(input)?;
          input.expect_comma()?;
          let m12 = f32::parse(input)?;
          input.expect_comma()?;
          let m13 = f32::parse(input)?;
          input.expect_comma()?;
          let m14 = f32::parse(input)?;
          input.expect_comma()?;
          let m21 = f32::parse(input)?;
          input.expect_comma()?;
          let m22 = f32::parse(input)?;
          input.expect_comma()?;
          let m23 = f32::parse(input)?;
          input.expect_comma()?;
          let m24 = f32::parse(input)?;
          input.expect_comma()?;
          let m31 = f32::parse(input)?;
          input.expect_comma()?;
          let m32 = f32::parse(input)?;
          input.expect_comma()?;
          let m33 = f32::parse(input)?;
          input.expect_comma()?;
          let m34 = f32::parse(input)?;
          input.expect_comma()?;
          let m41 = f32::parse(input)?;
          input.expect_comma()?;
          let m42 = f32::parse(input)?;
          input.expect_comma()?;
          let m43 = f32::parse(input)?;
          input.expect_comma()?;
          let m44 = f32::parse(input)?;
          Ok(Transform::Matrix3d(Matrix3d {
            m11, m12, m13, m14,
            m21, m22, m23, m24,
            m31, m32, m33, m34,
            m41, m42, m43, m44
          }))
        },
        "translate" => {
          let x = LengthPercentage::parse(input)?;
          if input.try_parse(|input| input.expect_comma()).is_ok() {
            let y = LengthPercentage::parse(input)?;
            Ok(Transform::Translate(x, y))
          } else {
            Ok(Transform::Translate(x, LengthPercentage::Length(Length::zero())))
          }
        },
        "translatex" => {
          let x = LengthPercentage::parse(input)?;
          Ok(Transform::TranslateX(x))
        },
        "translatey" => {
          let y = LengthPercentage::parse(input)?;
          Ok(Transform::TranslateY(y))
        },
        "translatez" => {
          let z = Length::parse(input)?;
          Ok(Transform::TranslateZ(z))
        },
        "translate3d" => {
          let x = LengthPercentage::parse(input)?;
          input.expect_comma()?;
          let y = LengthPercentage::parse(input)?;
          input.expect_comma()?;
          let z = Length::parse(input)?;
          Ok(Transform::Translate3d(x, y, z))
        },
        "scale" => {
          let x = NumberOrPercentage::parse(input)?;
          if input.try_parse(|input| input.expect_comma()).is_ok() {
            let y = NumberOrPercentage::parse(input)?;
            Ok(Transform::Scale(x, y))
          } else {
            Ok(Transform::Scale(x.clone(), x))
          }
        },
        "scalex" => {
          let x = NumberOrPercentage::parse(input)?;
          Ok(Transform::ScaleX(x))
        },
        "scaley" => {
          let y = NumberOrPercentage::parse(input)?;
          Ok(Transform::ScaleY(y))
        },
        "scalez" => {
          let z = NumberOrPercentage::parse(input)?;
          Ok(Transform::ScaleZ(z))
        },
        "scale3d" => {
          let x = NumberOrPercentage::parse(input)?;
          input.expect_comma()?;
          let y = NumberOrPercentage::parse(input)?;
          input.expect_comma()?;
          let z = NumberOrPercentage::parse(input)?;
          Ok(Transform::Scale3d(x, y, z))
        },
        "rotate" => {
          let angle = Angle::parse(input)?;
          Ok(Transform::Rotate(angle))
        },
        "rotatex" => {
          let angle = Angle::parse(input)?;
          Ok(Transform::RotateX(angle))
        },
        "rotatey" => {
          let angle = Angle::parse(input)?;
          Ok(Transform::RotateY(angle))
        },
        "rotatez" => {
          let angle = Angle::parse(input)?;
          Ok(Transform::RotateZ(angle))
        },
        "rotate3d" => {
          let x = f32::parse(input)?;
          input.expect_comma()?;
          let y = f32::parse(input)?;
          input.expect_comma()?;
          let z = f32::parse(input)?;
          input.expect_comma()?;
          let angle = Angle::parse(input)?;
          Ok(Transform::Rotate3d(x, y, z, angle))
        },
        "skew" => {
          let x = Angle::parse(input)?;
          if input.try_parse(|input| input.expect_comma()).is_ok() {
            let y = Angle::parse(input)?;
            Ok(Transform::Skew(x, y))
          } else {
            Ok(Transform::Skew(x, Angle::Deg(0.0)))
          }
        },
        "skewx" => {
          let angle = Angle::parse(input)?;
          Ok(Transform::SkewX(angle))
        },
        "skewy" => {
          let angle = Angle::parse(input)?;
          Ok(Transform::SkewY(angle))
        },
        "perspective" => {
          let len = Length::parse(input)?;
          Ok(Transform::Perspective(len))
        },
        _ => Err(location.new_unexpected_token_error(
          cssparser::Token::Ident(function.clone())
        ))
      }
    })
  }
}

impl ToCss for Transform {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use Transform::*;
    match self {
      Translate(x, y) => {
        if dest.minify && *x == 0.0 && *y != 0.0 {
          dest.write_str("translateY(")?;
          y.to_css(dest)?
        } else {
          dest.write_str("translate(")?;
          x.to_css(dest)?;
          if *y != 0.0 {
            dest.delim(',', false)?;
            y.to_css(dest)?;
          }
        }
        dest.write_char(')')
      }
      TranslateX(x) => {
        dest.write_str(if dest.minify { "translate(" } else { "translateX(" })?;
        x.to_css(dest)?;
        dest.write_char(')')
      }
      TranslateY(y) => {
        dest.write_str("translateY(")?;
        y.to_css(dest)?;
        dest.write_char(')')
      }
      TranslateZ(z) => {
        dest.write_str("translateZ(")?;
        z.to_css(dest)?;
        dest.write_char(')')
      }
      Translate3d(x, y, z) => {
        if dest.minify && *x != 0.0 && *y == 0.0 && z.is_zero() {
          dest.write_str("translate(")?;
          x.to_css(dest)?;
        } else if dest.minify && *x == 0.0 && *y != 0.0 && z.is_zero() {
          dest.write_str("translateY(")?;
          y.to_css(dest)?;
        } else if dest.minify && *x == 0.0 && *y == 0.0 && !z.is_zero() {
          dest.write_str("translateZ(")?;
          z.to_css(dest)?;
        } else if dest.minify && z.is_zero() {
          dest.write_str("translate(")?;
          x.to_css(dest)?;
          dest.delim(',', false)?;
          y.to_css(dest)?;
        } else {
          dest.write_str("translate3d(")?;
          x.to_css(dest)?;
          dest.delim(',', false)?;
          y.to_css(dest)?;
          dest.delim(',', false)?;
          z.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Scale(x, y) => {
        if dest.minify && *x == 1.0 && *y != 1.0 {
          dest.write_str("scaleY(")?;
          y.to_css(dest)?;
        } else if dest.minify && *x != 1.0 && *y == 1.0 {
          dest.write_str("scaleX(")?;
          x.to_css(dest)?;
        } else {
          dest.write_str("scale(")?;
          x.to_css(dest)?;
          if *y != *x {
            dest.delim(',', false)?;
            y.to_css(dest)?;
          }
        }
        dest.write_char(')')
      }
      ScaleX(x) => {
        dest.write_str("scaleX(")?;
        x.to_css(dest)?;
        dest.write_char(')')
      }
      ScaleY(y) => {
        dest.write_str("scaleY(")?;
        y.to_css(dest)?;
        dest.write_char(')')
      }
      ScaleZ(z) => {
        dest.write_str("scaleZ(")?;
        z.to_css(dest)?;
        dest.write_char(')')
      }
      Scale3d(x, y, z) => {
        if dest.minify && *z == 1.0 && *x == *y {
          // scale3d(x, x, 1) => scale(x)
          dest.write_str("scale(")?;
          x.to_css(dest)?;
        } else if dest.minify && *x != 1.0 && *y == 1.0 && *z == 1.0 {
          // scale3d(x, 1, 1) => scaleX(x)
          dest.write_str("scaleX(")?;
          x.to_css(dest)?;
        } else if dest.minify && *x == 1.0 && *y != 1.0 && *z == 1.0 {
           // scale3d(1, y, 1) => scaleY(y)
          dest.write_str("scaleY(")?;
          y.to_css(dest)?;
        } else if dest.minify && *x == 1.0 && *y == 1.0 && *z != 1.0 {
          // scale3d(1, 1, z) => scaleZ(z)
          dest.write_str("scaleZ(")?;
          z.to_css(dest)?;
        } else if dest.minify && *z == 1.0 {
          // scale3d(x, y, 1) => scale(x, y)
          dest.write_str("scale(")?;
          x.to_css(dest)?;
          dest.delim(',', false)?;
          y.to_css(dest)?;
        } else {
          dest.write_str("scale3d(")?;
          x.to_css(dest)?;
          dest.delim(',', false)?;
          y.to_css(dest)?;
          dest.delim(',', false)?;
          z.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Rotate(angle) => {
        dest.write_str("rotate(")?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      RotateX(angle) => {
        dest.write_str("rotateX(")?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      RotateY(angle) => {
        dest.write_str("rotateY(")?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      RotateZ(angle) => {
        dest.write_str(if dest.minify { "rotate(" } else { "rotateZ(" })?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      Rotate3d(x, y, z, angle) => {
        if dest.minify && *x == 1.0 && *y == 0.0 && *z == 0.0 {
          // rotate3d(1, 0, 0, a) => rotateX(a)
          dest.write_str("rotateX(")?;
          angle.to_css(dest)?;
        } else if dest.minify && *x == 0.0 && *y == 1.0 && *z == 0.0 {
          // rotate3d(0, 1, 0, a) => rotateY(a)
          dest.write_str("rotateY(")?;
          angle.to_css(dest)?;
        } else if dest.minify && *x == 0.0 && *y == 0.0 && *z == 1.0 {
          // rotate3d(0, 0, 1, a) => rotate(a)
          dest.write_str("rotate(")?;
          angle.to_css(dest)?;
        } else {
          dest.write_str("rotate3d(")?;
          x.to_css(dest)?;
          dest.delim(',', false)?;
          y.to_css(dest)?;
          dest.delim(',', false)?;
          z.to_css(dest)?;
          dest.delim(',', false)?;
          angle.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Skew(x, y) => {
        if dest.minify && x.is_zero() && !y.is_zero() {
          dest.write_str("skewY(")?;
          y.to_css(dest)?
        } else {
          dest.write_str("skew(")?;
          x.to_css(dest)?;
          if !y.is_zero() {
            dest.delim(',', false)?;
            y.to_css(dest)?;
          }
        }
        dest.write_char(')')
      }
      SkewX(angle) => {
        dest.write_str(if dest.minify { "skew(" } else { "skewX(" })?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      SkewY(angle) => {
        dest.write_str("skewY(")?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      Perspective(len) => {
        dest.write_str("perspective(")?;
        len.to_css(dest)?;
        dest.write_char(')')
      }
      Matrix(super::transform::Matrix { a, b, c, d, e, f }) => {
        dest.write_str("matrix(")?;
        a.to_css(dest)?;
        dest.delim(',', false)?;
        b.to_css(dest)?;
        dest.delim(',', false)?;
        c.to_css(dest)?;
        dest.delim(',', false)?;
        d.to_css(dest)?;
        dest.delim(',', false)?;
        e.to_css(dest)?;
        dest.delim(',', false)?;
        f.to_css(dest)?;
        dest.write_char(')')
      }
      Matrix3d(super::transform::Matrix3d {
        m11, m12, m13, m14,
        m21, m22, m23, m24,
        m31, m32, m33, m34,
        m41, m42, m43, m44
      }) => {
        dest.write_str("matrix3d(")?;
        m11.to_css(dest)?;
        dest.delim(',', false)?;
        m12.to_css(dest)?;
        dest.delim(',', false)?;
        m13.to_css(dest)?;
        dest.delim(',', false)?;
        m14.to_css(dest)?;
        dest.delim(',', false)?;
        m21.to_css(dest)?;
        dest.delim(',', false)?;
        m22.to_css(dest)?;
        dest.delim(',', false)?;
        m23.to_css(dest)?;
        dest.delim(',', false)?;
        m24.to_css(dest)?;
        dest.delim(',', false)?;
        m31.to_css(dest)?;
        dest.delim(',', false)?;
        m32.to_css(dest)?;
        dest.delim(',', false)?;
        m33.to_css(dest)?;
        dest.delim(',', false)?;
        m34.to_css(dest)?;
        dest.delim(',', false)?;
        m41.to_css(dest)?;
        dest.delim(',', false)?;
        m42.to_css(dest)?;
        dest.delim(',', false)?;
        m43.to_css(dest)?;
        dest.delim(',', false)?;
        m44.to_css(dest)?;
        dest.write_char(')')
      }
    }
  }
}

impl Transform {
  pub fn to_matrix(&self) -> Option<Matrix3d<f32>> {
    match &self {
      Transform::Translate(LengthPercentage::Length(x), LengthPercentage::Length(y)) => {
        if let (Some(x), Some(y)) = (x.to_px(), y.to_px()) {
          return Some(Matrix3d::translate(x, y, 0.0))
        }
      }
      Transform::TranslateX(LengthPercentage::Length(x)) => {
        if let Some(x) = x.to_px() {
          return Some(Matrix3d::translate(x, 0.0, 0.0))
        }
      }
      Transform::TranslateY(LengthPercentage::Length(y)) => {
        if let Some(y) = y.to_px() {
          return Some(Matrix3d::translate(0.0, y, 0.0))
        }
      }
      Transform::TranslateZ(z) => {
        if let Some(z) = z.to_px() {
          return Some(Matrix3d::translate(0.0, 0.0, z))
        }
      }
      Transform::Translate3d(LengthPercentage::Length(x), LengthPercentage::Length(y), z) => {
        if let (Some(x), Some(y), Some(z)) = (x.to_px(), y.to_px(), z.to_px()) {
          return Some(Matrix3d::translate(x, y, z))
        }
      }
      Transform::Scale(x, y) => {
        return Some(Matrix3d::scale(x.into(), y.into(), 1.0))
      }
      Transform::ScaleX(x) => {
        return Some(Matrix3d::scale(x.into(), 1.0, 1.0))
      }
      Transform::ScaleY(y) => {
        return Some(Matrix3d::scale(1.0, y.into(), 1.0))
      }
      Transform::ScaleZ(z) => {
        return Some(Matrix3d::scale(1.0, 1.0, z.into()))
      }
      Transform::Scale3d(x, y, z) => {
        return Some(Matrix3d::scale(x.into(), y.into(), z.into()))
      }
      Transform::Rotate(angle) | Transform::RotateZ(angle) => {
        return Some(Matrix3d::rotate(0.0, 0.0, 1.0, angle.to_radians()))
      }
      Transform::RotateX(angle) => {
        return Some(Matrix3d::rotate(1.0, 0.0, 0.0, angle.to_radians()))
      }
      Transform::RotateY(angle) => {
        return Some(Matrix3d::rotate(0.0, 1.0, 0.0, angle.to_radians()))
      }
      Transform::Rotate3d(x, y, z, angle) => {
        return Some(Matrix3d::rotate(*x, *y, *z, angle.to_radians()))
      }
      Transform::Skew(x, y) => {
        return Some(Matrix3d::skew(x.to_radians(), y.to_radians()))
      }
      Transform::SkewX(x) => {
        return Some(Matrix3d::skew(x.to_radians(), 0.0))
      }
      Transform::SkewY(y) => {
        return Some(Matrix3d::skew(0.0, y.to_radians()))
      }
      Transform::Perspective(len) => {
        if let Some(len) = len.to_px() {
          return Some(Matrix3d::perspective(len))
        }
      }
      Transform::Matrix(Matrix { a, b, c, d, e, f }) => {
        return Some(Matrix3d {
          m11: *a, m12: *c, m13: 0.0, m14: *e,
          m21: *b, m22: *d, m23: 0.0, m24: *f,
          m31: 0.0, m32: 0.0, m33: 1.0, m34: 0.0,
          m41: 0.0, m42: 0.0, m43: 0.0, m44: 1.0
        })
      }
      Transform::Matrix3d(m) => return Some(m.clone()),
      _ => {}
    }
    None
  }
}
