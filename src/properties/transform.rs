//! CSS properties related to 2D and 3D transforms.

use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::stylesheet::PrinterOptions;
use crate::traits::{Parse, PropertyHandler, ToCss, Zero};
use crate::values::{
  angle::Angle,
  length::{Length, LengthPercentage},
  percentage::NumberOrPercentage,
};
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use std::f32::consts::PI;

/// A value for the [transform](https://www.w3.org/TR/2019/CR-css-transforms-1-20190214/#propdef-transform) property.
#[derive(Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TransformList(pub Vec<Transform>);

impl<'i> Parse<'i> for TransformList {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(TransformList(vec![]));
    }

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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.0.is_empty() {
      dest.write_str("none")?;
      return Ok(());
    }

    if dest.minify {
      // Combine transforms into a single matrix.
      if let Some(matrix) = self.to_matrix() {
        // Generate based on the original transforms.
        let mut base = String::new();
        self.to_css_base(&mut Printer::new(
          &mut base,
          PrinterOptions {
            minify: true,
            ..PrinterOptions::default()
          },
        ))?;

        // Decompose the matrix into transform functions if possible.
        // If the resulting length is shorter than the original, use it.
        if let Some(d) = matrix.decompose() {
          let mut decomposed = String::new();
          d.to_css_base(&mut Printer::new(
            &mut decomposed,
            PrinterOptions {
              minify: true,
              ..PrinterOptions::default()
            },
          ))?;
          if decomposed.len() < base.len() {
            base = decomposed;
          }
        }

        // Also generate a matrix() or matrix3d() representation and compare that.
        let mut mat = String::new();
        if let Some(matrix) = matrix.to_matrix2d() {
          Transform::Matrix(matrix).to_css(&mut Printer::new(
            &mut mat,
            PrinterOptions {
              minify: true,
              ..PrinterOptions::default()
            },
          ))?
        } else {
          Transform::Matrix3d(matrix).to_css(&mut Printer::new(
            &mut mat,
            PrinterOptions {
              minify: true,
              ..PrinterOptions::default()
            },
          ))?
        }

        if mat.len() < base.len() {
          dest.write_str(&mat)?;
        } else {
          dest.write_str(&base)?;
        }

        return Ok(());
      }
    }

    self.to_css_base(dest)
  }
}

impl TransformList {
  fn to_css_base<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    for item in &self.0 {
      item.to_css(dest)?;
    }
    Ok(())
  }

  /// Converts the transform list to a 3D matrix if possible.
  pub fn to_matrix(&self) -> Option<Matrix3d<f32>> {
    let mut matrix = Matrix3d::identity();
    for transform in &self.0 {
      if let Some(m) = transform.to_matrix() {
        matrix = m.multiply(&matrix);
      } else {
        return None;
      }
    }
    Some(matrix)
  }
}

/// An individual [transform function](https://www.w3.org/TR/2019/CR-css-transforms-1-20190214/#two-d-transform-functions).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Transform {
  /// A 2D translation.
  Translate(LengthPercentage, LengthPercentage),
  /// A translation in the X direction.
  TranslateX(LengthPercentage),
  /// A translation in the Y direction.
  TranslateY(LengthPercentage),
  /// A translation in the Z direction.
  TranslateZ(Length),
  /// A 3D translation.
  Translate3d(LengthPercentage, LengthPercentage, Length),
  /// A 2D scale.
  Scale(NumberOrPercentage, NumberOrPercentage),
  /// A scale in the X direction.
  ScaleX(NumberOrPercentage),
  /// A scale in the Y direction.
  ScaleY(NumberOrPercentage),
  /// A scale in the Z direction.
  ScaleZ(NumberOrPercentage),
  /// A 3D scale.
  Scale3d(NumberOrPercentage, NumberOrPercentage, NumberOrPercentage),
  /// A 2D rotation.
  Rotate(Angle),
  /// A rotation around the X axis.
  RotateX(Angle),
  /// A rotation around the Y axis.
  RotateY(Angle),
  /// A rotation around the Z axis.
  RotateZ(Angle),
  /// A 3D rotation.
  Rotate3d(f32, f32, f32, Angle),
  /// A 2D skew.
  Skew(Angle, Angle),
  /// A skew along the X axis.
  SkewX(Angle),
  /// A skew along the Y axis.
  SkewY(Angle),
  /// A perspective transform.
  Perspective(Length),
  /// A 2D matrix transform.
  Matrix(Matrix<f32>),
  /// A 3D matrix transform.
  Matrix3d(Matrix3d<f32>),
}

/// A 2D matrix.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[allow(missing_docs)]
pub struct Matrix<T> {
  pub a: T,
  pub b: T,
  pub c: T,
  pub d: T,
  pub e: T,
  pub f: T,
}

impl Matrix<f32> {
  /// Converts the matrix to a 3D matrix.
  pub fn to_matrix3d(&self) -> Matrix3d<f32> {
    Matrix3d {
      m11: self.a,
      m12: self.b,
      m13: 0.0,
      m14: 0.0,
      m21: self.c,
      m22: self.d,
      m23: 0.0,
      m24: 0.0,
      m31: 0.0,
      m32: 0.0,
      m33: 1.0,
      m34: 0.0,
      m41: self.e,
      m42: self.f,
      m43: 0.0,
      m44: 1.0,
    }
  }
}

/// A 3D matrix.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[allow(missing_docs)]
pub struct Matrix3d<T> {
  pub m11: T,
  pub m12: T,
  pub m13: T,
  pub m14: T,
  pub m21: T,
  pub m22: T,
  pub m23: T,
  pub m24: T,
  pub m31: T,
  pub m32: T,
  pub m33: T,
  pub m34: T,
  pub m41: T,
  pub m42: T,
  pub m43: T,
  pub m44: T,
}

// https://drafts.csswg.org/css-transforms-2/#mathematical-description
impl Matrix3d<f32> {
  /// Creates an identity matrix.
  pub fn identity() -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0,
      m12: 0.0,
      m13: 0.0,
      m14: 0.0,
      m21: 0.0,
      m22: 1.0,
      m23: 0.0,
      m24: 0.0,
      m31: 0.0,
      m32: 0.0,
      m33: 1.0,
      m34: 0.0,
      m41: 0.0,
      m42: 0.0,
      m43: 0.0,
      m44: 1.0,
    }
  }

  /// Creates a translation matrix.
  pub fn translate(x: f32, y: f32, z: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0,
      m12: 0.0,
      m13: 0.0,
      m14: 0.0,
      m21: 0.0,
      m22: 1.0,
      m23: 0.0,
      m24: 0.0,
      m31: 0.0,
      m32: 0.0,
      m33: 1.0,
      m34: 0.0,
      m41: x,
      m42: y,
      m43: z,
      m44: 1.0,
    }
  }

  /// Creates a scale matrix.
  pub fn scale(x: f32, y: f32, z: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: x,
      m12: 0.0,
      m13: 0.0,
      m14: 0.0,
      m21: 0.0,
      m22: y,
      m23: 0.0,
      m24: 0.0,
      m31: 0.0,
      m32: 0.0,
      m33: z,
      m34: 0.0,
      m41: 0.0,
      m42: 0.0,
      m43: 0.0,
      m44: 1.0,
    }
  }

  /// Creates a rotation matrix.
  pub fn rotate(x: f32, y: f32, z: f32, angle: f32) -> Matrix3d<f32> {
    // Normalize the vector.
    let length = (x * x + y * y + z * z).sqrt();
    if length == 0.0 {
      // A direction vector that cannot be normalized, such as [0,0,0], will cause the rotation to not be applied.
      return Matrix3d::identity();
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
      m11,
      m12,
      m13,
      m14: 0.0,
      m21,
      m22,
      m23,
      m24: 0.0,
      m31,
      m32,
      m33,
      m34: 0.0,
      m41: 0.0,
      m42: 0.0,
      m43: 0.0,
      m44: 1.0,
    }
  }

  /// Creates a skew matrix.
  pub fn skew(a: f32, b: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0,
      m12: b.tan(),
      m13: 0.0,
      m14: 0.0,
      m21: a.tan(),
      m22: 1.0,
      m23: 0.0,
      m24: 0.0,
      m31: 0.0,
      m32: 0.0,
      m33: 1.0,
      m34: 0.0,
      m41: 0.0,
      m42: 0.0,
      m43: 0.0,
      m44: 1.0,
    }
  }

  /// Creates a perspective matrix.
  pub fn perspective(d: f32) -> Matrix3d<f32> {
    Matrix3d {
      m11: 1.0,
      m12: 0.0,
      m13: 0.0,
      m14: 0.0,
      m21: 0.0,
      m22: 1.0,
      m23: 0.0,
      m24: 0.0,
      m31: 0.0,
      m32: 0.0,
      m33: 1.0,
      m34: -1.0 / d,
      m41: 0.0,
      m42: 0.0,
      m43: 0.0,
      m44: 1.0,
    }
  }

  /// Multiplies this matrix by another, returning a new matrix.
  pub fn multiply(&self, other: &Self) -> Self {
    Matrix3d {
      m11: self.m11 * other.m11 + self.m12 * other.m21 + self.m13 * other.m31 + self.m14 * other.m41,
      m12: self.m11 * other.m12 + self.m12 * other.m22 + self.m13 * other.m32 + self.m14 * other.m42,
      m13: self.m11 * other.m13 + self.m12 * other.m23 + self.m13 * other.m33 + self.m14 * other.m43,
      m14: self.m11 * other.m14 + self.m12 * other.m24 + self.m13 * other.m34 + self.m14 * other.m44,
      m21: self.m21 * other.m11 + self.m22 * other.m21 + self.m23 * other.m31 + self.m24 * other.m41,
      m22: self.m21 * other.m12 + self.m22 * other.m22 + self.m23 * other.m32 + self.m24 * other.m42,
      m23: self.m21 * other.m13 + self.m22 * other.m23 + self.m23 * other.m33 + self.m24 * other.m43,
      m24: self.m21 * other.m14 + self.m22 * other.m24 + self.m23 * other.m34 + self.m24 * other.m44,
      m31: self.m31 * other.m11 + self.m32 * other.m21 + self.m33 * other.m31 + self.m34 * other.m41,
      m32: self.m31 * other.m12 + self.m32 * other.m22 + self.m33 * other.m32 + self.m34 * other.m42,
      m33: self.m31 * other.m13 + self.m32 * other.m23 + self.m33 * other.m33 + self.m34 * other.m43,
      m34: self.m31 * other.m14 + self.m32 * other.m24 + self.m33 * other.m34 + self.m34 * other.m44,
      m41: self.m41 * other.m11 + self.m42 * other.m21 + self.m43 * other.m31 + self.m44 * other.m41,
      m42: self.m41 * other.m12 + self.m42 * other.m22 + self.m43 * other.m32 + self.m44 * other.m42,
      m43: self.m41 * other.m13 + self.m42 * other.m23 + self.m43 * other.m33 + self.m44 * other.m43,
      m44: self.m41 * other.m14 + self.m42 * other.m24 + self.m43 * other.m34 + self.m44 * other.m44,
    }
  }

  /// Returns whether this matrix could be converted to a 2D matrix.
  pub fn is_2d(&self) -> bool {
    self.m31 == 0.0
      && self.m32 == 0.0
      && self.m13 == 0.0
      && self.m23 == 0.0
      && self.m43 == 0.0
      && self.m14 == 0.0
      && self.m24 == 0.0
      && self.m34 == 0.0
      && self.m33 == 1.0
      && self.m44 == 1.0
  }

  /// Attempts to convert the matrix to 2D.
  /// Returns `None` if the conversion is not possible.
  pub fn to_matrix2d(&self) -> Option<Matrix<f32>> {
    if self.is_2d() {
      return Some(Matrix {
        a: self.m11,
        b: self.m12,
        c: self.m21,
        d: self.m22,
        e: self.m41,
        f: self.m42,
      });
    }
    None
  }

  /// Scales the matrix by the given factor.
  pub fn scale_by_factor(&mut self, scaling_factor: f32) {
    self.m11 *= scaling_factor;
    self.m12 *= scaling_factor;
    self.m13 *= scaling_factor;
    self.m14 *= scaling_factor;
    self.m21 *= scaling_factor;
    self.m22 *= scaling_factor;
    self.m23 *= scaling_factor;
    self.m24 *= scaling_factor;
    self.m31 *= scaling_factor;
    self.m32 *= scaling_factor;
    self.m33 *= scaling_factor;
    self.m34 *= scaling_factor;
    self.m41 *= scaling_factor;
    self.m42 *= scaling_factor;
    self.m43 *= scaling_factor;
    self.m44 *= scaling_factor;
  }

  /// Returns the determinant of the matrix.
  pub fn determinant(&self) -> f32 {
    self.m14 * self.m23 * self.m32 * self.m41
      - self.m13 * self.m24 * self.m32 * self.m41
      - self.m14 * self.m22 * self.m33 * self.m41
      + self.m12 * self.m24 * self.m33 * self.m41
      + self.m13 * self.m22 * self.m34 * self.m41
      - self.m12 * self.m23 * self.m34 * self.m41
      - self.m14 * self.m23 * self.m31 * self.m42
      + self.m13 * self.m24 * self.m31 * self.m42
      + self.m14 * self.m21 * self.m33 * self.m42
      - self.m11 * self.m24 * self.m33 * self.m42
      - self.m13 * self.m21 * self.m34 * self.m42
      + self.m11 * self.m23 * self.m34 * self.m42
      + self.m14 * self.m22 * self.m31 * self.m43
      - self.m12 * self.m24 * self.m31 * self.m43
      - self.m14 * self.m21 * self.m32 * self.m43
      + self.m11 * self.m24 * self.m32 * self.m43
      + self.m12 * self.m21 * self.m34 * self.m43
      - self.m11 * self.m22 * self.m34 * self.m43
      - self.m13 * self.m22 * self.m31 * self.m44
      + self.m12 * self.m23 * self.m31 * self.m44
      + self.m13 * self.m21 * self.m32 * self.m44
      - self.m11 * self.m23 * self.m32 * self.m44
      - self.m12 * self.m21 * self.m33 * self.m44
      + self.m11 * self.m22 * self.m33 * self.m44
  }

  /// Returns the inverse of the matrix if possible.
  pub fn inverse(&self) -> Option<Matrix3d<f32>> {
    let mut det = self.determinant();
    if det == 0.0 {
      return None;
    }

    det = 1.0 / det;
    Some(Matrix3d {
      m11: det
        * (self.m23 * self.m34 * self.m42 - self.m24 * self.m33 * self.m42 + self.m24 * self.m32 * self.m43
          - self.m22 * self.m34 * self.m43
          - self.m23 * self.m32 * self.m44
          + self.m22 * self.m33 * self.m44),
      m12: det
        * (self.m14 * self.m33 * self.m42 - self.m13 * self.m34 * self.m42 - self.m14 * self.m32 * self.m43
          + self.m12 * self.m34 * self.m43
          + self.m13 * self.m32 * self.m44
          - self.m12 * self.m33 * self.m44),
      m13: det
        * (self.m13 * self.m24 * self.m42 - self.m14 * self.m23 * self.m42 + self.m14 * self.m22 * self.m43
          - self.m12 * self.m24 * self.m43
          - self.m13 * self.m22 * self.m44
          + self.m12 * self.m23 * self.m44),
      m14: det
        * (self.m14 * self.m23 * self.m32 - self.m13 * self.m24 * self.m32 - self.m14 * self.m22 * self.m33
          + self.m12 * self.m24 * self.m33
          + self.m13 * self.m22 * self.m34
          - self.m12 * self.m23 * self.m34),
      m21: det
        * (self.m24 * self.m33 * self.m41 - self.m23 * self.m34 * self.m41 - self.m24 * self.m31 * self.m43
          + self.m21 * self.m34 * self.m43
          + self.m23 * self.m31 * self.m44
          - self.m21 * self.m33 * self.m44),
      m22: det
        * (self.m13 * self.m34 * self.m41 - self.m14 * self.m33 * self.m41 + self.m14 * self.m31 * self.m43
          - self.m11 * self.m34 * self.m43
          - self.m13 * self.m31 * self.m44
          + self.m11 * self.m33 * self.m44),
      m23: det
        * (self.m14 * self.m23 * self.m41 - self.m13 * self.m24 * self.m41 - self.m14 * self.m21 * self.m43
          + self.m11 * self.m24 * self.m43
          + self.m13 * self.m21 * self.m44
          - self.m11 * self.m23 * self.m44),
      m24: det
        * (self.m13 * self.m24 * self.m31 - self.m14 * self.m23 * self.m31 + self.m14 * self.m21 * self.m33
          - self.m11 * self.m24 * self.m33
          - self.m13 * self.m21 * self.m34
          + self.m11 * self.m23 * self.m34),
      m31: det
        * (self.m22 * self.m34 * self.m41 - self.m24 * self.m32 * self.m41 + self.m24 * self.m31 * self.m42
          - self.m21 * self.m34 * self.m42
          - self.m22 * self.m31 * self.m44
          + self.m21 * self.m32 * self.m44),
      m32: det
        * (self.m14 * self.m32 * self.m41 - self.m12 * self.m34 * self.m41 - self.m14 * self.m31 * self.m42
          + self.m11 * self.m34 * self.m42
          + self.m12 * self.m31 * self.m44
          - self.m11 * self.m32 * self.m44),
      m33: det
        * (self.m12 * self.m24 * self.m41 - self.m14 * self.m22 * self.m41 + self.m14 * self.m21 * self.m42
          - self.m11 * self.m24 * self.m42
          - self.m12 * self.m21 * self.m44
          + self.m11 * self.m22 * self.m44),
      m34: det
        * (self.m14 * self.m22 * self.m31 - self.m12 * self.m24 * self.m31 - self.m14 * self.m21 * self.m32
          + self.m11 * self.m24 * self.m32
          + self.m12 * self.m21 * self.m34
          - self.m11 * self.m22 * self.m34),
      m41: det
        * (self.m23 * self.m32 * self.m41 - self.m22 * self.m33 * self.m41 - self.m23 * self.m31 * self.m42
          + self.m21 * self.m33 * self.m42
          + self.m22 * self.m31 * self.m43
          - self.m21 * self.m32 * self.m43),
      m42: det
        * (self.m12 * self.m33 * self.m41 - self.m13 * self.m32 * self.m41 + self.m13 * self.m31 * self.m42
          - self.m11 * self.m33 * self.m42
          - self.m12 * self.m31 * self.m43
          + self.m11 * self.m32 * self.m43),
      m43: det
        * (self.m13 * self.m22 * self.m41 - self.m12 * self.m23 * self.m41 - self.m13 * self.m21 * self.m42
          + self.m11 * self.m23 * self.m42
          + self.m12 * self.m21 * self.m43
          - self.m11 * self.m22 * self.m43),
      m44: det
        * (self.m12 * self.m23 * self.m31 - self.m13 * self.m22 * self.m31 + self.m13 * self.m21 * self.m32
          - self.m11 * self.m23 * self.m32
          - self.m12 * self.m21 * self.m33
          + self.m11 * self.m22 * self.m33),
    })
  }

  /// Transposes the matrix.
  pub fn transpose(&self) -> Self {
    Self {
      m11: self.m11,
      m12: self.m21,
      m13: self.m31,
      m14: self.m41,
      m21: self.m12,
      m22: self.m22,
      m23: self.m32,
      m24: self.m42,
      m31: self.m13,
      m32: self.m23,
      m33: self.m33,
      m34: self.m43,
      m41: self.m14,
      m42: self.m24,
      m43: self.m34,
      m44: self.m44,
    }
  }

  /// Multiplies a vector by the matrix.
  pub fn multiply_vector(&self, pin: &[f32; 4]) -> [f32; 4] {
    [
      pin[0] * self.m11 + pin[1] * self.m21 + pin[2] * self.m31 + pin[3] * self.m41,
      pin[0] * self.m12 + pin[1] * self.m22 + pin[2] * self.m32 + pin[3] * self.m42,
      pin[0] * self.m13 + pin[1] * self.m23 + pin[2] * self.m33 + pin[3] * self.m43,
      pin[0] * self.m14 + pin[1] * self.m24 + pin[2] * self.m34 + pin[3] * self.m44,
    ]
  }

  /// Decomposes the matrix into a list of transform functions if possible.
  pub fn decompose(&self) -> Option<TransformList> {
    // https://drafts.csswg.org/css-transforms-2/#decomposing-a-3d-matrix
    // Combine 2 point.
    let combine = |a: [f32; 3], b: [f32; 3], ascl: f32, bscl: f32| {
      [
        (ascl * a[0]) + (bscl * b[0]),
        (ascl * a[1]) + (bscl * b[1]),
        (ascl * a[2]) + (bscl * b[2]),
      ]
    };

    // Dot product.
    let dot = |a: [f32; 3], b: [f32; 3]| a[0] * b[0] + a[1] * b[1] + a[2] * b[2];

    // Cross product.
    let cross = |row1: [f32; 3], row2: [f32; 3]| {
      [
        row1[1] * row2[2] - row1[2] * row2[1],
        row1[2] * row2[0] - row1[0] * row2[2],
        row1[0] * row2[1] - row1[1] * row2[0],
      ]
    };

    if self.m44 == 0.0 {
      return None;
    }

    let scaling_factor = self.m44;

    // Normalize the matrix.
    let mut matrix = self.clone();
    matrix.scale_by_factor(1.0 / scaling_factor);

    // perspective_matrix is used to solve for perspective, but it also provides
    // an easy way to test for singularity of the upper 3x3 component.
    let mut perspective_matrix = matrix.clone();
    perspective_matrix.m14 = 0.0;
    perspective_matrix.m24 = 0.0;
    perspective_matrix.m34 = 0.0;
    perspective_matrix.m44 = 1.0;

    if perspective_matrix.determinant() == 0.0 {
      return None;
    }

    let mut transforms = vec![];

    // First, isolate perspective.
    if matrix.m14 != 0.0 || matrix.m24 != 0.0 || matrix.m34 != 0.0 {
      let right_hand_side: [f32; 4] = [matrix.m14, matrix.m24, matrix.m34, matrix.m44];

      perspective_matrix = perspective_matrix.inverse().unwrap().transpose();
      let perspective = perspective_matrix.multiply_vector(&right_hand_side);
      if perspective[0] == 0.0 && perspective[1] == 0.0 && perspective[3] == 0.0 {
        transforms.push(Transform::Perspective(Length::px(-1.0 / perspective[2])))
      } else {
        return None;
      }
    }

    // Next take care of translation (easy).
    // let translate = Translate3D(matrix.m41, matrix.m42, matrix.m43);
    if matrix.m41 != 0.0 || matrix.m42 != 0.0 || matrix.m43 != 0.0 {
      transforms.push(Transform::Translate3d(
        LengthPercentage::px(matrix.m41),
        LengthPercentage::px(matrix.m42),
        Length::px(matrix.m43),
      ));
    }

    // Now get scale and shear. 'row' is a 3 element array of 3 component vectors
    let mut row = [
      [matrix.m11, matrix.m12, matrix.m13],
      [matrix.m21, matrix.m22, matrix.m23],
      [matrix.m31, matrix.m32, matrix.m33],
    ];

    // Compute X scale factor and normalize first row.
    let row0len = (row[0][0] * row[0][0] + row[0][1] * row[0][1] + row[0][2] * row[0][2]).sqrt();
    let mut scale_x = row0len;
    row[0] = [row[0][0] / row0len, row[0][1] / row0len, row[0][2] / row0len];

    // Compute XY shear factor and make 2nd row orthogonal to 1st.
    let mut skew_x = dot(row[0], row[1]);
    row[1] = combine(row[1], row[0], 1.0, -skew_x);

    // Now, compute Y scale and normalize 2nd row.
    let row1len = (row[1][0] * row[1][0] + row[1][1] * row[1][1] + row[1][2] * row[1][2]).sqrt();
    let mut scale_y = row1len;
    row[1] = [row[1][0] / row1len, row[1][1] / row1len, row[1][2] / row1len];
    skew_x /= scale_y;

    // Compute XZ and YZ shears, orthogonalize 3rd row
    let mut skew_y = dot(row[0], row[2]);
    row[2] = combine(row[2], row[0], 1.0, -skew_y);
    let mut skew_z = dot(row[1], row[2]);
    row[2] = combine(row[2], row[1], 1.0, -skew_z);

    // Next, get Z scale and normalize 3rd row.
    let row2len = (row[2][0] * row[2][0] + row[2][1] * row[2][1] + row[2][2] * row[2][2]).sqrt();
    let mut scale_z = row2len;
    row[2] = [row[2][0] / row2len, row[2][1] / row2len, row[2][2] / row2len];
    skew_y /= scale_z;
    skew_z /= scale_z;

    if skew_z != 0.0 {
      return None; // ???
    }

    // Round to 5 digits of precision, which is what we print.
    macro_rules! round {
      ($var: ident) => {
        $var = ($var * 100000.0).round() / 100000.0;
      };
    }

    round!(skew_x);
    round!(skew_y);
    round!(skew_z);

    if skew_x != 0.0 || skew_y != 0.0 || skew_z != 0.0 {
      transforms.push(Transform::Skew(Angle::Rad(skew_x), Angle::Rad(skew_y)));
    }

    // At this point, the matrix (in rows) is orthonormal.
    // Check for a coordinate system flip.  If the determinant
    // is -1, then negate the matrix and the scaling factors.
    if dot(row[0], cross(row[1], row[2])) < 0.0 {
      scale_x = -scale_x;
      scale_y = -scale_y;
      scale_z = -scale_z;
      for i in 0..3 {
        row[i][0] *= -1.0;
        row[i][1] *= -1.0;
        row[i][2] *= -1.0;
      }
    }

    round!(scale_x);
    round!(scale_y);
    round!(scale_z);

    if scale_x != 1.0 || scale_y != 1.0 || scale_z != 1.0 {
      transforms.push(Transform::Scale3d(
        NumberOrPercentage::Number(scale_x),
        NumberOrPercentage::Number(scale_y),
        NumberOrPercentage::Number(scale_z),
      ))
    }

    // Now, get the rotations out.
    let mut rotate_x = 0.5 * ((1.0 + row[0][0] - row[1][1] - row[2][2]).max(0.0)).sqrt();
    let mut rotate_y = 0.5 * ((1.0 - row[0][0] + row[1][1] - row[2][2]).max(0.0)).sqrt();
    let mut rotate_z = 0.5 * ((1.0 - row[0][0] - row[1][1] + row[2][2]).max(0.0)).sqrt();
    let rotate_w = 0.5 * ((1.0 + row[0][0] + row[1][1] + row[2][2]).max(0.0)).sqrt();

    if row[2][1] > row[1][2] {
      rotate_x = -rotate_x
    }

    if row[0][2] > row[2][0] {
      rotate_y = -rotate_y
    }

    if row[1][0] > row[0][1] {
      rotate_z = -rotate_z
    }

    let len = (rotate_x * rotate_x + rotate_y * rotate_y + rotate_z * rotate_z).sqrt();
    if len != 0.0 {
      rotate_x /= len;
      rotate_y /= len;
      rotate_z /= len;
    }
    let a = 2.0 * len.atan2(rotate_w);

    // normalize the vector so one of the values is 1
    let max = rotate_x.max(rotate_y).max(rotate_z);
    rotate_x /= max;
    rotate_y /= max;
    rotate_z /= max;

    if a != 0.0 {
      transforms.push(Transform::Rotate3d(rotate_x, rotate_y, rotate_z, Angle::Rad(a)))
    }

    if transforms.is_empty() {
      return None;
    }

    Some(TransformList(transforms))
  }
}

impl<'i> Parse<'i> for Transform {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
            Ok(Transform::Translate(x, LengthPercentage::zero()))
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
          let angle = Angle::parse_with_unitless_zero(input)?;
          Ok(Transform::Rotate(angle))
        },
        "rotatex" => {
          let angle = Angle::parse_with_unitless_zero(input)?;
          Ok(Transform::RotateX(angle))
        },
        "rotatey" => {
          let angle = Angle::parse_with_unitless_zero(input)?;
          Ok(Transform::RotateY(angle))
        },
        "rotatez" => {
          let angle = Angle::parse_with_unitless_zero(input)?;
          Ok(Transform::RotateZ(angle))
        },
        "rotate3d" => {
          let x = f32::parse(input)?;
          input.expect_comma()?;
          let y = f32::parse(input)?;
          input.expect_comma()?;
          let z = f32::parse(input)?;
          input.expect_comma()?;
          let angle = Angle::parse_with_unitless_zero(input)?;
          Ok(Transform::Rotate3d(x, y, z, angle))
        },
        "skew" => {
          let x = Angle::parse_with_unitless_zero(input)?;
          if input.try_parse(|input| input.expect_comma()).is_ok() {
            let y = Angle::parse_with_unitless_zero(input)?;
            Ok(Transform::Skew(x, y))
          } else {
            Ok(Transform::Skew(x, Angle::Deg(0.0)))
          }
        },
        "skewx" => {
          let angle = Angle::parse_with_unitless_zero(input)?;
          Ok(Transform::SkewX(angle))
        },
        "skewy" => {
          let angle = Angle::parse_with_unitless_zero(input)?;
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use Transform::*;
    match self {
      Translate(x, y) => {
        if dest.minify && x.is_zero() && !y.is_zero() {
          dest.write_str("translateY(")?;
          y.to_css(dest)?
        } else {
          dest.write_str("translate(")?;
          x.to_css(dest)?;
          if !y.is_zero() {
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
        if dest.minify && !x.is_zero() && y.is_zero() && z.is_zero() {
          dest.write_str("translate(")?;
          x.to_css(dest)?;
        } else if dest.minify && x.is_zero() && !y.is_zero() && z.is_zero() {
          dest.write_str("translateY(")?;
          y.to_css(dest)?;
        } else if dest.minify && x.is_zero() && y.is_zero() && !z.is_zero() {
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
        let x: f32 = x.into();
        let y: f32 = y.into();
        if dest.minify && x == 1.0 && y != 1.0 {
          dest.write_str("scaleY(")?;
          y.to_css(dest)?;
        } else if dest.minify && x != 1.0 && y == 1.0 {
          dest.write_str("scaleX(")?;
          x.to_css(dest)?;
        } else {
          dest.write_str("scale(")?;
          x.to_css(dest)?;
          if y != x {
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
        let x: f32 = x.into();
        let y: f32 = y.into();
        let z: f32 = z.into();
        if dest.minify && z == 1.0 && x == y {
          // scale3d(x, x, 1) => scale(x)
          dest.write_str("scale(")?;
          x.to_css(dest)?;
        } else if dest.minify && x != 1.0 && y == 1.0 && z == 1.0 {
          // scale3d(x, 1, 1) => scaleX(x)
          dest.write_str("scaleX(")?;
          x.to_css(dest)?;
        } else if dest.minify && x == 1.0 && y != 1.0 && z == 1.0 {
          // scale3d(1, y, 1) => scaleY(y)
          dest.write_str("scaleY(")?;
          y.to_css(dest)?;
        } else if dest.minify && x == 1.0 && y == 1.0 && z != 1.0 {
          // scale3d(1, 1, z) => scaleZ(z)
          dest.write_str("scaleZ(")?;
          z.to_css(dest)?;
        } else if dest.minify && z == 1.0 {
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
        angle.to_css_with_unitless_zero(dest)?;
        dest.write_char(')')
      }
      RotateX(angle) => {
        dest.write_str("rotateX(")?;
        angle.to_css_with_unitless_zero(dest)?;
        dest.write_char(')')
      }
      RotateY(angle) => {
        dest.write_str("rotateY(")?;
        angle.to_css_with_unitless_zero(dest)?;
        dest.write_char(')')
      }
      RotateZ(angle) => {
        dest.write_str(if dest.minify { "rotate(" } else { "rotateZ(" })?;
        angle.to_css_with_unitless_zero(dest)?;
        dest.write_char(')')
      }
      Rotate3d(x, y, z, angle) => {
        if dest.minify && *x == 1.0 && *y == 0.0 && *z == 0.0 {
          // rotate3d(1, 0, 0, a) => rotateX(a)
          dest.write_str("rotateX(")?;
          angle.to_css_with_unitless_zero(dest)?;
        } else if dest.minify && *x == 0.0 && *y == 1.0 && *z == 0.0 {
          // rotate3d(0, 1, 0, a) => rotateY(a)
          dest.write_str("rotateY(")?;
          angle.to_css_with_unitless_zero(dest)?;
        } else if dest.minify && *x == 0.0 && *y == 0.0 && *z == 1.0 {
          // rotate3d(0, 0, 1, a) => rotate(a)
          dest.write_str("rotate(")?;
          angle.to_css_with_unitless_zero(dest)?;
        } else {
          dest.write_str("rotate3d(")?;
          x.to_css(dest)?;
          dest.delim(',', false)?;
          y.to_css(dest)?;
          dest.delim(',', false)?;
          z.to_css(dest)?;
          dest.delim(',', false)?;
          angle.to_css_with_unitless_zero(dest)?;
        }
        dest.write_char(')')
      }
      Skew(x, y) => {
        if dest.minify && x.is_zero() && !y.is_zero() {
          dest.write_str("skewY(")?;
          y.to_css_with_unitless_zero(dest)?
        } else {
          dest.write_str("skew(")?;
          x.to_css(dest)?;
          if !y.is_zero() {
            dest.delim(',', false)?;
            y.to_css_with_unitless_zero(dest)?;
          }
        }
        dest.write_char(')')
      }
      SkewX(angle) => {
        dest.write_str(if dest.minify { "skew(" } else { "skewX(" })?;
        angle.to_css_with_unitless_zero(dest)?;
        dest.write_char(')')
      }
      SkewY(angle) => {
        dest.write_str("skewY(")?;
        angle.to_css_with_unitless_zero(dest)?;
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
        m11,
        m12,
        m13,
        m14,
        m21,
        m22,
        m23,
        m24,
        m31,
        m32,
        m33,
        m34,
        m41,
        m42,
        m43,
        m44,
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
  /// Converts the transform to a 3D matrix.
  pub fn to_matrix(&self) -> Option<Matrix3d<f32>> {
    macro_rules! to_radians {
      ($angle: ident) => {{
        // If the angle is negative or more than a full circle, we cannot
        // safely convert to a matrix. Transforms are interpolated numerically
        // when types match, and this will have different results than
        // when interpolating matrices with large angles.
        // https://www.w3.org/TR/css-transforms-1/#matrix-interpolation
        let rad = $angle.to_radians();
        if rad < 0.0 || rad >= 2.0 * PI {
          return None;
        }

        rad
      }};
    }

    match &self {
      Transform::Translate(LengthPercentage::Dimension(x), LengthPercentage::Dimension(y)) => {
        if let (Some(x), Some(y)) = (x.to_px(), y.to_px()) {
          return Some(Matrix3d::translate(x, y, 0.0));
        }
      }
      Transform::TranslateX(LengthPercentage::Dimension(x)) => {
        if let Some(x) = x.to_px() {
          return Some(Matrix3d::translate(x, 0.0, 0.0));
        }
      }
      Transform::TranslateY(LengthPercentage::Dimension(y)) => {
        if let Some(y) = y.to_px() {
          return Some(Matrix3d::translate(0.0, y, 0.0));
        }
      }
      Transform::TranslateZ(z) => {
        if let Some(z) = z.to_px() {
          return Some(Matrix3d::translate(0.0, 0.0, z));
        }
      }
      Transform::Translate3d(LengthPercentage::Dimension(x), LengthPercentage::Dimension(y), z) => {
        if let (Some(x), Some(y), Some(z)) = (x.to_px(), y.to_px(), z.to_px()) {
          return Some(Matrix3d::translate(x, y, z));
        }
      }
      Transform::Scale(x, y) => return Some(Matrix3d::scale(x.into(), y.into(), 1.0)),
      Transform::ScaleX(x) => return Some(Matrix3d::scale(x.into(), 1.0, 1.0)),
      Transform::ScaleY(y) => return Some(Matrix3d::scale(1.0, y.into(), 1.0)),
      Transform::ScaleZ(z) => return Some(Matrix3d::scale(1.0, 1.0, z.into())),
      Transform::Scale3d(x, y, z) => return Some(Matrix3d::scale(x.into(), y.into(), z.into())),
      Transform::Rotate(angle) | Transform::RotateZ(angle) => {
        return Some(Matrix3d::rotate(0.0, 0.0, 1.0, to_radians!(angle)))
      }
      Transform::RotateX(angle) => return Some(Matrix3d::rotate(1.0, 0.0, 0.0, to_radians!(angle))),
      Transform::RotateY(angle) => return Some(Matrix3d::rotate(0.0, 1.0, 0.0, to_radians!(angle))),
      Transform::Rotate3d(x, y, z, angle) => return Some(Matrix3d::rotate(*x, *y, *z, to_radians!(angle))),
      Transform::Skew(x, y) => return Some(Matrix3d::skew(to_radians!(x), to_radians!(y))),
      Transform::SkewX(x) => return Some(Matrix3d::skew(to_radians!(x), 0.0)),
      Transform::SkewY(y) => return Some(Matrix3d::skew(0.0, to_radians!(y))),
      Transform::Perspective(len) => {
        if let Some(len) = len.to_px() {
          return Some(Matrix3d::perspective(len));
        }
      }
      Transform::Matrix(m) => return Some(m.to_matrix3d()),
      Transform::Matrix3d(m) => return Some(m.clone()),
      _ => {}
    }
    None
  }
}

enum_property! {
  /// A value for the [transform-style](https://drafts.csswg.org/css-transforms-2/#transform-style-property) property.
  #[allow(missing_docs)]
  pub enum TransformStyle {
    "flat": Flat,
    "preserve-3d": Preserve3d,
  }
}

enum_property! {
  /// A value for the [transform-box](https://drafts.csswg.org/css-transforms-1/#transform-box) property.
  pub enum TransformBox {
    /// Uses the content box as reference box.
    "content-box": ContentBox,
    /// Uses the border box as reference box.
    "border-box": BorderBox,
    /// Uses the object bounding box as reference box.
    "fill-box": FillBox,
    /// Uses the stroke bounding box as reference box.
    "stroke-box": StrokeBox,
    /// Uses the nearest SVG viewport as reference box.
    "view-box": ViewBox,
  }
}

enum_property! {
  /// A value for the [backface-visibility](https://drafts.csswg.org/css-transforms-2/#backface-visibility-property) property.
  #[allow(missing_docs)]
  pub enum BackfaceVisibility {
    Visible,
    Hidden,
  }
}

/// A value for the [perspective](https://drafts.csswg.org/css-transforms-2/#perspective-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Perspective {
  /// No perspective transform is applied.
  None,
  /// Distance to the center of projection.
  Length(Length),
}

impl<'i> Parse<'i> for Perspective {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(Perspective::None);
    }

    Ok(Perspective::Length(Length::parse(input)?))
  }
}

impl ToCss for Perspective {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Perspective::None => dest.write_str("none"),
      Perspective::Length(len) => len.to_css(dest),
    }
  }
}

/// A value for the [translate](https://drafts.csswg.org/css-transforms-2/#propdef-translate) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Translate {
  /// The x translation.
  pub x: LengthPercentage,
  /// The y translation.
  pub y: LengthPercentage,
  /// The z translation.
  pub z: Length,
}

impl<'i> Parse<'i> for Translate {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Translate {
        x: LengthPercentage::zero(),
        y: LengthPercentage::zero(),
        z: Length::zero(),
      });
    }

    let x = LengthPercentage::parse(input)?;
    let y = input.try_parse(LengthPercentage::parse);
    let z = if y.is_ok() {
      input.try_parse(Length::parse).ok()
    } else {
      None
    };

    Ok(Translate {
      x,
      y: y.unwrap_or(LengthPercentage::zero()),
      z: z.unwrap_or(Length::zero()),
    })
  }
}

impl ToCss for Translate {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x.to_css(dest)?;
    if !self.y.is_zero() || !self.z.is_zero() {
      dest.write_char(' ')?;
      self.y.to_css(dest)?;
      if !self.z.is_zero() {
        dest.write_char(' ')?;
        self.z.to_css(dest)?;
      }
    }
    Ok(())
  }
}

impl Translate {
  /// Converts the translation to a transform function.
  pub fn to_transform(&self) -> Transform {
    Transform::Translate3d(self.x.clone(), self.y.clone(), self.z.clone())
  }
}

/// A value for the [rotate](https://drafts.csswg.org/css-transforms-2/#propdef-rotate) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Rotate {
  /// Rotation around the x axis.
  pub x: f32,
  /// Rotation around the y axis.
  pub y: f32,
  /// Rotation around the z axis.
  pub z: f32,
  /// The angle of rotation.
  pub angle: Angle,
}

impl<'i> Parse<'i> for Rotate {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Rotate {
        x: 0.0,
        y: 0.0,
        z: 1.0,
        angle: Angle::Deg(0.0),
      });
    }

    let angle = input.try_parse(Angle::parse);
    let (x, y, z) = input
      .try_parse(|input| {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        match_ignore_ascii_case! { &*ident,
          "x" => Ok((1.0, 0.0, 0.0)),
          "y" => Ok((0.0, 1.0, 0.0)),
          "z" => Ok((0.0, 0.0, 1.0)),
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        }
      })
      .or_else(
        |_: ParseError<'i, ParserError<'i>>| -> Result<_, ParseError<'i, ParserError<'i>>> {
          input.try_parse(|input| Ok((f32::parse(input)?, f32::parse(input)?, f32::parse(input)?)))
        },
      )
      .unwrap_or((0.0, 0.0, 1.0));
    let angle = angle.or_else(|_| Angle::parse(input))?;
    Ok(Rotate { x, y, z, angle })
  }
}

impl ToCss for Rotate {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.x == 0.0 && self.y == 0.0 && self.z == 1.0 && self.angle.is_zero() {
      dest.write_str("none")?;
      return Ok(());
    }

    if self.x == 1.0 && self.y == 0.0 && self.z == 0.0 {
      dest.write_str("x ")?;
    } else if self.x == 0.0 && self.y == 1.0 && self.z == 0.0 {
      dest.write_str("y ")?;
    } else if !(self.x == 0.0 && self.y == 0.0 && self.z == 1.0) {
      self.x.to_css(dest)?;
      dest.write_char(' ')?;
      self.y.to_css(dest)?;
      dest.write_char(' ')?;
      self.z.to_css(dest)?;
      dest.write_char(' ')?;
    }

    self.angle.to_css(dest)
  }
}

impl Rotate {
  /// Converts the rotation to a transform function.
  pub fn to_transform(&self) -> Transform {
    Transform::Rotate3d(self.x, self.y, self.z, self.angle.clone())
  }
}

/// A value for the [scale](https://drafts.csswg.org/css-transforms-2/#propdef-scale) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Scale {
  /// Scale on the x axis.
  pub x: NumberOrPercentage,
  /// Scale on the y axis.
  pub y: NumberOrPercentage,
  /// Scale on the z axis.
  pub z: NumberOrPercentage,
}

impl<'i> Parse<'i> for Scale {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(Scale {
        x: NumberOrPercentage::Number(1.0),
        y: NumberOrPercentage::Number(1.0),
        z: NumberOrPercentage::Number(1.0),
      });
    }

    let x = NumberOrPercentage::parse(input)?;
    let y = input.try_parse(NumberOrPercentage::parse);
    let z = if y.is_ok() {
      input.try_parse(NumberOrPercentage::parse).ok()
    } else {
      None
    };

    Ok(Scale {
      x: x.clone(),
      y: y.unwrap_or(x),
      z: z.unwrap_or(NumberOrPercentage::Number(1.0)),
    })
  }
}

impl ToCss for Scale {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x.to_css(dest)?;
    let zv: f32 = (&self.z).into();
    if self.y != self.x || zv != 1.0 {
      dest.write_char(' ')?;
      self.y.to_css(dest)?;
      if zv != 1.0 {
        dest.write_char(' ')?;
        self.z.to_css(dest)?;
      }
    }

    Ok(())
  }
}

impl Scale {
  /// Converts the scale to a transform function.
  pub fn to_transform(&self) -> Transform {
    Transform::Scale3d(self.x.clone(), self.y.clone(), self.z.clone())
  }
}

#[derive(Default)]
pub(crate) struct TransformHandler {
  transform: Option<(TransformList, VendorPrefix)>,
  translate: Option<Translate>,
  rotate: Option<Rotate>,
  scale: Option<Scale>,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for TransformHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    macro_rules! individual_property {
      ($prop: ident, $val: ident) => {
        if let Some((transform, _)) = &mut self.transform {
          transform.0.push($val.to_transform())
        } else {
          self.$prop = Some($val.clone());
          self.has_any = true;
        }
      };
    }

    match property {
      Transform(val, vp) => {
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((cur, prefixes)) = &self.transform {
          if cur != val && !prefixes.contains(*vp) {
            self.flush(dest, context);
          }
        }

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.transform {
          *val = val.clone();
          *prefixes |= *vp;
        } else {
          self.transform = Some((val.clone(), *vp));
          self.has_any = true;
        }

        self.translate = None;
        self.rotate = None;
        self.scale = None;
      }
      Translate(val) => individual_property!(translate, val),
      Rotate(val) => individual_property!(rotate, val),
      Scale(val) => individual_property!(scale, val),
      Unparsed(val)
        if matches!(
          val.property_id,
          PropertyId::Transform(_) | PropertyId::Translate | PropertyId::Rotate | PropertyId::Scale
        ) =>
      {
        self.flush(dest, context);
        let prop = if matches!(val.property_id, PropertyId::Transform(_)) {
          Property::Unparsed(val.get_prefixed(context.targets, Feature::Transform))
        } else {
          property.clone()
        };
        dest.push(prop)
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
  }
}

impl TransformHandler {
  fn flush<'i>(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    let transform = std::mem::take(&mut self.transform);
    let translate = std::mem::take(&mut self.translate);
    let rotate = std::mem::take(&mut self.rotate);
    let scale = std::mem::take(&mut self.scale);

    if let Some((transform, prefix)) = transform {
      let prefix = context.targets.prefixes(prefix, Feature::Transform);
      dest.push(Property::Transform(transform, prefix))
    }

    if let Some(translate) = translate {
      dest.push(Property::Translate(translate))
    }

    if let Some(rotate) = rotate {
      dest.push(Property::Rotate(rotate))
    }

    if let Some(scale) = scale {
      dest.push(Property::Scale(scale))
    }
  }
}
