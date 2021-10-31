use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{length::{LengthPercentage, NumberOrPercentage, Length, Angle}, ident::CustomIdent, time::Time, easing::EasingFunction};
use super::Property;
use crate::printer::Printer;
use std::fmt::Write;
use itertools::izip;
use smallvec::SmallVec;

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
    let mut first = true;
    for item in &self.0 {
      if first {
        first = false;
      } else {
        dest.delim(',', false)?;
      }
      item.to_css(dest)?;
    }
    Ok(())
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
        dest.write_str("translate3d(")?;
        x.to_css(dest)?;
        dest.delim(',', false)?;
        y.to_css(dest)?;
        dest.delim(',', false)?;
        z.to_css(dest)?;
        dest.write_char(')')
      }
      Scale(x, y) => {
        if *x == 1.0 && *y != 1.0 {
          dest.write_str("scaleY(")?;
          y.to_css(dest)?;
        } else if *x != 1.0 && *y == 1.0 {
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
      Scale3d(x, y, z) => {
        dest.write_str("scale3d(")?;
        x.to_css(dest)?;
        dest.delim(',', false)?;
        y.to_css(dest)?;
        dest.delim(',', false)?;
        z.to_css(dest)?;
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
        dest.write_str("rotateZ(")?;
        angle.to_css(dest)?;
        dest.write_char(')')
      }
      Rotate3d(x, y, z, angle) => {
        dest.write_str("rotate3d(")?;
        x.to_css(dest)?;
        dest.delim(',', false)?;
        y.to_css(dest)?;
        dest.delim(',', false)?;
        z.to_css(dest)?;
        dest.delim(',', false)?;
        angle.to_css(dest)?;
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
