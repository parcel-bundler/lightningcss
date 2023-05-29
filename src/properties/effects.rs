//! CSS properties related to filters and effects.

use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, ToCss, Zero};
use crate::values::color::ColorFallbackKind;
use crate::values::{angle::Angle, color::CssColor, length::Length, percentage::NumberOrPercentage, url::Url};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;

/// A [filter](https://drafts.fxtf.org/filter-effects-1/#filter-functions) function.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Filter<'i> {
  /// A `blur()` filter.
  Blur(Length),
  /// A `brightness()` filter.
  Brightness(NumberOrPercentage),
  /// A `contrast()` filter.
  Contrast(NumberOrPercentage),
  /// A `grayscale()` filter.
  Grayscale(NumberOrPercentage),
  /// A `hue-rotate()` filter.
  HueRotate(Angle),
  /// An `invert()` filter.
  Invert(NumberOrPercentage),
  /// An `opacity()` filter.
  Opacity(NumberOrPercentage),
  /// A `saturate()` filter.
  Saturate(NumberOrPercentage),
  /// A `sepia()` filter.
  Sepia(NumberOrPercentage),
  /// A `drop-shadow()` filter.
  DropShadow(DropShadow),
  /// A `url()` reference to an SVG filter.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Url(Url<'i>),
}

impl<'i> Parse<'i> for Filter<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      return Ok(Filter::Url(url));
    }

    let location = input.current_source_location();
    let function = input.expect_function()?;
    match_ignore_ascii_case! { &function,
      "blur" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Blur(input.try_parse(Length::parse).unwrap_or(Length::zero())))
        })
      },
      "brightness" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Brightness(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "contrast" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Contrast(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "grayscale" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Grayscale(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "hue-rotate" => {
        input.parse_nested_block(|input| {
          // Spec has an exception for unitless zero angles: https://github.com/w3c/fxtf-drafts/issues/228
          Ok(Filter::HueRotate(input.try_parse(Angle::parse_with_unitless_zero).unwrap_or(Angle::zero())))
        })
      },
      "invert" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Invert(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "opacity" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Opacity(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "saturate" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Saturate(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "sepia" => {
        input.parse_nested_block(|input| {
          Ok(Filter::Sepia(input.try_parse(NumberOrPercentage::parse).unwrap_or(NumberOrPercentage::Number(1.0))))
        })
      },
      "drop-shadow" => {
        input.parse_nested_block(|input| {
          Ok(Filter::DropShadow(DropShadow::parse(input)?))
        })
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(function.clone())
      ))
    }
  }
}

impl<'i> ToCss for Filter<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Filter::Blur(val) => {
        dest.write_str("blur(")?;
        if *val != Length::zero() {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Brightness(val) => {
        dest.write_str("brightness(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Contrast(val) => {
        dest.write_str("contrast(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Grayscale(val) => {
        dest.write_str("grayscale(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::HueRotate(val) => {
        dest.write_str("hue-rotate(")?;
        if !val.is_zero() {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Invert(val) => {
        dest.write_str("invert(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Opacity(val) => {
        dest.write_str("opacity(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Saturate(val) => {
        dest.write_str("saturate(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Sepia(val) => {
        dest.write_str("sepia(")?;
        let v: f32 = val.into();
        if v != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::DropShadow(val) => {
        dest.write_str("drop-shadow(")?;
        val.to_css(dest)?;
        dest.write_char(')')
      }
      Filter::Url(url) => url.to_css(dest),
    }
  }
}

impl<'i> Filter<'i> {
  fn get_fallback(&self, kind: ColorFallbackKind) -> Self {
    match self {
      Filter::DropShadow(shadow) => Filter::DropShadow(shadow.get_fallback(kind)),
      _ => self.clone(),
    }
  }
}

impl IsCompatible for Filter<'_> {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

/// A [`drop-shadow()`](https://drafts.fxtf.org/filter-effects-1/#funcdef-filter-drop-shadow) filter function.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct DropShadow {
  /// The color of the drop shadow.
  pub color: CssColor,
  /// The x offset of the drop shadow.
  pub x_offset: Length,
  /// The y offset of the drop shadow.
  pub y_offset: Length,
  /// The blur radius of the drop shadow.
  pub blur: Length,
}

impl<'i> Parse<'i> for DropShadow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color = None;
    let mut lengths = None;

    loop {
      if lengths.is_none() {
        let value = input.try_parse::<_, _, ParseError<ParserError<'i>>>(|input| {
          let horizontal = Length::parse(input)?;
          let vertical = Length::parse(input)?;
          let blur = input.try_parse(Length::parse).unwrap_or(Length::zero());
          Ok((horizontal, vertical, blur))
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

      break;
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(DropShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2,
    })
  }
}

impl ToCss for DropShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x_offset.to_css(dest)?;
    dest.write_char(' ')?;
    self.y_offset.to_css(dest)?;

    if self.blur != Length::zero() {
      dest.write_char(' ')?;
      self.blur.to_css(dest)?;
    }

    if self.color != CssColor::current_color() {
      dest.write_char(' ')?;
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}

impl DropShadow {
  fn get_fallback(&self, kind: ColorFallbackKind) -> DropShadow {
    DropShadow {
      color: self.color.get_fallback(kind),
      ..self.clone()
    }
  }
}

/// A value for the [filter](https://drafts.fxtf.org/filter-effects-1/#FilterProperty) and
/// [backdrop-filter](https://drafts.fxtf.org/filter-effects-2/#BackdropFilterProperty) properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum FilterList<'i> {
  /// The `none` keyword.
  None,
  /// A list of filter functions.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Filters(SmallVec<[Filter<'i>; 1]>),
}

impl<'i> Parse<'i> for FilterList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(FilterList::None);
    }

    let mut filters = SmallVec::new();
    while let Ok(filter) = input.try_parse(Filter::parse) {
      filters.push(filter);
    }

    Ok(FilterList::Filters(filters))
  }
}

impl<'i> ToCss for FilterList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      FilterList::None => dest.write_str("none"),
      FilterList::Filters(filters) => {
        let mut first = true;
        for filter in filters {
          if first {
            first = false;
          } else {
            dest.whitespace()?;
          }
          filter.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl<'i> FallbackValues for FilterList<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    let mut res = Vec::new();
    let mut fallbacks = ColorFallbackKind::empty();
    if let FilterList::Filters(filters) = self {
      for shadow in filters.iter() {
        if let Filter::DropShadow(shadow) = &shadow {
          fallbacks |= shadow.color.get_necessary_fallbacks(targets);
        }
      }

      if fallbacks.contains(ColorFallbackKind::RGB) {
        res.push(FilterList::Filters(
          filters
            .iter()
            .map(|filter| filter.get_fallback(ColorFallbackKind::RGB))
            .collect(),
        ));
      }

      if fallbacks.contains(ColorFallbackKind::P3) {
        res.push(FilterList::Filters(
          filters
            .iter()
            .map(|filter| filter.get_fallback(ColorFallbackKind::P3))
            .collect(),
        ));
      }

      if fallbacks.contains(ColorFallbackKind::LAB) {
        for filter in filters.iter_mut() {
          *filter = filter.get_fallback(ColorFallbackKind::LAB);
        }
      }
    }

    res
  }
}

impl IsCompatible for FilterList<'_> {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}
