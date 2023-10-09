//! CSS properties used in SVG.

use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, ToCss};
use crate::values::length::LengthPercentage;
use crate::values::{color::CssColor, url::Url};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// An SVG [`<paint>`](https://www.w3.org/TR/SVG2/painting.html#SpecifyingPaint) value
/// used in the `fill` and `stroke` properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum SVGPaint<'i> {
  /// No paint.
  None,
  /// A URL reference to a paint server element, e.g. `linearGradient`, `radialGradient`, and `pattern`.
  Url {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// The url of the paint server.
    url: Url<'i>,
    /// A fallback to be used used in case the paint server cannot be resolved.
    fallback: Option<SVGPaintFallback>,
  },
  /// A solid color paint.
  #[cfg_attr(feature = "serde", serde(with = "crate::serialization::ValueWrapper::<CssColor>"))]
  Color(CssColor),
  /// Use the paint value of fill from a context element.
  ContextFill,
  /// Use the paint value of stroke from a context element.
  ContextStroke,
}

/// A fallback for an SVG paint in case a paint server `url()` cannot be resolved.
///
/// See [SVGPaint](SVGPaint).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum SVGPaintFallback {
  /// No fallback.
  None,
  /// A solid color.
  Color(CssColor),
}

impl<'i> Parse<'i> for SVGPaint<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      let fallback = input.try_parse(SVGPaintFallback::parse).ok();
      return Ok(SVGPaint::Url { url, fallback });
    }

    if let Ok(color) = input.try_parse(CssColor::parse) {
      return Ok(SVGPaint::Color(color));
    }

    let location = input.current_source_location();
    let keyword = input.expect_ident()?;
    match_ignore_ascii_case! { &keyword,
      "none" => Ok(SVGPaint::None),
      "context-fill" => Ok(SVGPaint::ContextFill),
      "context-stroke" => Ok(SVGPaint::ContextStroke),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(keyword.clone())
      ))
    }
  }
}

impl<'i> ToCss for SVGPaint<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      SVGPaint::None => dest.write_str("none"),
      SVGPaint::Url { url, fallback } => {
        url.to_css(dest)?;
        if let Some(fallback) = fallback {
          dest.write_char(' ')?;
          fallback.to_css(dest)?;
        }
        Ok(())
      }
      SVGPaint::Color(color) => color.to_css(dest),
      SVGPaint::ContextFill => dest.write_str("context-fill"),
      SVGPaint::ContextStroke => dest.write_str("context-stroke"),
    }
  }
}

impl<'i> Parse<'i> for SVGPaintFallback {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(SVGPaintFallback::None);
    }

    Ok(SVGPaintFallback::Color(CssColor::parse(input)?))
  }
}

impl ToCss for SVGPaintFallback {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      SVGPaintFallback::None => dest.write_str("none"),
      SVGPaintFallback::Color(color) => color.to_css(dest),
    }
  }
}

impl<'i> FallbackValues for SVGPaint<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    match self {
      SVGPaint::Color(color) => color
        .get_fallbacks(targets)
        .into_iter()
        .map(|color| SVGPaint::Color(color))
        .collect(),
      SVGPaint::Url {
        url,
        fallback: Some(SVGPaintFallback::Color(color)),
      } => color
        .get_fallbacks(targets)
        .into_iter()
        .map(|color| SVGPaint::Url {
          url: url.clone(),
          fallback: Some(SVGPaintFallback::Color(color)),
        })
        .collect(),
      _ => Vec::new(),
    }
  }
}

impl IsCompatible for SVGPaint<'_> {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      SVGPaint::Color(c)
      | SVGPaint::Url {
        fallback: Some(SVGPaintFallback::Color(c)),
        ..
      } => c.is_compatible(browsers),
      SVGPaint::Url { .. } | SVGPaint::None | SVGPaint::ContextFill | SVGPaint::ContextStroke => true,
    }
  }
}

enum_property! {
  /// A value for the [stroke-linecap](https://www.w3.org/TR/SVG2/painting.html#LineCaps) property.
  pub enum StrokeLinecap {
    /// The stroke does not extend beyond its endpoints.
    Butt,
    /// The ends of the stroke are rounded.
    Round,
    /// The ends of the stroke are squared.
    Square,
  }
}

enum_property! {
  /// A value for the [stroke-linejoin](https://www.w3.org/TR/SVG2/painting.html#LineJoin) property.
  pub enum StrokeLinejoin {
    /// A sharp corner is to be used to join path segments.
    "miter": Miter,
    /// Same as `miter` but clipped beyond `stroke-miterlimit`.
    "miter-clip": MiterClip,
    /// A round corner is to be used to join path segments.
    "round": Round,
    /// A bevelled corner is to be used to join path segments.
    "bevel": Bevel,
    /// An arcs corner is to be used to join path segments.
    "arcs": Arcs,
  }
}

/// A value for the [stroke-dasharray](https://www.w3.org/TR/SVG2/painting.html#StrokeDashing) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum StrokeDasharray {
  /// No dashing is used.
  None,
  /// Specifies a dashing pattern to use.
  Values(Vec<LengthPercentage>),
}

impl<'i> Parse<'i> for StrokeDasharray {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(StrokeDasharray::None);
    }

    input.skip_whitespace();
    let mut results = vec![LengthPercentage::parse(input)?];
    loop {
      input.skip_whitespace();
      let comma_location = input.current_source_location();
      let comma = input.try_parse(|i| i.expect_comma()).is_ok();
      if let Ok(item) = input.try_parse(LengthPercentage::parse) {
        results.push(item);
      } else if comma {
        return Err(comma_location.new_unexpected_token_error(Token::Comma));
      } else {
        break;
      }
    }

    Ok(StrokeDasharray::Values(results))
  }
}

impl ToCss for StrokeDasharray {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      StrokeDasharray::None => dest.write_str("none"),
      StrokeDasharray::Values(values) => {
        let mut first = true;
        for value in values {
          if first {
            first = false;
          } else {
            dest.write_char(' ')?;
          }
          value.to_css_unitless(dest)?;
        }
        Ok(())
      }
    }
  }
}

/// A value for the [marker](https://www.w3.org/TR/SVG2/painting.html#VertexMarkerProperties) properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Marker<'i> {
  /// No marker.
  None,
  /// A url reference to a `<marker>` element.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Url(Url<'i>),
}

impl<'i> Parse<'i> for Marker<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      return Ok(Marker::Url(url));
    }

    input.expect_ident_matching("none")?;
    Ok(Marker::None)
  }
}

impl<'i> ToCss for Marker<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Marker::None => dest.write_str("none"),
      Marker::Url(url) => url.to_css(dest),
    }
  }
}

enum_property! {
  /// A value for the [color-interpolation](https://www.w3.org/TR/SVG2/painting.html#ColorInterpolation) property.
  pub enum ColorInterpolation {
    /// The UA can choose between sRGB or linearRGB.
    Auto,
    /// Color interpolation occurs in the sRGB color space.
    SRGB,
    /// Color interpolation occurs in the linearized RGB color space
    LinearRGB,
  }
}

enum_property! {
  /// A value for the [color-rendering](https://www.w3.org/TR/SVG2/painting.html#ColorRendering) property.
  pub enum ColorRendering {
    /// The UA can choose a tradeoff between speed and quality.
    Auto,
    /// The UA shall optimize speed over quality.
    OptimizeSpeed,
    /// The UA shall optimize quality over speed.
    OptimizeQuality,
  }
}

enum_property! {
  /// A value for the [shape-rendering](https://www.w3.org/TR/SVG2/painting.html#ShapeRendering) property.
  pub enum ShapeRendering {
    /// The UA can choose an appropriate tradeoff.
    Auto,
    /// The UA shall optimize speed.
    OptimizeSpeed,
    /// The UA shall optimize crisp edges.
    CrispEdges,
    /// The UA shall optimize geometric precision.
    GeometricPrecision,
  }
}

enum_property! {
  /// A value for the [text-rendering](https://www.w3.org/TR/SVG2/painting.html#TextRendering) property.
  pub enum TextRendering {
    /// The UA can choose an appropriate tradeoff.
    Auto,
    /// The UA shall optimize speed.
    OptimizeSpeed,
    /// The UA shall optimize legibility.
    OptimizeLegibility,
    /// The UA shall optimize geometric precision.
    GeometricPrecision,
  }
}

enum_property! {
  /// A value for the [image-rendering](https://www.w3.org/TR/SVG2/painting.html#ImageRendering) property.
  pub enum ImageRendering {
    /// The UA can choose a tradeoff between speed and quality.
    Auto,
    /// The UA shall optimize speed over quality.
    OptimizeSpeed,
    /// The UA shall optimize quality over speed.
    OptimizeQuality,
  }
}
