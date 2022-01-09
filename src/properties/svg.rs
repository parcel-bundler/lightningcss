use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::values::length::LengthPercentage;
use crate::macros::enum_property;
use crate::error::{ParserError, PrinterError};
use crate::values::{url::Url, color::CssColor};

/// https://www.w3.org/TR/SVG2/painting.html#SpecifyingPaint
#[derive(Debug, Clone, PartialEq)]
pub enum SVGPaint {
  None,
  Url(Url, Option<SVGPaintFallback>),
  Color(CssColor),
  ContextFill,
  ContextStroke
}

#[derive(Debug, Clone, PartialEq)]
pub enum SVGPaintFallback {
  None,
  Color(CssColor)
}

impl Parse for SVGPaint {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      let fallback = input.try_parse(SVGPaintFallback::parse).ok();
      return Ok(SVGPaint::Url(url, fallback))
    }

    if let Ok(color) = input.try_parse(CssColor::parse) {
      return Ok(SVGPaint::Color(color))
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

impl ToCss for SVGPaint {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      SVGPaint::None => dest.write_str("none"),
      SVGPaint::Url(url, fallback) => {
        url.to_css(dest)?;
        if let Some(fallback) = fallback {
          dest.write_char(' ')?;
          fallback.to_css(dest)?;
        }
        Ok(())
      }
      SVGPaint::Color(color) => color.to_css(dest),
      SVGPaint::ContextFill => dest.write_str("context-fill"),
      SVGPaint::ContextStroke => dest.write_str("context-stroke")
    }
  }
}

impl Parse for SVGPaintFallback {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(SVGPaintFallback::None)
    }

    Ok(SVGPaintFallback::Color(CssColor::parse(input)?))
  }
}

impl ToCss for SVGPaintFallback {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      SVGPaintFallback::None => dest.write_str("none"),
      SVGPaintFallback::Color(color) => color.to_css(dest)
    }
  }
}

enum_property!(StrokeLinecap,
  Butt,
  Round,
  Square
);

enum_property!(StrokeLinejoin,
  ("miter", Miter),
  ("miter-clip", MiterClip),
  ("round", Round),
  ("bevel", Bevel),
  ("arcs", Arcs)
);

/// https://www.w3.org/TR/SVG2/painting.html#StrokeDashing
#[derive(Debug, Clone, PartialEq)]
pub enum StrokeDasharray {
  None,
  Values(Vec<LengthPercentage>)
}

impl Parse for StrokeDasharray {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(StrokeDasharray::None)
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

/// https://www.w3.org/TR/SVG2/painting.html#VertexMarkerProperties
#[derive(Debug, Clone, PartialEq)]
pub enum Marker {
  None,
  Url(Url)
}

impl Parse for Marker {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      return Ok(Marker::Url(url))
    }

    input.expect_ident_matching("none")?;
    Ok(Marker::None)
  }
}

impl ToCss for Marker {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      Marker::None => dest.write_str("none"),
      Marker::Url(url) => url.to_css(dest)
    }
  }
}

enum_property!(ColorInterpolation,
  Auto,
  SRGB,
  LinearRGB
);

enum_property!(ColorRendering,
  Auto,
  OptimizeSpeed,
  OptimizeQuality
);

enum_property!(ShapeRendering,
  Auto,
  OptimizeSpeed,
  CrispEdges,
  GeometricPrecision
);

enum_property!(TextRendering,
  Auto,
  OptimizeSpeed,
  OptimizeLegibility,
  GeometricPrecision
);

enum_property!(ImageRendering,
  Auto,
  OptimizeSpeed,
  OptimizeQuality
);
