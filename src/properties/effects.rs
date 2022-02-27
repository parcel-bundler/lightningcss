use cssparser::*;
use smallvec::SmallVec;
use crate::declaration::DeclarationList;
use crate::logical::LogicalProperties;
use crate::targets::Browsers;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};
use crate::values::color::ColorFallbackKind;
use super::Property;
use crate::values::{
  url::Url,
  length::Length,
  percentage::NumberOrPercentage,
  angle::Angle,
  color::CssColor
};

/// https://drafts.fxtf.org/filter-effects-1/#FilterProperty
#[derive(Debug, Clone, PartialEq)]
pub enum Filter<'i> {
  Blur(Length),
  Brightness(NumberOrPercentage),
  Contrast(NumberOrPercentage),
  Grayscale(NumberOrPercentage),
  HueRotate(Angle),
  Invert(NumberOrPercentage),
  Opacity(NumberOrPercentage),
  Saturate(NumberOrPercentage),
  Sepia(NumberOrPercentage),
  DropShadow(DropShadow),
  Url(Url<'i>)
}

impl<'i> Parse<'i> for Filter<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(Url::parse) {
      return Ok(Filter::Url(url))
    }
    
    let location = input.current_source_location();
    let function = input.expect_function()?;
    match_ignore_ascii_case!{ &function,
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
          Ok(Filter::HueRotate(input.try_parse(Angle::parse).unwrap_or(Angle::Deg(0.0))))
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Contrast(val) => {
        dest.write_str("contrast(")?;
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Grayscale(val) => {
        dest.write_str("grayscale(")?;
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::HueRotate(val) => {
        dest.write_str("hue-rotate(")?;
        if *val != 0.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Invert(val) => {
        dest.write_str("invert(")?;
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Opacity(val) => {
        dest.write_str("opacity(")?;
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Saturate(val) => {
        dest.write_str("saturate(")?;
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::Sepia(val) => {
        dest.write_str("sepia(")?;
        if *val != 1.0 {
          val.to_css(dest)?;
        }
        dest.write_char(')')
      }
      Filter::DropShadow(val) => {
        dest.write_str("drop-shadow(")?;
        val.to_css(dest)?;
        dest.write_char(')')
      }
      Filter::Url(url) => url.to_css(dest)
    }
  }
}

impl<'i> Filter<'i> {
  fn get_fallback(&self, kind: ColorFallbackKind) -> Self {
    match self {
      Filter::DropShadow(shadow) => Filter::DropShadow(shadow.get_fallback(kind)),
      _ => self.clone()
    }
  }
}

/// https://drafts.fxtf.org/filter-effects-1/#funcdef-filter-drop-shadow
#[derive(Debug, Clone, PartialEq)]
pub struct DropShadow {
  pub color: CssColor,
  pub x_offset: Length,
  pub y_offset: Length,
  pub blur: Length
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

      break
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(DropShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2
    })
  }
}

impl ToCss for DropShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

#[derive(Debug, Clone, PartialEq)]
pub enum FilterList<'i> {
  None,
  Filters(SmallVec<[Filter<'i>; 1]>)
}

impl<'i> Parse<'i> for FilterList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(FilterList::None)
    }

    let mut filters = SmallVec::new();
    while let Ok(filter) = input.try_parse(Filter::parse) {
      filters.push(filter);
    }

    Ok(FilterList::Filters(filters))
  }
}

impl<'i> ToCss for FilterList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

impl<'i> FilterList<'i> {
  fn get_necessary_fallbacks(&self, targets: Browsers) -> ColorFallbackKind {
    let mut fallbacks = ColorFallbackKind::empty();
    if let FilterList::Filters(filters) = self {
      for shadow in filters {
        if let Filter::DropShadow(shadow) = &shadow {
          fallbacks |= shadow.color.get_necessary_fallbacks(targets);
        }
      }
    }
    fallbacks
  }
  
  fn get_fallback(&self, kind: ColorFallbackKind) -> Self {
    match self {
      FilterList::None => FilterList::None,
      FilterList::Filters(filters) => {
        FilterList::Filters(
          filters
            .iter()
            .map(|filter| filter.get_fallback(kind))
            .collect()
        )
      }
    }
  }
}

#[derive(Default)]
pub(crate) struct FilterHandler {
  targets: Option<Browsers>
}

impl FilterHandler {
  pub fn new(targets: Option<Browsers>) -> FilterHandler {
    FilterHandler {
      targets
    }
  }
}

impl<'i> PropertyHandler<'i> for FilterHandler {
  fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, _: &mut LogicalProperties) -> bool {
    if let (Property::Filter(filters), Some(targets)) = (property, self.targets) {
      let fallbacks = filters.get_necessary_fallbacks(targets);

      if fallbacks.contains(ColorFallbackKind::RGB) {
        dest.push(Property::Filter(filters.get_fallback(ColorFallbackKind::RGB)));
      }

      if fallbacks.contains(ColorFallbackKind::P3) {
        dest.push(Property::Filter(filters.get_fallback(ColorFallbackKind::P3)));
      }

      if fallbacks.contains(ColorFallbackKind::LAB) {
        dest.push(Property::Filter(filters.get_fallback(ColorFallbackKind::LAB)));
      } else {
        dest.push(Property::Filter(filters.clone()));
      }

      true
    } else {
      false
    }
  }

  fn finalize(&mut self, _: &mut DeclarationList, _: &mut LogicalProperties) {}
}
