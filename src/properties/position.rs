//! CSS properties related to positioning.

use super::Property;
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::traits::{Parse, PropertyHandler, ToCss};
use crate::values::number::CSSInteger;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A value for the [position](https://www.w3.org/TR/css-position-3/#position-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Position {
  /// The box is laid in the document flow.
  Static,
  /// The box is laid out in the document flow and offset from the resulting position.
  Relative,
  /// The box is taken out of document flow and positioned in reference to its relative ancestor.
  Absolute,
  /// Similar to relative but adjusted according to the ancestor scrollable element.
  Sticky(VendorPrefix),
  /// The box is taken out of the document flow and positioned in reference to the page viewport.
  Fixed,
}

impl<'i> Parse<'i> for Position {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "static" => Ok(Position::Static),
      "relative" => Ok(Position::Relative),
      "absolute" => Ok(Position::Absolute),
      "fixed" => Ok(Position::Fixed),
      "sticky" => Ok(Position::Sticky(VendorPrefix::None)),
      "-webkit-sticky" => Ok(Position::Sticky(VendorPrefix::WebKit)),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for Position {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Position::Static => dest.write_str("static"),
      Position::Relative => dest.write_str("relative"),
      Position::Absolute => dest.write_str("absolute"),
      Position::Fixed => dest.write_str("fixed"),
      Position::Sticky(prefix) => {
        prefix.to_css(dest)?;
        dest.write_str("sticky")
      }
    }
  }
}

/// A value for the [z-index](https://drafts.csswg.org/css2/#z-index) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ZIndex {
  /// The `auto` keyword.
  Auto,
  /// An integer value.
  Integer(CSSInteger),
}

impl<'i> Parse<'i> for ZIndex {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(value) = input.expect_integer() {
      return Ok(ZIndex::Integer(value));
    }

    input.expect_ident_matching("auto")?;
    Ok(ZIndex::Auto)
  }
}

impl ToCss for ZIndex {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      ZIndex::Auto => dest.write_str("auto"),
      ZIndex::Integer(value) => value.to_css(dest),
    }
  }
}

#[derive(Default)]
pub(crate) struct PositionHandler {
  position: Option<Position>,
}

impl<'i> PropertyHandler<'i> for PositionHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    _: &mut DeclarationList<'i>,
    _: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    if let Property::Position(position) = property {
      if let (Some(Position::Sticky(cur)), Position::Sticky(new)) = (&mut self.position, position) {
        *cur |= *new;
      } else {
        self.position = Some(position.clone());
      }

      return true;
    }

    false
  }

  fn finalize(&mut self, dest: &mut DeclarationList, context: &mut PropertyHandlerContext<'i, '_>) {
    if self.position.is_none() {
      return;
    }

    if let Some(position) = std::mem::take(&mut self.position) {
      match position {
        Position::Sticky(mut prefix) => {
          prefix = context.targets.prefixes(prefix, Feature::Sticky);
          if prefix.contains(VendorPrefix::WebKit) {
            dest.push(Property::Position(Position::Sticky(VendorPrefix::WebKit)))
          }

          if prefix.contains(VendorPrefix::None) {
            dest.push(Property::Position(Position::Sticky(VendorPrefix::None)))
          }
        }
        _ => dest.push(Property::Position(position)),
      }
    }
  }
}
