use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::Property;
use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;
use crate::prefixes::Feature;
use crate::declaration::DeclarationList;
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};

/// https://www.w3.org/TR/2020/WD-css-position-3-20200519/#position-property
#[derive(Debug, Clone, PartialEq)]
pub enum Position {
  Static,
  Relative,
  Absolute,
  Sticky(VendorPrefix),
  Fixed
}

impl Parse for Position {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

#[derive(Default)]
pub(crate) struct PositionHandler {
  targets: Option<Browsers>,
  position: Option<Position>
}

impl PositionHandler {
  pub fn new(targets: Option<Browsers>) -> PositionHandler {
    PositionHandler {
      targets,
      ..PositionHandler::default()
    }
  }
}

impl PropertyHandler for PositionHandler {
  fn handle_property(&mut self, property: &Property, _: &mut DeclarationList) -> bool {
    if let Property::Position(position) = property {
      if let (Some(Position::Sticky(cur)), Position::Sticky(new)) = (&mut self.position, position) {
        *cur |= *new;
      } else {
        self.position = Some(position.clone());
      }

      return true
    }

    false
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    if self.position.is_none() {
      return
    }
    
    if let Some(position) = std::mem::take(&mut self.position) {
      match position {
        Position::Sticky(mut prefix) => {
          if prefix.contains(VendorPrefix::None) {
            if let Some(targets) = self.targets {
              prefix = Feature::Sticky.prefixes_for(targets)
            }
          }

          if prefix.contains(VendorPrefix::WebKit) {
            dest.push(Property::Position(Position::Sticky(VendorPrefix::WebKit)))
          }

          if prefix.contains(VendorPrefix::None) {
            dest.push(Property::Position(Position::Sticky(VendorPrefix::None)))
          }
        }
        _ => {
          dest.push(Property::Position(position))
        }
      }
    }
  }
}
