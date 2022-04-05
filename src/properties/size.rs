use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::traits::{Parse, PropertyHandler, ToCss};
use crate::values::length::LengthPercentage;
use cssparser::*;

/// https://drafts.csswg.org/css-sizing-3/#specifying-sizes

/// https://drafts.csswg.org/css-sizing-3/#preferred-size-properties
#[derive(Debug, Clone, PartialEq)]
pub enum Size {
  Auto,
  LengthPercentage(LengthPercentage),
  MinContent,
  MaxContent,
  FitContent(LengthPercentage),
}

impl<'i> Parse<'i> for Size {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("auto")).is_ok() {
      return Ok(Size::Auto);
    }

    if input.try_parse(|i| i.expect_ident_matching("min-content")).is_ok() {
      return Ok(Size::MinContent);
    }

    if input.try_parse(|i| i.expect_ident_matching("max-content")).is_ok() {
      return Ok(Size::MaxContent);
    }

    if let Ok(l) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(Size::LengthPercentage(l));
    }

    if let Ok(l) = parse_fit_content(input) {
      return Ok(Size::FitContent(l));
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for Size {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use Size::*;
    match self {
      Auto => dest.write_str("auto"),
      MinContent => dest.write_str("min-content"),
      MaxContent => dest.write_str("max-content"),
      FitContent(l) => {
        dest.write_str("fit-content(")?;
        l.to_css(dest)?;
        dest.write_str(")")
      }
      LengthPercentage(l) => l.to_css(dest),
    }
  }
}

/// https://drafts.csswg.org/css-sizing-3/#min-size-properties
/// https://drafts.csswg.org/css-sizing-3/#max-size-properties
#[derive(Debug, Clone, PartialEq)]
pub enum MinMaxSize {
  None,
  LengthPercentage(LengthPercentage),
  MinContent,
  MaxContent,
  FitContent(LengthPercentage),
}

impl<'i> Parse<'i> for MinMaxSize {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("none")).is_ok() {
      return Ok(MinMaxSize::None);
    }

    if input.try_parse(|i| i.expect_ident_matching("min-content")).is_ok() {
      return Ok(MinMaxSize::MinContent);
    }

    if input.try_parse(|i| i.expect_ident_matching("max-content")).is_ok() {
      return Ok(MinMaxSize::MaxContent);
    }

    if let Ok(percent) = input.try_parse(|input| LengthPercentage::parse(input)) {
      return Ok(MinMaxSize::LengthPercentage(percent));
    }

    if let Ok(l) = parse_fit_content(input) {
      return Ok(MinMaxSize::FitContent(l));
    }

    Err(input.new_error_for_next_token())
  }
}

impl ToCss for MinMaxSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use MinMaxSize::*;
    match self {
      None => dest.write_str("none"),
      MinContent => dest.write_str("min-content"),
      MaxContent => dest.write_str("max-content"),
      FitContent(l) => {
        dest.write_str("fit-content(")?;
        l.to_css(dest)?;
        dest.write_str(")")
      }
      LengthPercentage(l) => l.to_css(dest),
    }
  }
}

fn parse_fit_content<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<LengthPercentage, ParseError<'i, ParserError<'i>>> {
  input.expect_function_matching("fit-content")?;
  input.parse_nested_block(|input| LengthPercentage::parse(input))
}

enum_property! {
  /// https://drafts.csswg.org/css-sizing-3/#box-sizing
  pub enum BoxSizing {
    "content-box": ContentBox,
    "border-box": BorderBox,
  }
}

#[derive(Default)]
pub(crate) struct SizeHandler;

impl<'i> PropertyHandler<'i> for SizeHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i>,
  ) -> bool {
    let logical_supported = context.is_supported(Feature::LogicalSize);

    macro_rules! logical {
      ($prop: ident, $val: ident, $physical: ident) => {
        if logical_supported {
          dest.push(Property::$prop($val.clone()));
        } else {
          dest.push(Property::$physical($val.clone()));
        }
      };
    }

    match property {
      Property::Width(_)
      | Property::Height(_)
      | Property::MinWidth(_)
      | Property::MaxWidth(_)
      | Property::MinHeight(_)
      | Property::MaxHeight(_) => {
        dest.push(property.clone());
      }
      Property::BlockSize(size) => logical!(BlockSize, size, Height),
      Property::MinBlockSize(size) => logical!(MinBlockSize, size, MinHeight),
      Property::MaxBlockSize(size) => logical!(MaxBlockSize, size, MaxHeight),
      Property::InlineSize(size) => logical!(InlineSize, size, Width),
      Property::MinInlineSize(size) => logical!(MinInlineSize, size, MinWidth),
      Property::MaxInlineSize(size) => logical!(MaxInlineSize, size, MaxWidth),
      Property::Unparsed(unparsed) => {
        macro_rules! logical_unparsed {
          ($physical: ident) => {
            if logical_supported {
              dest.push(property.clone());
            } else {
              dest.push(Property::Unparsed(
                unparsed.with_property_id(PropertyId::$physical),
              ));
            }
          };
        }

        match &unparsed.property_id {
          PropertyId::Width
          | PropertyId::Height
          | PropertyId::MinWidth
          | PropertyId::MaxWidth
          | PropertyId::MinHeight
          | PropertyId::MaxHeight => {
            dest.push(property.clone());
          }
          PropertyId::BlockSize => logical_unparsed!(Height),
          PropertyId::MinBlockSize => logical_unparsed!(MinHeight),
          PropertyId::MaxBlockSize => logical_unparsed!(MaxHeight),
          PropertyId::InlineSize => logical_unparsed!(Width),
          PropertyId::MinInlineSize => logical_unparsed!(MinWidth),
          PropertyId::MaxInlineSize => logical_unparsed!(MaxWidth),
          _ => return false,
        }
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, _: &mut DeclarationList, _: &mut PropertyHandlerContext<'i>) {}
}
