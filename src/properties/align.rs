//! CSS properties related to box alignment.

use super::flex::{BoxAlign, BoxPack, FlexAlign, FlexItemAlign, FlexLinePack, FlexPack};
use super::{Property, PropertyId};
use crate::compat;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::*;
use crate::prefixes::{is_flex_2009, Feature};
use crate::printer::Printer;
use crate::traits::{FromStandard, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::length::LengthPercentage;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

#[cfg(feature = "serde")]
use crate::serialization::ValueWrapper;

/// A [`<baseline-position>`](https://www.w3.org/TR/css-align-3/#typedef-baseline-position) value,
/// as used in the alignment properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum BaselinePosition {
  /// The first baseline.
  First,
  /// The last baseline.
  Last,
}

impl<'i> Parse<'i> for BaselinePosition {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "baseline" => Ok(BaselinePosition::First),
      "first" => {
        input.expect_ident_matching("baseline")?;
        Ok(BaselinePosition::First)
      },
      "last" => {
        input.expect_ident_matching("baseline")?;
        Ok(BaselinePosition::Last)
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for BaselinePosition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      BaselinePosition::First => dest.write_str("baseline"),
      BaselinePosition::Last => dest.write_str("last baseline"),
    }
  }
}

enum_property! {
  /// A [`<content-distribution>`](https://www.w3.org/TR/css-align-3/#typedef-content-distribution) value.
  pub enum ContentDistribution {
    /// Items are spaced evenly, with the first and last items against the edge of the container.
    "space-between": SpaceBetween,
    /// Items are spaced evenly, with half-size spaces at the start and end.
    "space-around": SpaceAround,
    /// Items are spaced evenly, with full-size spaces at the start and end.
    "space-evenly": SpaceEvenly,
    /// Items are stretched evenly to fill free space.
    "stretch": Stretch,
  }
}

enum_property! {
  /// An [`<overflow-position>`](https://www.w3.org/TR/css-align-3/#typedef-overflow-position) value.
  pub enum OverflowPosition {
    /// If the size of the alignment subject overflows the alignment container,
    /// the alignment subject is instead aligned as if the alignment mode were start.
    Safe,
    /// Regardless of the relative sizes of the alignment subject and alignment
    /// container, the given alignment value is honored.
    Unsafe,
  }
}

enum_property! {
  /// A [`<content-position>`](https://www.w3.org/TR/css-align-3/#typedef-content-position) value.
  pub enum ContentPosition {
    /// Content is centered within the container.
    "center": Center,
    /// Content is aligned to the start of the container.
    "start": Start,
    /// Content is aligned to the end of the container.
    "end": End,
    /// Same as `start` when within a flexbox container.
    "flex-start": FlexStart,
    /// Same as `end` when within a flexbox container.
    "flex-end": FlexEnd,
  }
}

/// A value for the [align-content](https://www.w3.org/TR/css-align-3/#propdef-align-content) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AlignContent {
  /// Default alignment.
  Normal,
  /// A baseline position.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<BaselinePosition>"))]
  BaselinePosition(BaselinePosition),
  /// A content distribution keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<ContentDistribution>"))]
  ContentDistribution(ContentDistribution),
  /// A content position keyword.
  ContentPosition {
    /// A content position keyword.
    value: ContentPosition,
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
}

impl<'i> Parse<'i> for AlignContent {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(AlignContent::Normal);
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(AlignContent::BaselinePosition(val));
    }

    if let Ok(val) = input.try_parse(ContentDistribution::parse) {
      return Ok(AlignContent::ContentDistribution(val));
    }

    let overflow = input.try_parse(OverflowPosition::parse).ok();
    let value = ContentPosition::parse(input)?;
    Ok(AlignContent::ContentPosition { overflow, value })
  }
}

impl ToCss for AlignContent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      AlignContent::Normal => dest.write_str("normal"),
      AlignContent::BaselinePosition(val) => val.to_css(dest),
      AlignContent::ContentDistribution(val) => val.to_css(dest),
      AlignContent::ContentPosition { overflow, value } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        value.to_css(dest)
      }
    }
  }
}

/// A value for the [justify-content](https://www.w3.org/TR/css-align-3/#propdef-justify-content) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum JustifyContent {
  /// Default justification.
  Normal,
  /// A content distribution keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<ContentDistribution>"))]
  ContentDistribution(ContentDistribution),
  /// A content position keyword.
  ContentPosition {
    /// A content position keyword.
    value: ContentPosition,
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// Justify to the left.
  Left {
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// Justify to the right.
  Right {
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
}

impl<'i> Parse<'i> for JustifyContent {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(JustifyContent::Normal);
    }

    if let Ok(val) = input.try_parse(ContentDistribution::parse) {
      return Ok(JustifyContent::ContentDistribution(val));
    }

    let overflow = input.try_parse(OverflowPosition::parse).ok();
    if let Ok(content_position) = input.try_parse(ContentPosition::parse) {
      return Ok(JustifyContent::ContentPosition {
        overflow,
        value: content_position,
      });
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "left" => Ok(JustifyContent::Left { overflow }),
      "right" => Ok(JustifyContent::Right { overflow }),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for JustifyContent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      JustifyContent::Normal => dest.write_str("normal"),
      JustifyContent::ContentDistribution(value) => value.to_css(dest),
      JustifyContent::ContentPosition { overflow, value } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        value.to_css(dest)
      }
      JustifyContent::Left { overflow } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("left")
      }
      JustifyContent::Right { overflow } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("right")
      }
    }
  }
}

define_shorthand! {
  /// A value for the [place-content](https://www.w3.org/TR/css-align-3/#place-content) shorthand property.
  pub struct PlaceContent {
    /// The content alignment.
    align: AlignContent(AlignContent, VendorPrefix),
    /// The content justification.
    justify: JustifyContent(JustifyContent, VendorPrefix),
  }
}

impl<'i> Parse<'i> for PlaceContent {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let align = AlignContent::parse(input)?;
    let justify = match input.try_parse(JustifyContent::parse) {
      Ok(j) => j,
      Err(_) => {
        // The second value is assigned to justify-content; if omitted, it is copied
        // from the first value, unless that value is a <baseline-position> in which
        // case it is defaulted to start.
        match align {
          AlignContent::BaselinePosition(_) => JustifyContent::ContentPosition {
            overflow: None,
            value: ContentPosition::Start,
          },
          AlignContent::Normal => JustifyContent::Normal,
          AlignContent::ContentDistribution(value) => JustifyContent::ContentDistribution(value.clone()),
          AlignContent::ContentPosition { overflow, value } => JustifyContent::ContentPosition {
            overflow: overflow.clone(),
            value: value.clone(),
          },
        }
      }
    };

    Ok(PlaceContent { align, justify })
  }
}

impl ToCss for PlaceContent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.align.to_css(dest)?;
    let is_equal = match self.justify {
      JustifyContent::Normal if self.align == AlignContent::Normal => true,
      JustifyContent::ContentDistribution(d) if matches!(self.align, AlignContent::ContentDistribution(d2) if d == d2) => {
        true
      }
      JustifyContent::ContentPosition { overflow: o, value: c } if matches!(self.align, AlignContent::ContentPosition { overflow: o2, value: c2 } if o == o2 && c == c2) => {
        true
      }
      _ => false,
    };

    if !is_equal {
      dest.write_str(" ")?;
      self.justify.to_css(dest)?;
    }

    Ok(())
  }
}

enum_property! {
  /// A [`<self-position>`](https://www.w3.org/TR/css-align-3/#typedef-self-position) value.
  pub enum SelfPosition {
    /// Item is centered within the container.
    "center": Center,
    /// Item is aligned to the start of the container.
    "start": Start,
    /// Item is aligned to the end of the container.
    "end": End,
    /// Item is aligned to the edge of the container corresponding to the start side of the item.
    "self-start": SelfStart,
    /// Item is aligned to the edge of the container corresponding to the end side of the item.
    "self-end": SelfEnd,
    /// Item  is aligned to the start of the container, within flexbox layouts.
    "flex-start": FlexStart,
    /// Item  is aligned to the end of the container, within flexbox layouts.
    "flex-end": FlexEnd,
  }
}

/// A value for the [align-self](https://www.w3.org/TR/css-align-3/#align-self-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AlignSelf {
  /// Automatic alignment.
  Auto,
  /// Default alignment.
  Normal,
  /// Item is stretched.
  Stretch,
  /// A baseline position keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<BaselinePosition>"))]
  BaselinePosition(BaselinePosition),
  /// A self position keyword.
  SelfPosition {
    /// A self position keyword.
    value: SelfPosition,
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
}

impl<'i> Parse<'i> for AlignSelf {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(AlignSelf::Auto);
    }

    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(AlignSelf::Normal);
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(AlignSelf::Stretch);
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(AlignSelf::BaselinePosition(val));
    }

    let overflow = input.try_parse(OverflowPosition::parse).ok();
    let value = SelfPosition::parse(input)?;
    Ok(AlignSelf::SelfPosition { overflow, value })
  }
}

impl ToCss for AlignSelf {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      AlignSelf::Auto => dest.write_str("auto"),
      AlignSelf::Normal => dest.write_str("normal"),
      AlignSelf::Stretch => dest.write_str("stretch"),
      AlignSelf::BaselinePosition(val) => val.to_css(dest),
      AlignSelf::SelfPosition { overflow, value } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        value.to_css(dest)
      }
    }
  }
}

/// A value for the [justify-self](https://www.w3.org/TR/css-align-3/#justify-self-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum JustifySelf {
  /// Automatic justification.
  Auto,
  /// Default justification.
  Normal,
  /// Item is stretched.
  Stretch,
  /// A baseline position keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<BaselinePosition>"))]
  BaselinePosition(BaselinePosition),
  /// A self position keyword.
  SelfPosition {
    /// A self position keyword.
    value: SelfPosition,
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// Item is justified to the left.
  Left {
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// Item is justified to the right.
  Right {
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
}

impl<'i> Parse<'i> for JustifySelf {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(JustifySelf::Auto);
    }

    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(JustifySelf::Normal);
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(JustifySelf::Stretch);
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(JustifySelf::BaselinePosition(val));
    }

    let overflow = input.try_parse(OverflowPosition::parse).ok();
    if let Ok(value) = input.try_parse(SelfPosition::parse) {
      return Ok(JustifySelf::SelfPosition { overflow, value });
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "left" => Ok(JustifySelf::Left { overflow }),
      "right" => Ok(JustifySelf::Right { overflow }),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for JustifySelf {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      JustifySelf::Auto => dest.write_str("auto"),
      JustifySelf::Normal => dest.write_str("normal"),
      JustifySelf::Stretch => dest.write_str("stretch"),
      JustifySelf::BaselinePosition(val) => val.to_css(dest),
      JustifySelf::SelfPosition { overflow, value } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        value.to_css(dest)
      }
      JustifySelf::Left { overflow } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("left")
      }
      JustifySelf::Right { overflow } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("right")
      }
    }
  }
}

define_shorthand! {
  /// A value for the [place-self](https://www.w3.org/TR/css-align-3/#place-self-property) shorthand property.
  pub struct PlaceSelf {
    /// The item alignment.
    align: AlignSelf(AlignSelf, VendorPrefix),
    /// The item justification.
    justify: JustifySelf(JustifySelf),
  }
}

impl<'i> Parse<'i> for PlaceSelf {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let align = AlignSelf::parse(input)?;
    let justify = match input.try_parse(JustifySelf::parse) {
      Ok(j) => j,
      Err(_) => {
        // The second value is assigned to justify-self; if omitted, it is copied from the first value.
        match &align {
          AlignSelf::Auto => JustifySelf::Auto,
          AlignSelf::Normal => JustifySelf::Normal,
          AlignSelf::Stretch => JustifySelf::Stretch,
          AlignSelf::BaselinePosition(p) => JustifySelf::BaselinePosition(p.clone()),
          AlignSelf::SelfPosition { overflow, value } => JustifySelf::SelfPosition {
            overflow: overflow.clone(),
            value: value.clone(),
          },
        }
      }
    };

    Ok(PlaceSelf { align, justify })
  }
}

impl ToCss for PlaceSelf {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.align.to_css(dest)?;
    let is_equal = match &self.justify {
      JustifySelf::Auto => true,
      JustifySelf::Normal => self.align == AlignSelf::Normal,
      JustifySelf::Stretch => self.align == AlignSelf::Normal,
      JustifySelf::BaselinePosition(p) if matches!(&self.align, AlignSelf::BaselinePosition(p2) if p == p2) => {
        true
      }
      JustifySelf::SelfPosition { overflow: o, value: c } if matches!(&self.align, AlignSelf::SelfPosition  { overflow: o2, value: c2 } if o == o2 && c == c2) => {
        true
      }
      _ => false,
    };

    if !is_equal {
      dest.write_str(" ")?;
      self.justify.to_css(dest)?;
    }

    Ok(())
  }
}

/// A value for the [align-items](https://www.w3.org/TR/css-align-3/#align-items-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AlignItems {
  /// Default alignment.
  Normal,
  /// Items are stretched.
  Stretch,
  /// A baseline position keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<BaselinePosition>"))]
  BaselinePosition(BaselinePosition),
  /// A self position keyword.
  SelfPosition {
    /// A self position keyword.
    value: SelfPosition,
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
}

impl<'i> Parse<'i> for AlignItems {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(AlignItems::Normal);
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(AlignItems::Stretch);
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(AlignItems::BaselinePosition(val));
    }

    let overflow = input.try_parse(OverflowPosition::parse).ok();
    let value = SelfPosition::parse(input)?;
    Ok(AlignItems::SelfPosition { overflow, value })
  }
}

impl ToCss for AlignItems {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      AlignItems::Normal => dest.write_str("normal"),
      AlignItems::Stretch => dest.write_str("stretch"),
      AlignItems::BaselinePosition(val) => val.to_css(dest),
      AlignItems::SelfPosition { overflow, value } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        value.to_css(dest)
      }
    }
  }
}

/// A legacy justification keyword, as used in the `justify-items` property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum LegacyJustify {
  /// Left justify.
  Left,
  /// Right justify.
  Right,
  /// Centered.
  Center,
}

impl<'i> Parse<'i> for LegacyJustify {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "legacy" => {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        match_ignore_ascii_case! { &*ident,
          "left" => Ok(LegacyJustify::Left),
          "right" => Ok(LegacyJustify::Right),
          "center" => Ok(LegacyJustify::Center),
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        }
      },
      "left" => {
        input.expect_ident_matching("legacy")?;
        Ok(LegacyJustify::Left)
      },
      "right" => {
        input.expect_ident_matching("legacy")?;
        Ok(LegacyJustify::Right)
      },
      "center" => {
        input.expect_ident_matching("legacy")?;
        Ok(LegacyJustify::Center)
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for LegacyJustify {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_str("legacy ")?;
    match self {
      LegacyJustify::Left => dest.write_str("left"),
      LegacyJustify::Right => dest.write_str("right"),
      LegacyJustify::Center => dest.write_str("center"),
    }
  }
}

/// A value for the [justify-items](https://www.w3.org/TR/css-align-3/#justify-items-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum JustifyItems {
  /// Default justification.
  Normal,
  /// Items are stretched.
  Stretch,
  /// A baseline position keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<BaselinePosition>"))]
  BaselinePosition(BaselinePosition),
  /// A self position keyword, with optional overflow position.
  SelfPosition {
    /// A self position keyword.
    value: SelfPosition,
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// Items are justified to the left, with an optional overflow position.
  Left {
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// Items are justified to the right, with an optional overflow position.
  Right {
    /// An overflow alignment mode.
    overflow: Option<OverflowPosition>,
  },
  /// A legacy justification keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LegacyJustify>"))]
  Legacy(LegacyJustify),
}

impl<'i> Parse<'i> for JustifyItems {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(JustifyItems::Normal);
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(JustifyItems::Stretch);
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(JustifyItems::BaselinePosition(val));
    }

    if let Ok(val) = input.try_parse(LegacyJustify::parse) {
      return Ok(JustifyItems::Legacy(val));
    }

    let overflow = input.try_parse(OverflowPosition::parse).ok();
    if let Ok(value) = input.try_parse(SelfPosition::parse) {
      return Ok(JustifyItems::SelfPosition { overflow, value });
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "left" => Ok(JustifyItems::Left { overflow }),
      "right" => Ok(JustifyItems::Right { overflow }),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for JustifyItems {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      JustifyItems::Normal => dest.write_str("normal"),
      JustifyItems::Stretch => dest.write_str("stretch"),
      JustifyItems::BaselinePosition(val) => val.to_css(dest),
      JustifyItems::Legacy(val) => val.to_css(dest),
      JustifyItems::SelfPosition { overflow, value } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        value.to_css(dest)
      }
      JustifyItems::Left { overflow } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("left")
      }
      JustifyItems::Right { overflow } => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("right")
      }
    }
  }
}

define_shorthand! {
  /// A value for the [place-items](https://www.w3.org/TR/css-align-3/#place-items-property) shorthand property.
  pub struct PlaceItems {
    /// The item alignment.
    align: AlignItems(AlignItems, VendorPrefix),
    /// The item justification.
    justify: JustifyItems(JustifyItems),
  }
}

impl<'i> Parse<'i> for PlaceItems {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let align = AlignItems::parse(input)?;
    let justify = match input.try_parse(JustifyItems::parse) {
      Ok(j) => j,
      Err(_) => {
        // The second value is assigned to justify-items; if omitted, it is copied from the first value.
        match &align {
          AlignItems::Normal => JustifyItems::Normal,
          AlignItems::Stretch => JustifyItems::Stretch,
          AlignItems::BaselinePosition(p) => JustifyItems::BaselinePosition(p.clone()),
          AlignItems::SelfPosition { overflow, value } => JustifyItems::SelfPosition {
            overflow: overflow.clone(),
            value: value.clone(),
          },
        }
      }
    };

    Ok(PlaceItems { align, justify })
  }
}

impl ToCss for PlaceItems {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.align.to_css(dest)?;
    let is_equal = match &self.justify {
      JustifyItems::Normal => self.align == AlignItems::Normal,
      JustifyItems::Stretch => self.align == AlignItems::Normal,
      JustifyItems::BaselinePosition(p) if matches!(&self.align, AlignItems::BaselinePosition(p2) if p == p2) => {
        true
      }
      JustifyItems::SelfPosition { overflow: o, value: c } if matches!(&self.align, AlignItems::SelfPosition { overflow: o2, value: c2 } if o == o2 && c == c2) => {
        true
      }
      _ => false,
    };

    if !is_equal {
      dest.write_str(" ")?;
      self.justify.to_css(dest)?;
    }

    Ok(())
  }
}

/// A [gap](https://www.w3.org/TR/css-align-3/#column-row-gap) value, as used in the
/// `column-gap` and `row-gap` properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum GapValue {
  /// Equal to `1em` for multi-column containers, and zero otherwise.
  Normal,
  /// An explicit length.
  LengthPercentage(LengthPercentage),
}

impl<'i> Parse<'i> for GapValue {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(GapValue::Normal);
    }

    let val = LengthPercentage::parse(input)?;
    Ok(GapValue::LengthPercentage(val))
  }
}

impl ToCss for GapValue {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      GapValue::Normal => dest.write_str("normal"),
      GapValue::LengthPercentage(lp) => lp.to_css(dest),
    }
  }
}

define_shorthand! {
  /// A value for the [gap](https://www.w3.org/TR/css-align-3/#gap-shorthand) shorthand property.
  pub struct Gap {
    /// The row gap.
    row: RowGap(GapValue),
    /// The column gap.
    column: ColumnGap(GapValue),
  }
}

impl<'i> Parse<'i> for Gap {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let row = GapValue::parse(input)?;
    let column = input.try_parse(GapValue::parse).unwrap_or(row.clone());
    Ok(Gap { row, column })
  }
}

impl ToCss for Gap {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.row.to_css(dest)?;
    if self.column != self.row {
      dest.write_str(" ")?;
      self.column.to_css(dest)?;
    }
    Ok(())
  }
}

#[derive(Default, Debug)]
pub(crate) struct AlignHandler {
  align_content: Option<(AlignContent, VendorPrefix)>,
  flex_line_pack: Option<(FlexLinePack, VendorPrefix)>,
  justify_content: Option<(JustifyContent, VendorPrefix)>,
  box_pack: Option<(BoxPack, VendorPrefix)>,
  flex_pack: Option<(FlexPack, VendorPrefix)>,
  align_self: Option<(AlignSelf, VendorPrefix)>,
  flex_item_align: Option<(FlexItemAlign, VendorPrefix)>,
  justify_self: Option<JustifySelf>,
  align_items: Option<(AlignItems, VendorPrefix)>,
  box_align: Option<(BoxAlign, VendorPrefix)>,
  flex_align: Option<(FlexAlign, VendorPrefix)>,
  justify_items: Option<JustifyItems>,
  row_gap: Option<GapValue>,
  column_gap: Option<GapValue>,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for AlignHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest, context);
          }
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: expr) => {{
        maybe_flush!($prop, $val, $vp);

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
        } else {
          self.$prop = Some(($val.clone(), *$vp));
          self.has_any = true;
        }
      }};
    }

    match property {
      AlignContent(val, vp) => {
        self.flex_line_pack = None;
        property!(align_content, val, vp);
      }
      FlexLinePack(val, vp) => property!(flex_line_pack, val, vp),
      JustifyContent(val, vp) => {
        self.box_pack = None;
        self.flex_pack = None;
        property!(justify_content, val, vp);
      }
      BoxPack(val, vp) => property!(box_pack, val, vp),
      FlexPack(val, vp) => property!(flex_pack, val, vp),
      PlaceContent(val) => {
        self.flex_line_pack = None;
        self.box_pack = None;
        self.flex_pack = None;
        maybe_flush!(align_content, &val.align, &VendorPrefix::None);
        maybe_flush!(justify_content, &val.justify, &VendorPrefix::None);
        property!(align_content, &val.align, &VendorPrefix::None);
        property!(justify_content, &val.justify, &VendorPrefix::None);
      }
      AlignSelf(val, vp) => {
        self.flex_item_align = None;
        property!(align_self, val, vp);
      }
      FlexItemAlign(val, vp) => property!(flex_item_align, val, vp),
      JustifySelf(val) => {
        self.justify_self = Some(val.clone());
        self.has_any = true;
      }
      PlaceSelf(val) => {
        self.flex_item_align = None;
        property!(align_self, &val.align, &VendorPrefix::None);
        self.justify_self = Some(val.justify.clone());
      }
      AlignItems(val, vp) => {
        self.box_align = None;
        self.flex_align = None;
        property!(align_items, val, vp);
      }
      BoxAlign(val, vp) => property!(box_align, val, vp),
      FlexAlign(val, vp) => property!(flex_align, val, vp),
      JustifyItems(val) => {
        self.justify_items = Some(val.clone());
        self.has_any = true;
      }
      PlaceItems(val) => {
        self.box_align = None;
        self.flex_align = None;
        property!(align_items, &val.align, &VendorPrefix::None);
        self.justify_items = Some(val.justify.clone());
      }
      RowGap(val) => {
        self.row_gap = Some(val.clone());
        self.has_any = true;
      }
      ColumnGap(val) => {
        self.column_gap = Some(val.clone());
        self.has_any = true;
      }
      Gap(val) => {
        self.row_gap = Some(val.row.clone());
        self.column_gap = Some(val.column.clone());
        self.has_any = true;
      }
      Unparsed(val) if is_align_property(&val.property_id) => {
        self.flush(dest, context);
        dest.push(property.clone()) // TODO: prefix?
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
  }
}

impl AlignHandler {
  fn flush<'i>(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    let mut align_content = std::mem::take(&mut self.align_content);
    let mut justify_content = std::mem::take(&mut self.justify_content);
    let mut align_self = std::mem::take(&mut self.align_self);
    let mut justify_self = std::mem::take(&mut self.justify_self);
    let mut align_items = std::mem::take(&mut self.align_items);
    let mut justify_items = std::mem::take(&mut self.justify_items);
    let row_gap = std::mem::take(&mut self.row_gap);
    let column_gap = std::mem::take(&mut self.column_gap);
    let box_align = std::mem::take(&mut self.box_align);
    let box_pack = std::mem::take(&mut self.box_pack);
    let flex_line_pack = std::mem::take(&mut self.flex_line_pack);
    let flex_pack = std::mem::take(&mut self.flex_pack);
    let flex_align = std::mem::take(&mut self.flex_align);
    let flex_item_align = std::mem::take(&mut self.flex_item_align);

    // Gets prefixes for standard properties.
    macro_rules! prefixes {
      ($prop: ident) => {{
        let mut prefix = context.targets.prefixes(VendorPrefix::None, Feature::$prop);
        // Firefox only implemented the 2009 spec prefixed.
        // Microsoft only implemented the 2012 spec prefixed.
        prefix.remove(VendorPrefix::Moz | VendorPrefix::Ms);
        prefix
      }};
    }

    macro_rules! standard_property {
      ($prop: ident, $key: ident) => {
        if let Some((val, prefix)) = $key {
          // If we have an unprefixed property, override necessary prefixes.
          let prefix = if prefix.contains(VendorPrefix::None) {
            prefixes!($prop)
          } else {
            prefix
          };
          dest.push(Property::$prop(val, prefix))
        }
      };
    }

    macro_rules! legacy_property {
      ($prop: ident, $key: ident, $( $prop_2009: ident )?, $prop_2012: ident) => {
        if let Some((val, prefix)) = &$key {
          // If we have an unprefixed standard property, generate legacy prefixed versions.
          let mut prefix = context.targets.prefixes(*prefix, Feature::$prop);

          if prefix.contains(VendorPrefix::None) {
            $(
              // 2009 spec, implemented by webkit and firefox.
              if let Some(targets) = context.targets.browsers {
                let mut prefixes_2009 = VendorPrefix::empty();
                if is_flex_2009(targets) {
                  prefixes_2009 |= VendorPrefix::WebKit;
                }
                if prefix.contains(VendorPrefix::Moz) {
                  prefixes_2009 |= VendorPrefix::Moz;
                }
                if !prefixes_2009.is_empty() {
                  if let Some(v) = $prop_2009::from_standard(&val) {
                    dest.push(Property::$prop_2009(v, prefixes_2009));
                  }
                }
              }
            )?
          }

          // 2012 spec, implemented by microsoft.
          if prefix.contains(VendorPrefix::Ms) {
            if let Some(v) = $prop_2012::from_standard(&val) {
              dest.push(Property::$prop_2012(v, VendorPrefix::Ms));
            }
          }

          // Remove Firefox and IE from standard prefixes.
          prefix.remove(VendorPrefix::Moz | VendorPrefix::Ms);
        }
      };
    }

    macro_rules! prefixed_property {
      ($prop: ident, $key: expr) => {
        if let Some((val, prefix)) = $key {
          dest.push(Property::$prop(val, prefix))
        }
      };
    }

    macro_rules! unprefixed_property {
      ($prop: ident, $key: expr) => {
        if let Some(val) = $key {
          dest.push(Property::$prop(val))
        }
      };
    }

    macro_rules! shorthand {
      ($prop: ident, $align_prop: ident, $align: ident, $justify: ident $(, $justify_prop: ident )?) => {
        if let (Some((align, align_prefix)), Some(justify)) = (&mut $align, &mut $justify) {
          let intersection = *align_prefix $( & {
            // Hack for conditional compilation. Have to use a variable.
            #[allow(non_snake_case)]
            let $justify_prop = justify.1;
            $justify_prop
          })?;

          // Only use shorthand if unprefixed.
          if intersection.contains(VendorPrefix::None) {
            // Add prefixed longhands if needed.
            *align_prefix = prefixes!($align_prop);
            align_prefix.remove(VendorPrefix::None);
            if !align_prefix.is_empty() {
              dest.push(Property::$align_prop(align.clone(), *align_prefix))
            }

            $(
              let (justify, justify_prefix) = justify;
              *justify_prefix = prefixes!($justify_prop);
              justify_prefix.remove(VendorPrefix::None);

              if !justify_prefix.is_empty() {
                dest.push(Property::$justify_prop(justify.clone(), *justify_prefix))
              }
            )?

            // Add shorthand.
            dest.push(Property::$prop($prop {
              align: align.clone(),
              justify: justify.clone()
            }));

            $align = None;
            $justify = None;
          }
        }
      };
    }

    // 2009 properties
    prefixed_property!(BoxAlign, box_align);
    prefixed_property!(BoxPack, box_pack);

    // 2012 properties
    prefixed_property!(FlexPack, flex_pack);
    prefixed_property!(FlexAlign, flex_align);
    prefixed_property!(FlexItemAlign, flex_item_align);
    prefixed_property!(FlexLinePack, flex_line_pack);

    legacy_property!(AlignContent, align_content, , FlexLinePack);
    legacy_property!(JustifyContent, justify_content, BoxPack, FlexPack);
    if context.targets.is_compatible(compat::Feature::PlaceContent) {
      shorthand!(
        PlaceContent,
        AlignContent,
        align_content,
        justify_content,
        JustifyContent
      );
    }
    standard_property!(AlignContent, align_content);
    standard_property!(JustifyContent, justify_content);

    legacy_property!(AlignSelf, align_self, , FlexItemAlign);
    if context.targets.is_compatible(compat::Feature::PlaceSelf) {
      shorthand!(PlaceSelf, AlignSelf, align_self, justify_self);
    }
    standard_property!(AlignSelf, align_self);
    unprefixed_property!(JustifySelf, justify_self);

    legacy_property!(AlignItems, align_items, BoxAlign, FlexAlign);
    if context.targets.is_compatible(compat::Feature::PlaceItems) {
      shorthand!(PlaceItems, AlignItems, align_items, justify_items);
    }
    standard_property!(AlignItems, align_items);
    unprefixed_property!(JustifyItems, justify_items);

    if row_gap.is_some() && column_gap.is_some() {
      dest.push(Property::Gap(Gap {
        row: row_gap.unwrap(),
        column: column_gap.unwrap(),
      }))
    } else {
      if let Some(gap) = row_gap {
        dest.push(Property::RowGap(gap))
      }

      if let Some(gap) = column_gap {
        dest.push(Property::ColumnGap(gap))
      }
    }
  }
}

#[inline]
fn is_align_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::AlignContent(_)
    | PropertyId::FlexLinePack(_)
    | PropertyId::JustifyContent(_)
    | PropertyId::BoxPack(_)
    | PropertyId::FlexPack(_)
    | PropertyId::PlaceContent
    | PropertyId::AlignSelf(_)
    | PropertyId::FlexItemAlign(_)
    | PropertyId::JustifySelf
    | PropertyId::PlaceSelf
    | PropertyId::AlignItems(_)
    | PropertyId::BoxAlign(_)
    | PropertyId::FlexAlign(_)
    | PropertyId::JustifyItems
    | PropertyId::PlaceItems
    | PropertyId::RowGap
    | PropertyId::ColumnGap
    | PropertyId::Gap => true,
    _ => false,
  }
}
