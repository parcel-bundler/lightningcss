use cssparser::*;
use crate::macros::*;
use crate::values::length::LengthPercentage;
use crate::traits::{Parse, ToCss, PropertyHandler, FromStandard};
use super::{Property, PropertyId};
use crate::vendor_prefix::VendorPrefix;
use crate::declaration::DeclarationList;
use super::flex::{BoxAlign, FlexLinePack, BoxPack, FlexPack, FlexAlign, FlexItemAlign};
use crate::targets::Browsers;
use crate::prefixes::{Feature, is_flex_2009};
use crate::printer::Printer;
use crate::compat;

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#typedef-baseline-position
#[derive(Debug, Clone, PartialEq)]
pub enum BaselinePosition {
  First,
  Last
}

impl Parse for BaselinePosition {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      BaselinePosition::First => dest.write_str("baseline"),
      BaselinePosition::Last => dest.write_str("last baseline")
    }
  }
}

// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#typedef-content-distribution
enum_property!(ContentDistribution,
  ("space-between", SpaceBetween),
  ("space-around", SpaceAround),
  ("space-evenly", SpaceEvenly),
  ("stretch", Stretch)
);

// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#typedef-overflow-position
enum_property!(OverflowPosition,
  Safe,
  Unsafe
);

// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#typedef-content-position
enum_property!(ContentPosition,
  ("center", Center),
  ("start", Start),
  ("end", End),
  ("flex-start", FlexStart),
  ("flex-end", FlexEnd)
);

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#propdef-align-content
#[derive(Debug, Clone, PartialEq)]
pub enum AlignContent {
  Normal,
  BaselinePosition(BaselinePosition),
  ContentDistribution(ContentDistribution),
  ContentPosition(Option<OverflowPosition>, ContentPosition)
}

impl Parse for AlignContent {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(AlignContent::Normal)
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(AlignContent::BaselinePosition(val))
    }

    if let Ok(val) = input.try_parse(ContentDistribution::parse) {
      return Ok(AlignContent::ContentDistribution(val))
    }

    let overflow_position = input.try_parse(OverflowPosition::parse).ok();
    let content_position = ContentPosition::parse(input)?;
    Ok(AlignContent::ContentPosition(overflow_position, content_position))
  }
}

impl ToCss for AlignContent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      AlignContent::Normal => dest.write_str("normal"),
      AlignContent::BaselinePosition(val) => val.to_css(dest),
      AlignContent::ContentDistribution(val) => val.to_css(dest),
      AlignContent::ContentPosition(overflow, pos) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        pos.to_css(dest)
      }
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#propdef-justify-content
#[derive(Debug, Clone, PartialEq)]
pub enum JustifyContent {
  Normal,
  ContentDistribution(ContentDistribution),
  ContentPosition(Option<OverflowPosition>, ContentPosition),
  Left(Option<OverflowPosition>),
  Right(Option<OverflowPosition>)
}

impl Parse for JustifyContent {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(JustifyContent::Normal)
    }

    if let Ok(val) = input.try_parse(ContentDistribution::parse) {
      return Ok(JustifyContent::ContentDistribution(val))
    }

    let overflow_position = input.try_parse(OverflowPosition::parse).ok();
    if let Ok(content_position) = input.try_parse(ContentPosition::parse) {
      return Ok(JustifyContent::ContentPosition(overflow_position, content_position))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "left" => Ok(JustifyContent::Left(overflow_position)),
      "right" => Ok(JustifyContent::Right(overflow_position)),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for JustifyContent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      JustifyContent::Normal => dest.write_str("normal"),
      JustifyContent::ContentDistribution(val) => val.to_css(dest),
      JustifyContent::ContentPosition(overflow, pos) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        pos.to_css(dest)
      }
      JustifyContent::Left(overflow) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("left")
      }
      JustifyContent::Right(overflow) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("right")
      }
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#place-content
#[derive(Debug, Clone, PartialEq)]
pub struct PlaceContent {
  pub align: AlignContent,
  pub justify: JustifyContent
}

impl Parse for PlaceContent {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let align = AlignContent::parse(input)?;
    let justify = match input.try_parse(JustifyContent::parse) {
      Ok(j) => j,
      Err(_) => {
        // The second value is assigned to justify-content; if omitted, it is copied 
        // from the first value, unless that value is a <baseline-position> in which 
        // case it is defaulted to start.
        match align {
          AlignContent::BaselinePosition(_) => JustifyContent::ContentPosition(None, ContentPosition::Start),
          AlignContent::Normal => JustifyContent::Normal,
          AlignContent::ContentDistribution(c) => JustifyContent::ContentDistribution(c.clone()),
          AlignContent::ContentPosition(o, c) => JustifyContent::ContentPosition(o.clone(), c.clone())
        }
      }
    };

    Ok(PlaceContent { align, justify })
  }
}

impl ToCss for PlaceContent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.align.to_css(dest)?;
    let is_equal = match self.justify {
      JustifyContent::Normal if self.align == AlignContent::Normal => true,
      JustifyContent::ContentDistribution(d) if matches!(self.align, AlignContent::ContentDistribution(d2) if d == d2) => true,
      JustifyContent::ContentPosition(o, c) if matches!(self.align, AlignContent::ContentPosition(o2, c2) if o == o2 && c == c2) => true,
      _ => false
    };

    if !is_equal {
      dest.write_str(" ")?;
      self.justify.to_css(dest)?;
    }

    Ok(())
  }
}

// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#typedef-self-position
enum_property!(SelfPosition,
  ("center", Center),
  ("start", Start),
  ("end", End),
  ("self-start", SelfStart),
  ("self-end", SelfEnd),
  ("flex-start", FlexStart),
  ("flex-end", FlexEnd)
);

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#propdef-align-self
#[derive(Debug, Clone, PartialEq)]
pub enum AlignSelf {
  Auto,
  Normal,
  Stretch,
  BaselinePosition(BaselinePosition),
  SelfPosition(Option<OverflowPosition>, SelfPosition)
}

impl Parse for AlignSelf {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(AlignSelf::Auto)
    }

    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(AlignSelf::Normal)
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(AlignSelf::Stretch)
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(AlignSelf::BaselinePosition(val))
    }

    let overflow_position = input.try_parse(OverflowPosition::parse).ok();
    let self_position = SelfPosition::parse(input)?;
    Ok(AlignSelf::SelfPosition(overflow_position, self_position))
  }
}

impl ToCss for AlignSelf {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      AlignSelf::Auto => dest.write_str("auto"),
      AlignSelf::Normal => dest.write_str("normal"),
      AlignSelf::Stretch => dest.write_str("stretch"),
      AlignSelf::BaselinePosition(val) => val.to_css(dest),
      AlignSelf::SelfPosition(overflow, pos) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        pos.to_css(dest)
      }
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#propdef-justify-self
#[derive(Debug, Clone, PartialEq)]
pub enum JustifySelf {
  Auto,
  Normal,
  Stretch,
  BaselinePosition(BaselinePosition),
  SelfPosition(Option<OverflowPosition>, SelfPosition),
  Left(Option<OverflowPosition>),
  Right(Option<OverflowPosition>)
}

impl Parse for JustifySelf {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(JustifySelf::Auto)
    }

    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(JustifySelf::Normal)
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(JustifySelf::Stretch)
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(JustifySelf::BaselinePosition(val))
    }

    let overflow_position = input.try_parse(OverflowPosition::parse).ok();
    if let Ok(self_position) = input.try_parse(SelfPosition::parse) {
      return Ok(JustifySelf::SelfPosition(overflow_position, self_position))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "left" => Ok(JustifySelf::Left(overflow_position)),
      "right" => Ok(JustifySelf::Right(overflow_position)),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for JustifySelf {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      JustifySelf::Auto => dest.write_str("auto"),
      JustifySelf::Normal => dest.write_str("normal"),
      JustifySelf::Stretch => dest.write_str("stretch"),
      JustifySelf::BaselinePosition(val) => val.to_css(dest),
      JustifySelf::SelfPosition(overflow, pos) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        pos.to_css(dest)
      }
      JustifySelf::Left(overflow) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("left")
      }
      JustifySelf::Right(overflow) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("right")
      }
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#place-self-property
#[derive(Debug, Clone, PartialEq)]
pub struct PlaceSelf {
  pub align: AlignSelf,
  pub justify: JustifySelf
}

impl Parse for PlaceSelf {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
          AlignSelf::SelfPosition(o, c) => JustifySelf::SelfPosition(o.clone(), c.clone())
        }
      }
    };

    Ok(PlaceSelf { align, justify })
  }
}

impl ToCss for PlaceSelf {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.align.to_css(dest)?;
    let is_equal = match &self.justify {
      JustifySelf::Auto => true,
      JustifySelf::Normal => self.align == AlignSelf::Normal,
      JustifySelf::Stretch => self.align == AlignSelf::Normal,
      JustifySelf::BaselinePosition(p) if matches!(&self.align, AlignSelf::BaselinePosition(p2) if p == p2) => true,
      JustifySelf::SelfPosition(o, c) if matches!(&self.align, AlignSelf::SelfPosition(o2, c2) if o == o2 && c == c2) => true,
      _ => false
    };

    if !is_equal {
      dest.write_str(" ")?;
      self.justify.to_css(dest)?;
    }

    Ok(())
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#align-items-property
#[derive(Debug, Clone, PartialEq)]
pub enum AlignItems {
  Normal,
  Stretch,
  BaselinePosition(BaselinePosition),
  SelfPosition(Option<OverflowPosition>, SelfPosition)
}

impl Parse for AlignItems {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(AlignItems::Normal)
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(AlignItems::Stretch)
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(AlignItems::BaselinePosition(val))
    }

    let overflow_position = input.try_parse(OverflowPosition::parse).ok();
    let self_position = SelfPosition::parse(input)?;
    Ok(AlignItems::SelfPosition(overflow_position, self_position))
  }
}

impl ToCss for AlignItems {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      AlignItems::Normal => dest.write_str("normal"),
      AlignItems::Stretch => dest.write_str("stretch"),
      AlignItems::BaselinePosition(val) => val.to_css(dest),
      AlignItems::SelfPosition(overflow, pos) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        pos.to_css(dest)
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LegacyJustify {
  Left,
  Right,
  Center
}

impl Parse for LegacyJustify {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("legacy ")?;
    match self {
      LegacyJustify::Left => dest.write_str("left"),
      LegacyJustify::Right => dest.write_str("right"),
      LegacyJustify::Center => dest.write_str("center")
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#justify-items-property
#[derive(Debug, Clone, PartialEq)]
pub enum JustifyItems {
  Normal,
  Stretch,
  BaselinePosition(BaselinePosition),
  SelfPosition(Option<OverflowPosition>, SelfPosition),
  Left(Option<OverflowPosition>),
  Right(Option<OverflowPosition>),
  Legacy(LegacyJustify)
}

impl Parse for JustifyItems {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(JustifyItems::Normal)
    }

    if input.try_parse(|input| input.expect_ident_matching("stretch")).is_ok() {
      return Ok(JustifyItems::Stretch)
    }

    if let Ok(val) = input.try_parse(BaselinePosition::parse) {
      return Ok(JustifyItems::BaselinePosition(val))
    }

    if let Ok(val) = input.try_parse(LegacyJustify::parse) {
      return Ok(JustifyItems::Legacy(val))
    }

    let overflow_position = input.try_parse(OverflowPosition::parse).ok();
    if let Ok(self_position) = input.try_parse(SelfPosition::parse) {
      return Ok(JustifyItems::SelfPosition(overflow_position, self_position))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "left" => Ok(JustifyItems::Left(overflow_position)),
      "right" => Ok(JustifyItems::Right(overflow_position)),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for JustifyItems {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      JustifyItems::Normal => dest.write_str("normal"),
      JustifyItems::Stretch => dest.write_str("stretch"),
      JustifyItems::BaselinePosition(val) => val.to_css(dest),
      JustifyItems::Legacy(val) => val.to_css(dest),
      JustifyItems::SelfPosition(overflow, pos) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        pos.to_css(dest)
      }
      JustifyItems::Left(overflow) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("left")
      }
      JustifyItems::Right(overflow) => {
        if let Some(overflow) = overflow {
          overflow.to_css(dest)?;
          dest.write_str(" ")?;
        }

        dest.write_str("right")
      }
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#place-items-property
#[derive(Debug, Clone, PartialEq)]
pub struct PlaceItems {
  pub align: AlignItems,
  pub justify: JustifyItems
}

impl Parse for PlaceItems {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let align = AlignItems::parse(input)?;
    let justify = match input.try_parse(JustifyItems::parse) {
      Ok(j) => j,
      Err(_) => {
        // The second value is assigned to justify-items; if omitted, it is copied from the first value.
        match &align {
          AlignItems::Normal => JustifyItems::Normal,
          AlignItems::Stretch => JustifyItems::Stretch,
          AlignItems::BaselinePosition(p) => JustifyItems::BaselinePosition(p.clone()),
          AlignItems::SelfPosition(o, c) => JustifyItems::SelfPosition(o.clone(), c.clone())
        }
      }
    };

    Ok(PlaceItems { align, justify })
  }
}

impl ToCss for PlaceItems {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.align.to_css(dest)?;
    let is_equal = match &self.justify {
      JustifyItems::Normal => self.align == AlignItems::Normal,
      JustifyItems::Stretch => self.align == AlignItems::Normal,
      JustifyItems::BaselinePosition(p) if matches!(&self.align, AlignItems::BaselinePosition(p2) if p == p2) => true,
      JustifyItems::SelfPosition(o, c) if matches!(&self.align, AlignItems::SelfPosition(o2, c2) if o == o2 && c == c2) => true,
      _ => false
    };

    if !is_equal {
      dest.write_str(" ")?;
      self.justify.to_css(dest)?;
    }

    Ok(())
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#column-row-gap
#[derive(Debug, Clone, PartialEq)]
pub enum GapValue {
  Normal,
  LengthPercentage(LengthPercentage)
}

impl Parse for GapValue {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(GapValue::Normal)
    }

    let val = LengthPercentage::parse(input)?;
    Ok(GapValue::LengthPercentage(val))
  }
}

impl ToCss for GapValue {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      GapValue::Normal => dest.write_str("normal"),
      GapValue::LengthPercentage(lp) => lp.to_css(dest)
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-align-3-20200421/#gap-shorthand
#[derive(Debug, Clone, PartialEq)]
pub struct Gap {
  pub row: GapValue,
  pub column: GapValue
}

impl Parse for Gap {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let row = GapValue::parse(input)?;
    let column = input.try_parse(GapValue::parse).unwrap_or(row.clone());
    Ok(Gap { row, column })
  }
}

impl ToCss for Gap {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
  targets: Option<Browsers>,
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
  has_any: bool
}

impl AlignHandler {
  pub fn new(targets: Option<Browsers>) -> AlignHandler {
    AlignHandler {
      targets,
      ..AlignHandler::default()
    }
  }
}

impl PropertyHandler for AlignHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest);
          }
        }

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
      },
      FlexLinePack(val, vp) => property!(flex_line_pack, val, vp),
      JustifyContent(val, vp) => {
        self.box_pack = None;
        self.flex_pack = None;
        property!(justify_content, val, vp);
      },
      BoxPack(val, vp) => property!(box_pack, val, vp),
      FlexPack(val, vp) => property!(flex_pack, val, vp),
      PlaceContent(val) => {
        self.flex_line_pack = None;
        self.box_pack = None;
        self.flex_pack = None;
        property!(align_content, &val.align, &VendorPrefix::None);
        property!(justify_content, &val.justify, &VendorPrefix::None);
      }
      AlignSelf(val, vp) => {
        self.flex_item_align = None;
        property!(align_self, val, vp);
      },
      FlexItemAlign(val, vp) => property!(flex_item_align, val, vp),
      JustifySelf(val) => {
        self.justify_self = Some(val.clone());
        self.has_any = true;
      },
      PlaceSelf(val) => {
        self.flex_item_align = None;
        property!(align_self, &val.align, &VendorPrefix::None);
        self.justify_self = Some(val.justify.clone());
      }
      AlignItems(val, vp) => {
        self.box_align = None;
        self.flex_align = None;
        property!(align_items, val, vp);
      },
      BoxAlign(val, vp) => property!(box_align, val, vp),
      FlexAlign(val, vp) => property!(flex_align, val, vp),
      JustifyItems(val) => {
        self.justify_items = Some(val.clone());
        self.has_any = true;
      },
      PlaceItems(val) => {
        self.box_align = None;
        self.flex_align = None;
        property!(align_items, &val.align, &VendorPrefix::None);
        self.justify_items = Some(val.justify.clone());
      }
      RowGap(val) => {
        self.row_gap = Some(val.clone());
        self.has_any = true;
      },
      ColumnGap(val) => {
        self.column_gap = Some(val.clone());
        self.has_any = true;
      },
      Gap(val) => {
        self.row_gap = Some(val.row.clone());
        self.column_gap = Some(val.column.clone());
        self.has_any = true;
      }
      Unparsed(val) if is_align_property(&val.property_id) => {
        self.flush(dest);
        dest.push(property.clone()) // TODO: prefix?
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    self.flush(dest);
  }
}

impl AlignHandler {
  fn flush(&mut self, dest: &mut DeclarationList) {
    if !self.has_any {
      return
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
        let mut prefix = VendorPrefix::None;
        if let Some(targets) = self.targets {
          prefix = Feature::$prop.prefixes_for(targets);
          // Firefox only implemented the 2009 spec prefixed.
          // Microsoft only implemented the 2012 spec prefixed.
          prefix.remove(VendorPrefix::Moz | VendorPrefix::Ms);
        }
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
          let mut prefix = *prefix;
          if prefix.contains(VendorPrefix::None) {
            if let Some(targets) = self.targets {
              prefix = Feature::$prop.prefixes_for(targets);

              // 2009 spec, implemented by webkit and firefox.
              $(
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
              )?
              
              // 2012 spec, implemented by microsoft.
              if prefix.contains(VendorPrefix::Ms) {
                if let Some(v) = $prop_2012::from_standard(&val) {
                  dest.push(Property::$prop_2012(v, VendorPrefix::Ms));
                }
              }

              // Remove Firefox and IE from standard prefixes.
              prefix.remove(VendorPrefix::Moz | VendorPrefix::Ms);
            }
          }
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
    if self.targets.is_none() || compat::Feature::PlaceContent.is_compatible(self.targets.unwrap()) {
      shorthand!(PlaceContent, AlignContent, align_content, justify_content, JustifyContent);
    }
    standard_property!(AlignContent, align_content);
    standard_property!(JustifyContent, justify_content);

    legacy_property!(AlignSelf, align_self, , FlexItemAlign);
    if self.targets.is_none() || compat::Feature::PlaceSelf.is_compatible(self.targets.unwrap()) {
      shorthand!(PlaceSelf, AlignSelf, align_self, justify_self);
    }
    standard_property!(AlignSelf, align_self);
    unprefixed_property!(JustifySelf, justify_self);

    legacy_property!(AlignItems, align_items, BoxAlign, FlexAlign);
    if self.targets.is_none() || compat::Feature::PlaceItems.is_compatible(self.targets.unwrap()) {
      shorthand!(PlaceItems, AlignItems, align_items, justify_items);
    }
    standard_property!(AlignItems, align_items);
    unprefixed_property!(JustifyItems, justify_items);

    if row_gap.is_some() && column_gap.is_some() {
      dest.push(Property::Gap(Gap {
        row: row_gap.unwrap(),
        column: column_gap.unwrap()
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
    PropertyId::AlignContent(_) |
    PropertyId::FlexLinePack(_) |
    PropertyId::JustifyContent(_) |
    PropertyId::BoxPack(_) |
    PropertyId::FlexPack(_) |
    PropertyId::PlaceContent |
    PropertyId::AlignSelf(_) |
    PropertyId::FlexItemAlign(_) |
    PropertyId::JustifySelf |
    PropertyId::PlaceSelf |
    PropertyId::AlignItems(_) |
    PropertyId::BoxAlign(_) |
    PropertyId::FlexAlign(_) |
    PropertyId::JustifyItems |
    PropertyId::PlaceItems |
    PropertyId::RowGap |
    PropertyId::ColumnGap |
    PropertyId::Gap => true,
    _ => false
  }
}
