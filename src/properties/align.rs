use cssparser::*;
use crate::macros::*;
use crate::values::length::LengthPercentage;
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::Property;
use crate::printer::Printer;

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
  align: AlignContent,
  justify: JustifyContent
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
  align: AlignSelf,
  justify: JustifySelf
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
  align: AlignItems,
  justify: JustifyItems
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
  row: GapValue,
  column: GapValue
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
pub struct AlignHandler {
  align_content: Option<AlignContent>,
  justify_content: Option<JustifyContent>,
  align_self: Option<AlignSelf>,
  justify_self: Option<JustifySelf>,
  align_items: Option<AlignItems>,
  justify_items: Option<JustifyItems>,
  row_gap: Option<GapValue>,
  column_gap: Option<GapValue>
}

impl PropertyHandler for AlignHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;

    match property {
      AlignContent(val) => self.align_content = Some(val.clone()),
      JustifyContent(val) => self.justify_content = Some(val.clone()),
      PlaceContent(val) => {
        self.align_content = Some(val.align.clone());
        self.justify_content = Some(val.justify.clone());
      }
      AlignSelf(val) => self.align_self = Some(val.clone()),
      JustifySelf(val) => self.justify_self = Some(val.clone()),
      PlaceSelf(val) => {
        self.align_self = Some(val.align.clone());
        self.justify_self = Some(val.justify.clone());
      }
      AlignItems(val) => self.align_items = Some(val.clone()),
      JustifyItems(val) => self.justify_items = Some(val.clone()),
      PlaceItems(val) => {
        self.align_items = Some(val.align.clone());
        self.justify_items = Some(val.justify.clone());
      }
      RowGap(val) => self.row_gap = Some(val.clone()),
      ColumnGap(val) => self.column_gap = Some(val.clone()),
      Gap(val) => {
        self.row_gap = Some(val.row.clone());
        self.column_gap = Some(val.column.clone());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self) -> Vec<Property> {
    let mut decls = vec![];
    let align_content = std::mem::take(&mut self.align_content);
    let justify_content = std::mem::take(&mut self.justify_content);
    let align_self = std::mem::take(&mut self.align_self);
    let justify_self = std::mem::take(&mut self.justify_self);
    let align_items = std::mem::take(&mut self.align_items);
    let justify_items = std::mem::take(&mut self.justify_items);
    let row_gap = std::mem::take(&mut self.row_gap);
    let column_gap = std::mem::take(&mut self.column_gap);

    if align_content.is_some() && justify_content.is_some() {
      decls.push(Property::PlaceContent(PlaceContent {
        align: align_content.unwrap(),
        justify: justify_content.unwrap()
      }))
    } else {
      if let Some(align) = align_content {
        decls.push(Property::AlignContent(align))
      }

      if let Some(justify) = justify_content {
        decls.push(Property::JustifyContent(justify))
      }
    }

    if align_self.is_some() && justify_self.is_some() {
      decls.push(Property::PlaceSelf(PlaceSelf {
        align: align_self.unwrap(),
        justify: justify_self.unwrap()
      }))
    } else {
      if let Some(align) = align_self {
        decls.push(Property::AlignSelf(align))
      }

      if let Some(justify) = justify_self {
        decls.push(Property::JustifySelf(justify))
      }
    }

    if align_items.is_some() && justify_items.is_some() {
      decls.push(Property::PlaceItems(PlaceItems {
        align: align_items.unwrap(),
        justify: justify_items.unwrap()
      }))
    } else {
      if let Some(align) = align_items {
        decls.push(Property::AlignItems(align))
      }

      if let Some(justify) = justify_items {
        decls.push(Property::JustifyItems(justify))
      }
    }

    if row_gap.is_some() && column_gap.is_some() {
      decls.push(Property::Gap(Gap {
        row: row_gap.unwrap(),
        column: column_gap.unwrap()
      }))
    } else {
      if let Some(gap) = row_gap {
        decls.push(Property::RowGap(gap))
      }

      if let Some(gap) = column_gap {
        decls.push(Property::ColumnGap(gap))
      }
    }

    decls
  }
}
