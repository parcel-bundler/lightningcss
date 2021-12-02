use cssparser::*;
use crate::values::{
  length::{LengthPercentage}
};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::values::ident::CustomIdent;
use smallvec::SmallVec;
use crate::values::length::serialize_dimension;

/// https://drafts.csswg.org/css-grid-2/#track-sizing
#[derive(Debug, Clone, PartialEq)]
pub enum TrackSizing {
  None,
  TrackList(TrackList),
}

/// https://drafts.csswg.org/css-grid-2/#typedef-track-list
#[derive(Debug, Clone, PartialEq)]
pub struct TrackList {
  pub line_names: Vec<SmallVec<[CustomIdent; 1]>>,
  pub items: Vec<TrackListItem>
}

#[derive(Debug, Clone, PartialEq)]
pub enum TrackListItem {
  TrackSize(TrackSize),
  TrackRepeat(TrackRepeat)
}

/// https://drafts.csswg.org/css-grid-2/#typedef-track-size
#[derive(Debug, Clone, PartialEq)]
pub enum TrackSize {
  TrackBreadth(TrackBreadth),
  MinMax(TrackBreadth, TrackBreadth),
  FitContent(LengthPercentage)
}

/// https://drafts.csswg.org/css-grid-2/#typedef-track-breadth
#[derive(Debug, Clone, PartialEq)]
pub enum TrackBreadth {
  Length(LengthPercentage),
  Flex(f32),
  MinContent,
  MaxContent,
  Auto
}

/// https://drafts.csswg.org/css-grid-2/#typedef-track-repeat
#[derive(Debug, Clone, PartialEq)]
pub struct TrackRepeat {
  count: RepeatCount,
  line_names: Vec<SmallVec<[CustomIdent; 1]>>,
  track_sizes: Vec<TrackSize>
}

/// https://drafts.csswg.org/css-grid-2/#typedef-track-repeat
#[derive(Debug, Clone, PartialEq)]
pub enum RepeatCount {
  Number(f32),
  AutoFill,
  AutoFit
}

impl Parse for TrackSize {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(breadth) = input.try_parse(TrackBreadth::parse) {
      return Ok(TrackSize::TrackBreadth(breadth))
    }

    if input.try_parse(|input| input.expect_function_matching("minmax")).is_ok() {
      return input.parse_nested_block(|input| {
        let breadth = TrackBreadth::parse_internal(input, false)?;
        input.expect_comma()?;
        Ok(TrackSize::MinMax(
          breadth,
          TrackBreadth::parse(input)?,
        ))
      })
    }

    input.expect_function_matching("fit-content")?;
    let len = input.parse_nested_block(LengthPercentage::parse)?;
    Ok(TrackSize::FitContent(len))
  }
}

impl ToCss for TrackSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      TrackSize::TrackBreadth(breadth) => breadth.to_css(dest),
      TrackSize::MinMax(a, b) => {
        dest.write_str("minmax(")?;
        a.to_css(dest)?;
        dest.delim(',', false)?;
        b.to_css(dest)?;
        dest.write_char(')')
      }
      TrackSize::FitContent(len) => {
        dest.write_str("fit-content(")?;
        len.to_css(dest)?;
        dest.write_char(')')
      }
    }
  }
}

impl Parse for TrackBreadth {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    Self::parse_internal(input, true)
  }
}

impl TrackBreadth {
  fn parse_internal<'i, 't>(input: &mut Parser<'i, 't>, allow_flex: bool) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(len) = input.try_parse(LengthPercentage::parse) {
      return Ok(TrackBreadth::Length(len))
    }

    if allow_flex {
      if let Ok(flex) = input.try_parse(Self::parse_flex) {
        return Ok(TrackBreadth::Flex(flex))
      }
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "auto" => Ok(TrackBreadth::Auto),
      "min-content" => Ok(TrackBreadth::MinContent),
      "max-content" => Ok(TrackBreadth::MaxContent),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }

  fn parse_flex<'i, 't>(input: &mut Parser<'i, 't>) -> Result<f32, ParseError<'i, ()>> {
    let location = input.current_source_location();
    match *input.next()? {
      Token::Dimension { value, ref unit, .. } if unit.eq_ignore_ascii_case("fr") && value.is_sign_positive() => Ok(value),
      ref t => Err(location.new_unexpected_token_error(t.clone())),
    }
  }
}

impl ToCss for TrackBreadth {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      TrackBreadth::Auto => dest.write_str("auto"),
      TrackBreadth::MinContent => dest.write_str("min-content"),
      TrackBreadth::MaxContent => dest.write_str("max-content"),
      TrackBreadth::Length(len) => len.to_css(dest),
      TrackBreadth::Flex(flex) => serialize_dimension(*flex, "fr", dest)
    }
  }
}

impl Parse for TrackRepeat {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    input.expect_function_matching("repeat")?;
    input.parse_nested_block(|input| {
      let count = RepeatCount::parse(input)?;
      input.expect_comma()?;

      let mut line_names = Vec::new();
      let mut track_sizes = Vec::new();

      loop {
        let line_name = input.try_parse(parse_line_names).unwrap_or_default();
        line_names.push(line_name);

        if let Ok(track_size) = input.try_parse(TrackSize::parse) {
          // TODO: error handling
          track_sizes.push(track_size)
        } else {
          break
        }
      }

      Ok(TrackRepeat {
        count,
        line_names,
        track_sizes
      })
    })
  }
}

impl ToCss for TrackRepeat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("repeat(")?;
    self.count.to_css(dest)?;
    dest.delim(',', false)?;
    
    let mut track_sizes_iter = self.track_sizes.iter();
    let mut first = true;
    for names in self.line_names.iter() {
      if !names.is_empty() {
        serialize_line_names(names, dest)?;
      }

      if let Some(size) = track_sizes_iter.next() {
        // Whitespace is required if there are no line names.
        if !names.is_empty() {
          dest.whitespace()?;
        } else if !first {
          dest.write_char(' ')?;
        }
        size.to_css(dest)?;
      }

      first = false;
    }

    dest.write_char(')')
  }
}

impl Parse for RepeatCount {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(num) = input.try_parse(f32::parse) {
      return Ok(RepeatCount::Number(num))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "auto-fill" => Ok(RepeatCount::AutoFill),
      "auto-fit" => Ok(RepeatCount::AutoFit),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for RepeatCount {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      RepeatCount::AutoFill => dest.write_str("auto-fill"),
      RepeatCount::AutoFit => dest.write_str("auto-fit"),
      RepeatCount::Number(num) => num.to_css(dest)
    }
  }
}

fn parse_line_names<'i, 't>(input: &mut Parser<'i, 't>) -> Result<SmallVec<[CustomIdent; 1]>, ParseError<'i, ()>> {
  input.expect_square_bracket_block()?;
  input.parse_nested_block(|input| {
    let mut values = SmallVec::new();
    while let Ok(ident) = input.try_parse(CustomIdent::parse) {
      values.push(ident)
    }
    Ok(values)
  })
}

fn serialize_line_names<W>(names: &SmallVec<[CustomIdent; 1]>, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
  dest.write_char('[')?;
  let mut first = true;
  for name in names {
    if first {
      first = false;
    } else {
      dest.write_char(' ')?;
    }
    name.to_css(dest)?;
  }
  dest.write_char(']')
}

impl Parse for TrackList {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut line_names = Vec::new();
    let mut items = Vec::new();

    loop {
      let line_name = input.try_parse(parse_line_names).unwrap_or_default();
      line_names.push(line_name);

      if let Ok(track_size) = input.try_parse(TrackSize::parse) {
        // TODO: error handling
        items.push(TrackListItem::TrackSize(track_size));
      } else if let Ok(repeat) = input.try_parse(TrackRepeat::parse) {
        // TODO: error handling
        items.push(TrackListItem::TrackRepeat(repeat))
      } else {
        break
      }
    }

    Ok(TrackList {
      line_names,
      items
    })
  }
}

impl ToCss for TrackList {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut items_iter = self.items.iter();
    let line_names_iter = self.line_names.iter();
    let mut first = true;

    for names in line_names_iter {
      if !names.is_empty() {
        serialize_line_names(names, dest)?;
      }

      if let Some(item) = items_iter.next() {
        // Whitespace is required if there are no line names.
        if !names.is_empty() {
          dest.whitespace()?;
        } else if !first {
          dest.write_char(' ')?;
        }
        match item {
          TrackListItem::TrackRepeat(repeat) => repeat.to_css(dest)?,
          TrackListItem::TrackSize(size) => size.to_css(dest)?
        };
      }

      first = false;
    }

    Ok(())
  }
}

impl Parse for TrackSizing {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(TrackSizing::None)
    }

    let track_list = TrackList::parse(input)?;
    Ok(TrackSizing::TrackList(track_list))
  }
}

impl ToCss for TrackSizing {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      TrackSizing::None => dest.write_str("none"),
      TrackSizing::TrackList(list) => list.to_css(dest)
    }
  }
}
