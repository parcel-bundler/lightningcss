#![allow(non_upper_case_globals)]

use cssparser::*;
use crate::values::{
  length::{LengthPercentage}
};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::values::ident::CustomIdent;
use smallvec::SmallVec;
use crate::values::length::serialize_dimension;
use bitflags::bitflags;

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

impl Default for TrackSize {
  fn default() -> TrackSize {
    TrackSize::TrackBreadth(TrackBreadth::Auto)
  }
}

/// https://drafts.csswg.org/css-grid-2/#auto-tracks
#[derive(Debug, Clone, PartialEq)]
pub struct TrackSizeList(pub SmallVec<[TrackSize; 1]>);

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

fn serialize_line_names<W>(names: &[CustomIdent], dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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

impl Parse for TrackSizeList {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut res = SmallVec::new();
    while let Ok(size) = input.try_parse(TrackSize::parse) {
      res.push(size)
    }
    Ok(TrackSizeList(res))
  }
}

impl ToCss for TrackSizeList {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut first = true;
    for item in &self.0 {
      if first {
        first = false;
      } else {
        dest.write_char(' ')?;
      }
      item.to_css(dest)?;
    }
    Ok(())
  }
}

/// https://drafts.csswg.org/css-grid-2/#grid-template-areas-property
#[derive(Debug, Clone, PartialEq)]
pub enum GridTemplateAreas {
  None,
  Areas {
    columns: u32,
    areas: Vec<Option<String>>
  }
}

impl Parse for GridTemplateAreas {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(GridTemplateAreas::None)
    }

    let mut tokens = Vec::new();
    let mut row = 0;
    let mut columns = 0;
    while let Ok(s) = input.try_parse(|input| input.expect_string().map(|s| s.as_ref().to_owned())) {
      let parsed_columns = Self::parse_string(&s, &mut tokens)
        .map_err(|()| input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;

      if row == 0 {
        columns = parsed_columns;
      } else if parsed_columns != columns {
        return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
      }

      row += 1;
    }

    Ok(GridTemplateAreas::Areas {
      columns,
      areas: tokens
    })
  }
}

impl GridTemplateAreas {
  fn parse_string(string: &str, tokens: &mut Vec<Option<String>>) -> Result<u32, ()> {
    let mut string = string;
    let mut column = 0;
    loop {
      let rest = string.trim_start_matches(HTML_SPACE_CHARACTERS);
      if rest.is_empty() {
        // Each string must produce a valid token.
        if column == 0 {
          return Err(())
        }
        break
      }

      column += 1;

      if rest.starts_with('.') {
        string = &rest[rest.find(|c| c != '.').unwrap_or(rest.len())..];
        tokens.push(None);
        continue
      }

      if !rest.starts_with(is_name_code_point) {
        return Err(())
      }

      let token_len = rest.find(|c| !is_name_code_point(c)).unwrap_or(rest.len());
      let token = &rest[..token_len];
      tokens.push(Some(token.into()));
      string = &rest[token_len..];
    }

    Ok(column)
  }
}

static HTML_SPACE_CHARACTERS: &'static [char] = &['\u{0020}', '\u{0009}', '\u{000a}', '\u{000c}', '\u{000d}'];

fn is_name_code_point(c: char) -> bool {
  c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '\u{80}' ||
    c == '_' ||
    c >= '0' && c <= '9' ||
    c == '-'
}

impl ToCss for GridTemplateAreas {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      GridTemplateAreas::None => dest.write_str("none"),
      GridTemplateAreas::Areas { areas, .. } => {
        let mut iter = areas.iter();
        let mut next = iter.next();
        let mut first = true;
        while next.is_some() {
          if !first && !dest.minify {
            dest.newline()?;
          }

          self.write_string(dest, &mut iter, &mut next)?;

          if first {
            first = false;
            if !dest.minify {
              // Indent by the width of "grid-template-areas: ", so the rows line up.
              dest.indent_by(21);
            }
          }
        }

        if !dest.minify {
          dest.dedent_by(21);
        }
        
        Ok(())
      }
    }
  }
}

impl GridTemplateAreas {
  fn write_string<'a, W>(&self, dest: &mut Printer<W>, iter: &mut std::slice::Iter<'a, Option<String>>, next: &mut Option<&'a Option<String>>) -> std::fmt::Result where W: std::fmt::Write {
    let columns = match self {
      GridTemplateAreas::Areas { columns, .. } => *columns,
      _ => unreachable!()
    };

    dest.write_char('"')?;

    let mut last_was_null = false;
    for i in 0..columns {
      if let Some(token) = next {
        if let Some(string) = token {
          if i > 0 && (!last_was_null || !dest.minify) {
            dest.write_char(' ')?;
          }
          dest.write_str(string)?;
          last_was_null = false;
        } else {
          if last_was_null || !dest.minify {
            dest.write_char(' ')?;
          }
          dest.write_char('.')?;
          last_was_null = true;
        }
      }

      *next = iter.next();
    }

    dest.write_char('"')
  }
}

/// https://drafts.csswg.org/css-grid-2/#explicit-grid-shorthand
#[derive(Debug, Clone, PartialEq)]
pub struct GridTemplate {
  rows: TrackSizing,
  columns: TrackSizing,
  areas: GridTemplateAreas
}

impl Parse for GridTemplate {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(GridTemplate {
        rows: TrackSizing::None,
        columns: TrackSizing::None,
        areas: GridTemplateAreas::None
      })
    }

    let start = input.state();
    let mut line_names: Vec<SmallVec<[CustomIdent; 1]>> = Vec::new();
    let mut items = Vec::new();
    let mut columns = 0;
    let mut row = 0;
    let mut tokens = Vec::new();
    
    loop {
      if let Ok(first_names) = input.try_parse(parse_line_names) {
        if let Some(last_names) = line_names.last_mut() {
          last_names.extend(first_names);
        } else {
          line_names.push(first_names);
        }
      }

      if let Ok(string) = input.try_parse(|input| input.expect_string().map(|s| s.as_ref().to_owned())) {
        let parsed_columns = GridTemplateAreas::parse_string(&string, &mut tokens)
          .map_err(|()| input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;

        if row == 0 {
          columns = parsed_columns;
        } else if parsed_columns != columns {
          return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }
  
        row += 1;

        let track_size = input.try_parse(TrackSize::parse).unwrap_or_default();
        items.push(TrackListItem::TrackSize(track_size));

        let last_names = input.try_parse(parse_line_names).unwrap_or_default();
        line_names.push(last_names);
      } else {
        break
      }
    }

    if !tokens.is_empty() {
      if line_names.len() == items.len() {
        line_names.push(Default::default());
      }

      let areas = GridTemplateAreas::Areas {
        columns,
        areas: tokens
      };
      let rows = TrackSizing::TrackList(TrackList {
        line_names,
        items
      });
      let columns = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
        let list = TrackList::parse(input)?;
        // TODO: check explicit
        TrackSizing::TrackList(list)
      } else {
        TrackSizing::None
      };
      Ok(GridTemplate {
        rows,
        columns,
        areas
      })
    } else {
      input.reset(&start);
      let rows = TrackSizing::parse(input)?;
      input.expect_delim('/')?;
      let columns = TrackSizing::parse(input)?;
      Ok(GridTemplate {
        rows,
        columns,
        areas: GridTemplateAreas::None
      })
    }
  }
}

impl ToCss for GridTemplate {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match &self.areas {
      GridTemplateAreas::None => {
        if self.rows == TrackSizing::None && self.columns == TrackSizing::None {
          dest.write_str("none")?;
        } else {
          self.rows.to_css(dest)?;
          dest.delim('/', true)?;
          self.columns.to_css(dest)?;
        }
      },
      GridTemplateAreas::Areas { areas, .. } => {
        let track_list = match &self.rows {
          TrackSizing::TrackList(list) => list,
          _ => unreachable!()
        };

        let mut areas_iter = areas.iter();
        let mut line_names_iter = track_list.line_names.iter();
        let mut items_iter = track_list.items.iter();

        let mut next = areas_iter.next();
        let mut first = true;
        let mut indented = false;
        while next.is_some() {
          macro_rules! newline {
            () => {    
              if !dest.minify {
                if !indented {
                  // Indent by the width of "grid-template: ", so the rows line up.
                  dest.indent_by(15);
                  indented = true;
                }   
                dest.newline()?; 
              }
            };
          }
  
          if let Some(line_names) = line_names_iter.next() {
            if !line_names.is_empty() {
              if !dest.minify && line_names.len() == 2 {
                dest.whitespace()?;
                serialize_line_names(&line_names[0..1], dest)?;
                newline!();
                serialize_line_names(&line_names[1..], dest)?;
              } else {
                if !first {
                  newline!();
                }
                serialize_line_names(line_names, dest)?;
              }
              dest.whitespace()?;
            } else {
              newline!();
            }
          } else {
            newline!();
          }

          self.areas.write_string(dest, &mut areas_iter, &mut next)?;

          if let Some(item) = items_iter.next() {
            if *item != TrackListItem::TrackSize(TrackSize::default()) {
              dest.whitespace()?;
              match item {
                TrackListItem::TrackSize(size) => size.to_css(dest)?,
                _ => unreachable!()
              }
            }
          }

          first = false;
        }

        if let Some(line_names) = line_names_iter.next() {
          if !line_names.is_empty() {
            dest.whitespace()?;
            serialize_line_names(line_names, dest)?;
          }
        }

        if let TrackSizing::TrackList(track_list) = &self.columns {
          dest.newline()?;
          dest.delim('/', false)?;
          track_list.to_css(dest)?;
        }

        if indented {
          dest.dedent_by(15);
        }
      }
    }

    Ok(())
  }
}

bitflags! {
  pub struct GridAutoFlow: u8 {
    const Row    = 0b00;
    const Column = 0b01;
    const Dense  = 0b10;
  }
}

impl Parse for GridAutoFlow {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut flow = GridAutoFlow::Row;

    macro_rules! match_dense {
      () => {
        if input.try_parse(|input| input.expect_ident_matching("dense")).is_ok() {
          flow |= GridAutoFlow::Dense;
        }
      };
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &ident,
      "row" => {
        match_dense!();
      },
      "column" => {
        flow = GridAutoFlow::Column;
        match_dense!();
      },
      "dense" => {
        let location = input.current_source_location();
        input.try_parse(|input| {
          let ident = input.expect_ident()?;
          match_ignore_ascii_case! { &ident,
            "row" => {},
            "column" => {
              flow = GridAutoFlow::Column;
            },
            _ => return Err(location.new_unexpected_token_error(
              cssparser::Token::Ident(ident.clone())
            ))
          }
          Ok(())
        })?;
        flow |= GridAutoFlow::Dense;
      },
      _ => return Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }

    Ok(flow)
  }
}

impl ToCss for GridAutoFlow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let s = if *self == GridAutoFlow::Row {
      "row"
    } else if *self == GridAutoFlow::Column {
      "column"
    } else if *self == GridAutoFlow::Row | GridAutoFlow::Dense {
      if dest.minify {
        "dense"
      } else {
        "row dense"
      }
    } else if *self == GridAutoFlow::Column | GridAutoFlow::Dense {
      "column dense"
    } else {
      unreachable!();
    };

    dest.write_str(s)
  }
}
