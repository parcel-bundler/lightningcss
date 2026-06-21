//! CSS properties related to grid layout.

#![allow(non_upper_case_globals)]

use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{Error, ErrorLocation, ParserError, PrinterError, PrinterErrorKind};
use crate::macros::{define_shorthand, impl_shorthand};
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::traits::{Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::ident::CustomIdent;
use crate::values::length::serialize_dimension;
use crate::values::number::{CSSInteger, CSSNumber};
use crate::values::{ident::CustomIdentList, length::LengthPercentage};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use bitflags::bitflags;
use cssparser::*;
use smallvec::SmallVec;

#[cfg(feature = "serde")]
use crate::serialization::ValueWrapper;

/// A [track sizing](https://drafts.csswg.org/css-grid-2/#track-sizing) value
/// for the `grid-template-rows` and `grid-template-columns` properties.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum TrackSizing<'i> {
  /// No explicit grid tracks.
  None,
  /// A list of grid tracks.
  #[cfg_attr(feature = "serde", serde(borrow))]
  TrackList(TrackList<'i>),
}

/// A [`<track-list>`](https://drafts.csswg.org/css-grid-2/#typedef-track-list) value,
/// as used in the `grid-template-rows` and `grid-template-columns` properties.
///
/// See [TrackSizing](TrackSizing).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TrackList<'i> {
  /// A list of line names.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub line_names: Vec<CustomIdentList<'i>>,
  /// A list of grid track items.
  pub items: Vec<TrackListItem<'i>>,
}

/// Either a track size or `repeat()` function.
///
/// See [TrackList](TrackList).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum TrackListItem<'i> {
  /// A track size.
  TrackSize(TrackSize),
  /// A `repeat()` function.
  #[cfg_attr(feature = "serde", serde(borrow))]
  TrackRepeat(TrackRepeat<'i>),
}

/// A [`<track-size>`](https://drafts.csswg.org/css-grid-2/#typedef-track-size) value,
/// as used in the `grid-template-rows` and `grid-template-columns` properties.
///
/// See [TrackListItem](TrackListItem).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum TrackSize {
  /// An explicit track breadth.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<TrackBreadth>"))]
  TrackBreadth(TrackBreadth),
  /// The `minmax()` function.
  MinMax {
    /// The minimum value.
    min: TrackBreadth,
    /// The maximum value.
    max: TrackBreadth,
  },
  /// The `fit-content()` function.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<LengthPercentage>"))]
  FitContent(LengthPercentage),
}

impl Default for TrackSize {
  fn default() -> TrackSize {
    TrackSize::TrackBreadth(TrackBreadth::Auto)
  }
}

/// A [track size list](https://drafts.csswg.org/css-grid-2/#auto-tracks), as used
/// in the `grid-auto-rows` and `grid-auto-columns` properties.
#[derive(Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct TrackSizeList(pub SmallVec<[TrackSize; 1]>);

/// A [`<track-breadth>`](https://drafts.csswg.org/css-grid-2/#typedef-track-breadth) value.
///
/// See [TrackSize](TrackSize).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum TrackBreadth {
  /// An explicit length.
  Length(LengthPercentage),
  /// A flex factor.
  Flex(CSSNumber),
  /// The `min-content` keyword.
  MinContent,
  /// The `max-content` keyword.
  MaxContent,
  /// The `auto` keyword.
  Auto,
}

/// A [`<track-repeat>`](https://drafts.csswg.org/css-grid-2/#typedef-track-repeat) value,
/// representing the `repeat()` function in a track list.
///
/// See [TrackListItem](TrackListItem).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TrackRepeat<'i> {
  /// The repeat count.
  pub count: RepeatCount,
  /// The line names to repeat.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub line_names: Vec<CustomIdentList<'i>>,
  /// The track sizes to repeat.
  pub track_sizes: Vec<TrackSize>,
}

/// A [`<repeat-count>`](https://drafts.csswg.org/css-grid-2/#typedef-track-repeat) value,
/// used in the `repeat()` function.
///
/// See [TrackRepeat](TrackRepeat).
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum RepeatCount {
  /// The number of times to repeat.
  Number(CSSInteger),
  /// The `auto-fill` keyword.
  AutoFill,
  /// The `auto-fit` keyword.
  AutoFit,
}

impl<'i> Parse<'i> for TrackSize {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(breadth) = input.try_parse(TrackBreadth::parse) {
      return Ok(TrackSize::TrackBreadth(breadth));
    }

    if input.try_parse(|input| input.expect_function_matching("minmax")).is_ok() {
      return input.parse_nested_block(|input| {
        let min = TrackBreadth::parse_internal(input, false)?;
        input.expect_comma()?;
        Ok(TrackSize::MinMax {
          min,
          max: TrackBreadth::parse(input)?,
        })
      });
    }

    input.expect_function_matching("fit-content")?;
    let len = input.parse_nested_block(LengthPercentage::parse)?;
    Ok(TrackSize::FitContent(len))
  }
}

impl ToCss for TrackSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      TrackSize::TrackBreadth(breadth) => breadth.to_css(dest),
      TrackSize::MinMax { min, max } => {
        dest.write_str("minmax(")?;
        min.to_css(dest)?;
        dest.delim(',', false)?;
        max.to_css(dest)?;
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

impl<'i> Parse<'i> for TrackBreadth {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Self::parse_internal(input, true)
  }
}

impl TrackBreadth {
  fn parse_internal<'i, 't>(
    input: &mut Parser<'i, 't>,
    allow_flex: bool,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(len) = input.try_parse(LengthPercentage::parse) {
      return Ok(TrackBreadth::Length(len));
    }

    if allow_flex {
      if let Ok(flex) = input.try_parse(Self::parse_flex) {
        return Ok(TrackBreadth::Flex(flex));
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

  fn parse_flex<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CSSNumber, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    match *input.next()? {
      Token::Dimension { value, ref unit, .. } if unit.eq_ignore_ascii_case("fr") && value.is_sign_positive() => {
        Ok(value)
      }
      ref t => Err(location.new_unexpected_token_error(t.clone())),
    }
  }
}

impl ToCss for TrackBreadth {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      TrackBreadth::Auto => dest.write_str("auto"),
      TrackBreadth::MinContent => dest.write_str("min-content"),
      TrackBreadth::MaxContent => dest.write_str("max-content"),
      TrackBreadth::Length(len) => len.to_css(dest),
      TrackBreadth::Flex(flex) => serialize_dimension(*flex, "fr", dest),
    }
  }
}

impl<'i> Parse<'i> for TrackRepeat<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
          break;
        }
      }

      Ok(TrackRepeat {
        count,
        line_names,
        track_sizes,
      })
    })
  }
}

impl<'i> ToCss for TrackRepeat<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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

fn parse_line_names<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<CustomIdentList<'i>, ParseError<'i, ParserError<'i>>> {
  input.expect_square_bracket_block()?;
  input.parse_nested_block(|input| {
    let mut values = SmallVec::new();
    while let Ok(ident) = input.try_parse(CustomIdent::parse) {
      values.push(ident)
    }
    Ok(values)
  })
}

fn serialize_line_names<W>(names: &[CustomIdent], dest: &mut Printer<W>) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  dest.write_char('[')?;
  let mut first = true;
  for name in names {
    if first {
      first = false;
    } else {
      dest.write_char(' ')?;
    }
    write_ident(&name.0, dest)?;
  }
  dest.write_char(']')
}

fn write_ident<W>(name: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  let css_module_grid_enabled = dest.css_module.as_ref().map_or(false, |css_module| css_module.config.grid);
  if css_module_grid_enabled {
    if let Some(css_module) = &mut dest.css_module {
      if let Some(last) = css_module.config.pattern.segments.last() {
        if !matches!(last, crate::css_modules::Segment::Local) {
          return Err(Error {
            kind: PrinterErrorKind::InvalidCssModulesPatternInGrid,
            loc: Some(ErrorLocation {
              filename: dest.filename().into(),
              line: dest.loc.line,
              column: dest.loc.column,
            }),
          });
        }
      }
    }
  }
  dest.write_ident(name, css_module_grid_enabled)?;
  Ok(())
}

impl<'i> Parse<'i> for TrackList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
        break;
      }
    }

    if items.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }

    Ok(TrackList { line_names, items })
  }
}

impl<'i> ToCss for TrackList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
          TrackListItem::TrackSize(size) => size.to_css(dest)?,
        };
      }

      first = false;
    }

    Ok(())
  }
}

impl<'i> TrackList<'i> {
  fn is_explicit(&self) -> bool {
    self.items.iter().all(|item| matches!(item, TrackListItem::TrackSize(_)))
  }
}

impl<'i> TrackSizing<'i> {
  fn is_explicit(&self) -> bool {
    match self {
      TrackSizing::None => true,
      TrackSizing::TrackList(list) => list.is_explicit(),
    }
  }
}

impl<'i> Parse<'i> for TrackSizeList {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut res = SmallVec::new();
    while let Ok(size) = input.try_parse(TrackSize::parse) {
      res.push(size)
    }
    if res.len() == 1 && res[0] == TrackSize::default() {
      res.clear();
    }
    Ok(TrackSizeList(res))
  }
}

impl ToCss for TrackSizeList {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.0.len() == 0 {
      return dest.write_str("auto");
    }

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

/// A value for the [grid-template-areas](https://drafts.csswg.org/css-grid-2/#grid-template-areas-property) property.
/// none | <string>+
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum GridTemplateAreas {
  /// No named grid areas.
  None,
  /// Defines the list of named grid areas.
  Areas {
    /// The number of columns in the grid.
    columns: u32,
    /// A flattened list of grid area names.
    /// Unnamed areas specified by the `.` token are represented as `None`.
    areas: Vec<Option<String>>,
  },
}

impl<'i> Parse<'i> for GridTemplateAreas {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(GridTemplateAreas::None);
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
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      }

      row += 1;
    }

    Ok(GridTemplateAreas::Areas { columns, areas: tokens })
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
          return Err(());
        }
        break;
      }

      column += 1;

      if rest.starts_with('.') {
        string = &rest[rest.find(|c| c != '.').unwrap_or(rest.len())..];
        tokens.push(None);
        continue;
      }

      if !rest.starts_with(is_name_code_point) {
        return Err(());
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
  c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '\u{80}' || c == '_' || c >= '0' && c <= '9' || c == '-'
}

impl ToCss for GridTemplateAreas {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
  fn write_string<'a, W>(
    &self,
    dest: &mut Printer<W>,
    iter: &mut std::slice::Iter<'a, Option<String>>,
    next: &mut Option<&'a Option<String>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let columns = match self {
      GridTemplateAreas::Areas { columns, .. } => *columns,
      _ => unreachable!(),
    };

    dest.write_char('"')?;

    let mut last_was_null = false;
    for i in 0..columns {
      if let Some(token) = next {
        if let Some(string) = token {
          if i > 0 && (!last_was_null || !dest.minify) {
            dest.write_char(' ')?;
          }
          write_ident(string, dest)?;
          last_was_null = false;
        } else {
          if i > 0 && (last_was_null || !dest.minify) {
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

/// A value for the [grid-template](https://drafts.csswg.org/css-grid-2/#explicit-grid-shorthand) shorthand property.
///
/// none | [ <'grid-template-rows'> / <'grid-template-columns'> ] | [ <line-names>? <string> <track-size>? <line-names>? ]+ [ / <explicit-track-list> ]?
///
/// If `areas` is not `None`, then `rows` must also not be `None`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct GridTemplate<'i> {
  /// The grid template rows.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub rows: TrackSizing<'i>,
  /// The grid template columns.
  pub columns: TrackSizing<'i>,
  /// The named grid areas.
  pub areas: GridTemplateAreas,
}

impl<'i> Parse<'i> for GridTemplate<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      input.expect_exhausted()?;
      return Ok(GridTemplate {
        rows: TrackSizing::None,
        columns: TrackSizing::None,
        areas: GridTemplateAreas::None,
      });
    }

    let start = input.state();
    let mut line_names: Vec<CustomIdentList<'i>> = Vec::new();
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
          .map_err(|()| input.new_custom_error(ParserError::InvalidDeclaration))?;

        if row == 0 {
          columns = parsed_columns;
        } else if parsed_columns != columns {
          return Err(input.new_custom_error(ParserError::InvalidDeclaration));
        }

        row += 1;

        let track_size = input.try_parse(TrackSize::parse).unwrap_or_default();
        items.push(TrackListItem::TrackSize(track_size));

        let last_names = input.try_parse(parse_line_names).unwrap_or_default();
        line_names.push(last_names);
      } else {
        break;
      }
    }

    if !tokens.is_empty() {
      if line_names.len() == items.len() {
        line_names.push(Default::default());
      }

      let areas = GridTemplateAreas::Areas { columns, areas: tokens };
      let rows = TrackSizing::TrackList(TrackList { line_names, items });
      let columns = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
        let list = TrackList::parse(input)?;
        if !list.is_explicit() {
          return Err(input.new_custom_error(ParserError::InvalidDeclaration));
        }
        TrackSizing::TrackList(list)
      } else {
        TrackSizing::None
      };
      Ok(GridTemplate { rows, columns, areas })
    } else {
      input.reset(&start);
      let rows = TrackSizing::parse(input)?;
      input.expect_delim('/')?;
      let columns = TrackSizing::parse(input)?;
      Ok(GridTemplate {
        rows,
        columns,
        areas: GridTemplateAreas::None,
      })
    }
  }
}

impl ToCss for GridTemplate<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.to_css_with_indent(dest, 15)
  }
}

impl GridTemplate<'_> {
  fn to_css_with_indent<W>(&self, dest: &mut Printer<W>, indent: u8) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match &self.areas {
      GridTemplateAreas::None => {
        if self.rows == TrackSizing::None && self.columns == TrackSizing::None {
          dest.write_str("none")?;
        } else {
          self.rows.to_css(dest)?;
          dest.delim('/', true)?;
          self.columns.to_css(dest)?;
        }
      }
      GridTemplateAreas::Areas { areas, .. } => {
        let track_list = match &self.rows {
          TrackSizing::TrackList(list) => list,
          _ => unreachable!(),
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
                  dest.indent_by(indent);
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
            } else if !first {
              newline!();
            }
          } else if !first {
            newline!();
          }

          self.areas.write_string(dest, &mut areas_iter, &mut next)?;

          if let Some(item) = items_iter.next() {
            if *item != TrackListItem::TrackSize(TrackSize::default()) {
              dest.whitespace()?;
              match item {
                TrackListItem::TrackSize(size) => size.to_css(dest)?,
                _ => unreachable!(),
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
          dest.dedent_by(indent);
        }
      }
    }

    Ok(())
  }
}

impl<'i> GridTemplate<'i> {
  #[inline]
  fn is_valid(rows: &TrackSizing, columns: &TrackSizing, areas: &GridTemplateAreas) -> bool {
    // The `grid-template` shorthand supports only explicit track values (i.e. no `repeat()`)
    // combined with grid-template-areas. If there are no areas, then any track values are allowed.
    *areas == GridTemplateAreas::None
      || (*rows != TrackSizing::None && rows.is_explicit() && columns.is_explicit())
  }
}

impl_shorthand! {
  GridTemplate(GridTemplate<'i>) {
    rows: [GridTemplateRows],
    columns: [GridTemplateColumns],
    areas: [GridTemplateAreas],
  }

  fn is_valid(shorthand) {
    GridTemplate::is_valid(&shorthand.rows, &shorthand.columns, &shorthand.areas)
  }
}

bitflags! {
  /// A value for the [grid-auto-flow](https://drafts.csswg.org/css-grid-2/#grid-auto-flow-property) property.
  ///
  /// [ row | column ] || dense
  ///
  /// The `Row` or `Column` flags may be combined with the `Dense` flag, but the `Row` and `Column` flags may
  /// not be combined.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "SerializedGridAutoFlow", into = "SerializedGridAutoFlow"))]
  #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct GridAutoFlow: u8 {
    /// The auto-placement algorithm places items by filling each row, adding new rows as necessary.
    const Row    = 0b00;
    /// The auto-placement algorithm places items by filling each column, adding new columns as necessary.
    const Column = 0b01;
    /// If specified, a dense packing algorithm is used, which fills in holes in the grid.
    const Dense  = 0b10;
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
struct SerializedGridAutoFlow {
  /// The direction of the auto flow.
  direction: AutoFlowDirection,
  /// If specified, a dense packing algorithm is used, which fills in holes in the grid.
  dense: bool,
}

impl From<GridAutoFlow> for SerializedGridAutoFlow {
  fn from(flow: GridAutoFlow) -> Self {
    Self {
      direction: if flow.contains(GridAutoFlow::Column) {
        AutoFlowDirection::Column
      } else {
        AutoFlowDirection::Row
      },
      dense: flow.contains(GridAutoFlow::Dense),
    }
  }
}

impl From<SerializedGridAutoFlow> for GridAutoFlow {
  fn from(s: SerializedGridAutoFlow) -> GridAutoFlow {
    let mut flow = match s.direction {
      AutoFlowDirection::Row => GridAutoFlow::Row,
      AutoFlowDirection::Column => GridAutoFlow::Column,
    };
    if s.dense {
      flow |= GridAutoFlow::Dense
    }

    flow
  }
}

#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "lowercase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum AutoFlowDirection {
  Row,
  Column,
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for GridAutoFlow {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    SerializedGridAutoFlow::json_schema(gen)
  }

  fn schema_name() -> String {
    "GridAutoFlow".into()
  }
}

impl Default for GridAutoFlow {
  fn default() -> GridAutoFlow {
    GridAutoFlow::Row
  }
}

impl GridAutoFlow {
  fn direction(self) -> GridAutoFlow {
    self & GridAutoFlow::Column
  }
}

impl<'i> Parse<'i> for GridAutoFlow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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

/// A value for the [grid](https://drafts.csswg.org/css-grid-2/#grid-shorthand) shorthand property.
///
/// <'grid-template'> | <'grid-template-rows'> / [ auto-flow && dense? ] <'grid-auto-columns'>? | [ auto-flow && dense? ] <'grid-auto-rows'>? / <'grid-template-columns'>
///
/// Explicit and implicit values may not be combined.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Grid<'i> {
  /// Explicit grid template rows.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub rows: TrackSizing<'i>,
  /// Explicit grid template columns.
  pub columns: TrackSizing<'i>,
  /// Explicit grid template areas.
  pub areas: GridTemplateAreas,
  /// The grid auto rows.
  pub auto_rows: TrackSizeList,
  /// The grid auto columns.
  pub auto_columns: TrackSizeList,
  /// The grid auto flow.
  pub auto_flow: GridAutoFlow,
}

impl<'i> Parse<'i> for Grid<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // <'grid-template'>
    if let Ok(template) = input.try_parse(GridTemplate::parse) {
      Ok(Grid {
        rows: template.rows,
        columns: template.columns,
        areas: template.areas,
        auto_rows: TrackSizeList::default(),
        auto_columns: TrackSizeList::default(),
        auto_flow: GridAutoFlow::default(),
      })

    // <'grid-template-rows'> / [ auto-flow && dense? ] <'grid-auto-columns'>?
    } else if let Ok(rows) = input.try_parse(TrackSizing::parse) {
      input.expect_delim('/')?;
      let auto_flow = parse_grid_auto_flow(input, GridAutoFlow::Column)?;
      let auto_columns = TrackSizeList::parse(input).unwrap_or_default();
      Ok(Grid {
        rows,
        columns: TrackSizing::None,
        areas: GridTemplateAreas::None,
        auto_rows: TrackSizeList::default(),
        auto_columns,
        auto_flow,
      })

    // [ auto-flow && dense? ] <'grid-auto-rows'>? / <'grid-template-columns'>
    } else {
      let auto_flow = parse_grid_auto_flow(input, GridAutoFlow::Row)?;
      let auto_rows = input.try_parse(TrackSizeList::parse).unwrap_or_default();
      input.expect_delim('/')?;
      let columns = TrackSizing::parse(input)?;
      Ok(Grid {
        rows: TrackSizing::None,
        columns,
        areas: GridTemplateAreas::None,
        auto_rows,
        auto_columns: TrackSizeList::default(),
        auto_flow,
      })
    }
  }
}

fn parse_grid_auto_flow<'i, 't>(
  input: &mut Parser<'i, 't>,
  flow: GridAutoFlow,
) -> Result<GridAutoFlow, ParseError<'i, ParserError<'i>>> {
  if input.try_parse(|input| input.expect_ident_matching("auto-flow")).is_ok() {
    if input.try_parse(|input| input.expect_ident_matching("dense")).is_ok() {
      Ok(flow | GridAutoFlow::Dense)
    } else {
      Ok(flow)
    }
  } else if input.try_parse(|input| input.expect_ident_matching("dense")).is_ok() {
    input.expect_ident_matching("auto-flow")?;
    Ok(flow | GridAutoFlow::Dense)
  } else {
    Err(input.new_error_for_next_token())
  }
}

impl ToCss for Grid<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let is_auto_initial = self.auto_rows == TrackSizeList::default()
      && self.auto_columns == TrackSizeList::default()
      && self.auto_flow == GridAutoFlow::default();

    // Handle the case where areas is set but rows is None (auto-flow syntax).
    // In this case, output "auto-flow / columns" format.
    if self.areas != GridTemplateAreas::None && self.rows == TrackSizing::None {
      dest.write_str("auto-flow")?;
      if self.auto_flow.contains(GridAutoFlow::Dense) {
        dest.write_str(" dense")?;
      }
      if self.auto_rows != TrackSizeList::default() {
        dest.write_char(' ')?;
        self.auto_rows.to_css(dest)?;
      }
      dest.delim('/', true)?;
      self.columns.to_css(dest)?;
      return Ok(());
    }

    // Handle the case where areas is set but columns is None (auto-flow column syntax).
    // In this case, output "rows / auto-flow" format.
    if self.areas != GridTemplateAreas::None
      && self.columns == TrackSizing::None
      && self.auto_flow.direction() == GridAutoFlow::Column
    {
      self.rows.to_css(dest)?;
      dest.delim('/', true)?;
      dest.write_str("auto-flow")?;
      if self.auto_flow.contains(GridAutoFlow::Dense) {
        dest.write_str(" dense")?;
      }
      if self.auto_columns != TrackSizeList::default() {
        dest.write_char(' ')?;
        self.auto_columns.to_css(dest)?;
      }
      return Ok(());
    }

    if self.areas != GridTemplateAreas::None
      || (self.rows != TrackSizing::None && self.columns != TrackSizing::None)
      || (self.areas == GridTemplateAreas::None && is_auto_initial)
    {
      if !is_auto_initial {
        unreachable!("invalid grid shorthand: mixed implicit and explicit values");
      }
      let template = GridTemplate {
        rows: self.rows.clone(),
        columns: self.columns.clone(),
        areas: self.areas.clone(),
      };
      template.to_css_with_indent(dest, 6)?;
    } else if self.auto_flow.direction() == GridAutoFlow::Column {
      if self.columns != TrackSizing::None || self.auto_rows != TrackSizeList::default() {
        unreachable!("invalid grid shorthand: mixed implicit and explicit values");
      }
      self.rows.to_css(dest)?;
      dest.delim('/', true)?;
      dest.write_str("auto-flow")?;
      if self.auto_flow.contains(GridAutoFlow::Dense) {
        dest.write_str(" dense")?;
      }
      if self.auto_columns != TrackSizeList::default() {
        dest.write_char(' ')?;
        self.auto_columns.to_css(dest)?;
      }
    } else {
      if self.rows != TrackSizing::None || self.auto_columns != TrackSizeList::default() {
        unreachable!("invalid grid shorthand: mixed implicit and explicit values");
      }
      dest.write_str("auto-flow")?;
      if self.auto_flow.contains(GridAutoFlow::Dense) {
        dest.write_str(" dense")?;
      }
      if self.auto_rows != TrackSizeList::default() {
        dest.write_char(' ')?;
        self.auto_rows.to_css(dest)?;
      }
      dest.delim('/', true)?;
      self.columns.to_css(dest)?;
    }

    Ok(())
  }
}

impl<'i> Grid<'i> {
  #[inline]
  fn is_valid(
    rows: &TrackSizing,
    columns: &TrackSizing,
    areas: &GridTemplateAreas,
    auto_rows: &TrackSizeList,
    auto_columns: &TrackSizeList,
    auto_flow: &GridAutoFlow,
  ) -> bool {
    let default_track_size_list = TrackSizeList::default();

    // When areas is set but rows is None (auto-flow syntax like "grid: auto-flow / 1fr"),
    // we can output the auto-flow shorthand along with "grid-template-areas" separately.
    // ⚠️ The case of `grid: 1fr / auto-flow` does not require such handling.
    if *areas != GridTemplateAreas::None && *rows == TrackSizing::None {
      return auto_flow.direction() == GridAutoFlow::Row;
    }

    // The `grid` shorthand can either be fully explicit (e.g. same as `grid-template`),
    // or explicit along a single axis. If there are auto rows, then there cannot be explicit rows, for example.
    let is_template = GridTemplate::is_valid(rows, columns, areas);
    let is_explicit = *auto_rows == default_track_size_list
      && *auto_columns == default_track_size_list
      && *auto_flow == GridAutoFlow::default();
    // grid-auto-flow: row shorthand syntax:
    // [ auto-flow && dense? ] <'grid-auto-rows'>? / <'grid-template-columns'>
    let is_auto_rows = auto_flow.direction() == GridAutoFlow::Row
      && *rows == TrackSizing::None
      && *auto_columns == default_track_size_list;
    // grid-auto-flow: column shorthand syntax:
    // <'grid-template-rows'> / [ auto-flow && dense? ] <'grid-auto-columns'>?
    let is_auto_columns = auto_flow.direction() == GridAutoFlow::Column
      && *columns == TrackSizing::None
      && *auto_rows == default_track_size_list;

    (is_template && is_explicit) || is_auto_rows || is_auto_columns
  }
}

// TODO: shorthand `grid: auto-flow 1fr / 100px` https://drafts.csswg.org/css-grid/#example-dec34e0f
impl_shorthand! {
  Grid(Grid<'i>) {
    rows: [GridTemplateRows],
    columns: [GridTemplateColumns],
    areas: [GridTemplateAreas],
    auto_rows: [GridAutoRows],
    auto_columns: [GridAutoColumns],
    auto_flow: [GridAutoFlow],
  }

  fn is_valid(grid) {
    Grid::is_valid(&grid.rows, &grid.columns, &grid.areas, &grid.auto_rows, &grid.auto_columns, &grid.auto_flow)
  }
}

/// A [`<grid-line>`](https://drafts.csswg.org/css-grid-2/#typedef-grid-row-start-grid-line) value,
/// used in the `grid-row-start`, `grid-row-end`, `grid-column-start`, and `grid-column-end` properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum GridLine<'i> {
  /// Automatic placement.
  Auto,
  /// A named grid area name (automatically postfixed by `-start` or `-end`), or and explicit grid line name.
  Area {
    /// A grid area name.
    name: CustomIdent<'i>,
  },
  /// The Nth grid line, optionally filtered by line name. Negative numbers count backwards from the end.
  Line {
    /// A line number.
    index: CSSInteger,
    /// A line name to filter by.
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: Option<CustomIdent<'i>>,
  },
  /// A grid span based on the Nth grid line from the opposite edge, optionally filtered by line name.
  Span {
    /// A line number.
    index: CSSInteger,
    /// A line name to filter by.
    name: Option<CustomIdent<'i>>,
  },
}

impl<'i> Parse<'i> for GridLine<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(GridLine::Auto);
    }

    if input.try_parse(|input| input.expect_ident_matching("span")).is_ok() {
      // TODO: is calc() supported here??
      let (index, name) = if let Ok(line_number) = input.try_parse(CSSInteger::parse) {
        let ident = input.try_parse(CustomIdent::parse).ok();
        (line_number, ident)
      } else if let Ok(ident) = input.try_parse(CustomIdent::parse) {
        let line_number = input.try_parse(CSSInteger::parse).unwrap_or(1);
        (line_number, Some(ident))
      } else {
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      };

      if index == 0 {
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      }

      return Ok(GridLine::Span { index, name });
    }

    if let Ok(index) = input.try_parse(CSSInteger::parse) {
      if index == 0 {
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      }
      let name = input.try_parse(CustomIdent::parse).ok();
      return Ok(GridLine::Line { index, name });
    }

    let name = CustomIdent::parse(input)?;
    if let Ok(index) = input.try_parse(CSSInteger::parse) {
      if index == 0 {
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      }
      return Ok(GridLine::Line {
        index,
        name: Some(name),
      });
    }

    Ok(GridLine::Area { name })
  }
}

impl ToCss for GridLine<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      GridLine::Auto => dest.write_str("auto"),
      GridLine::Area { name } => write_ident(&name.0, dest),
      GridLine::Line { index, name } => {
        index.to_css(dest)?;
        if let Some(id) = name {
          dest.write_char(' ')?;
          write_ident(&id.0, dest)?;
        }
        Ok(())
      }
      GridLine::Span { index, name } => {
        dest.write_str("span ")?;
        if *index != 1 || name.is_none() {
          index.to_css(dest)?;
          if name.is_some() {
            dest.write_char(' ')?;
          }
        }

        if let Some(id) = name {
          write_ident(&id.0, dest)?;
        }
        Ok(())
      }
    }
  }
}

impl<'i> GridLine<'i> {
  fn default_end_value(&self) -> GridLine<'i> {
    if matches!(self, GridLine::Area { .. }) {
      self.clone()
    } else {
      GridLine::Auto
    }
  }

  fn can_omit_end(&self, end: &GridLine) -> bool {
    if let GridLine::Area { name: start_id } = &self {
      matches!(end, GridLine::Area { name: end_id } if end_id == start_id)
    } else if matches!(end, GridLine::Auto) {
      true
    } else {
      false
    }
  }
}

macro_rules! impl_grid_placement {
  ($name: ident) => {
    impl<'i> Parse<'i> for $name<'i> {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let start = GridLine::parse(input)?;
        let end = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
          GridLine::parse(input)?
        } else {
          start.default_end_value()
        };

        Ok($name { start, end })
      }
    }

    impl ToCss for $name<'_> {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
      where
        W: std::fmt::Write,
      {
        self.start.to_css(dest)?;

        if !self.start.can_omit_end(&self.end) {
          dest.delim('/', true)?;
          self.end.to_css(dest)?;
        }
        Ok(())
      }
    }
  };
}

define_shorthand! {
  /// A value for the [grid-row](https://drafts.csswg.org/css-grid-2/#propdef-grid-row) shorthand property.
  /// <grid-line> [ / <grid-line> ]?
  pub struct GridRow<'i> {
    /// The starting line.
    #[cfg_attr(feature = "serde", serde(borrow))]
    start: GridRowStart(GridLine<'i>),
    /// The ending line.
    end: GridRowEnd(GridLine<'i>),
  }
}

define_shorthand! {
  /// A value for the [grid-column](https://drafts.csswg.org/css-grid-2/#propdef-grid-column) shorthand property.
  /// <grid-line> [ / <grid-line> ]?
  pub struct GridColumn<'i> {
    /// The starting line.
    #[cfg_attr(feature = "serde", serde(borrow))]
    start: GridColumnStart(GridLine<'i>),
    /// The ending line.
    end: GridColumnEnd(GridLine<'i>),
  }
}

impl_grid_placement!(GridRow);
impl_grid_placement!(GridColumn);

define_shorthand! {
  /// A value for the [grid-area](https://drafts.csswg.org/css-grid-2/#propdef-grid-area) shorthand property.
  /// <grid-line> [ / <grid-line> ]{0,3}
  pub struct GridArea<'i> {
    /// The grid row start placement.
    #[cfg_attr(feature = "serde", serde(borrow))]
    row_start: GridRowStart(GridLine<'i>),
    /// The grid column start placement.
    column_start: GridColumnStart(GridLine<'i>),
    /// The grid row end placement.
    row_end: GridRowEnd(GridLine<'i>),
    /// The grid column end placement.
    column_end: GridColumnEnd(GridLine<'i>),
  }
}

impl<'i> Parse<'i> for GridArea<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let row_start = GridLine::parse(input)?;
    let column_start = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      GridLine::parse(input)?
    } else {
      let opposite = row_start.default_end_value();
      return Ok(GridArea {
        row_start,
        column_start: opposite.clone(),
        row_end: opposite.clone(),
        column_end: opposite,
      });
    };

    let row_end = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      GridLine::parse(input)?
    } else {
      let row_end = row_start.default_end_value();
      let column_end = column_start.default_end_value();
      return Ok(GridArea {
        row_start,
        column_start,
        row_end,
        column_end,
      });
    };

    let column_end = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      GridLine::parse(input)?
    } else {
      let column_end = column_start.default_end_value();
      return Ok(GridArea {
        row_start,
        column_start,
        row_end,
        column_end,
      });
    };

    Ok(GridArea {
      row_start,
      column_start,
      row_end,
      column_end,
    })
  }
}

impl ToCss for GridArea<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.row_start.to_css(dest)?;

    let can_omit_column_end = self.column_start.can_omit_end(&self.column_end);
    let can_omit_row_end = can_omit_column_end && self.row_start.can_omit_end(&self.row_end);
    let can_omit_column_start = can_omit_row_end && self.row_start.can_omit_end(&self.column_start);

    if !can_omit_column_start {
      dest.delim('/', true)?;
      self.column_start.to_css(dest)?;
    }

    if !can_omit_row_end {
      dest.delim('/', true)?;
      self.row_end.to_css(dest)?;
    }

    if !can_omit_column_end {
      dest.delim('/', true)?;
      self.column_end.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Default, Debug)]
pub(crate) struct GridHandler<'i> {
  rows: Option<TrackSizing<'i>>,
  columns: Option<TrackSizing<'i>>,
  areas: Option<GridTemplateAreas>,
  auto_rows: Option<TrackSizeList>,
  auto_columns: Option<TrackSizeList>,
  auto_flow: Option<GridAutoFlow>,
  row_start: Option<GridLine<'i>>,
  column_start: Option<GridLine<'i>>,
  row_end: Option<GridLine<'i>>,
  column_end: Option<GridLine<'i>>,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for GridHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    match property {
      GridTemplateColumns(columns) => self.columns = Some(columns.clone()),
      GridTemplateRows(rows) => self.rows = Some(rows.clone()),
      GridTemplateAreas(areas) => self.areas = Some(areas.clone()),
      GridAutoColumns(auto_columns) => self.auto_columns = Some(auto_columns.clone()),
      GridAutoRows(auto_rows) => self.auto_rows = Some(auto_rows.clone()),
      GridAutoFlow(auto_flow) => self.auto_flow = Some(auto_flow.clone()),
      GridTemplate(template) => {
        self.rows = Some(template.rows.clone());
        self.columns = Some(template.columns.clone());
        self.areas = Some(template.areas.clone());
      }
      Grid(grid) => {
        self.rows = Some(grid.rows.clone());
        self.columns = Some(grid.columns.clone());
        self.areas = Some(grid.areas.clone());
        self.auto_rows = Some(grid.auto_rows.clone());
        self.auto_columns = Some(grid.auto_columns.clone());
        self.auto_flow = Some(grid.auto_flow.clone());
      }
      GridRowStart(row_start) => self.row_start = Some(row_start.clone()),
      GridRowEnd(row_end) => self.row_end = Some(row_end.clone()),
      GridColumnStart(column_start) => self.column_start = Some(column_start.clone()),
      GridColumnEnd(column_end) => self.column_end = Some(column_end.clone()),
      GridRow(row) => {
        self.row_start = Some(row.start.clone());
        self.row_end = Some(row.end.clone());
      }
      GridColumn(column) => {
        self.column_start = Some(column.start.clone());
        self.column_end = Some(column.end.clone());
      }
      GridArea(area) => {
        self.row_start = Some(area.row_start.clone());
        self.row_end = Some(area.row_end.clone());
        self.column_start = Some(area.column_start.clone());
        self.column_end = Some(area.column_end.clone());
      }
      Unparsed(val) if is_grid_property(&val.property_id) => {
        self.finalize(dest, context);
        dest.push(property.clone());
      }
      _ => return false,
    }

    self.has_any = true;
    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, _: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    let mut rows = std::mem::take(&mut self.rows);
    let mut columns = std::mem::take(&mut self.columns);
    let mut areas = std::mem::take(&mut self.areas);
    let mut auto_rows = std::mem::take(&mut self.auto_rows);
    let mut auto_columns = std::mem::take(&mut self.auto_columns);
    let mut auto_flow = std::mem::take(&mut self.auto_flow);
    let mut row_start = std::mem::take(&mut self.row_start);
    let mut row_end = std::mem::take(&mut self.row_end);
    let mut column_start = std::mem::take(&mut self.column_start);
    let mut column_end = std::mem::take(&mut self.column_end);

    if let (Some(rows_val), Some(columns_val), Some(areas_val)) = (&rows, &columns, &areas) {
      let mut has_template = true;
      if let (Some(auto_rows_val), Some(auto_columns_val), Some(auto_flow_val)) =
        (&auto_rows, &auto_columns, &auto_flow)
      {
        // The `grid` shorthand can either be fully explicit (e.g. same as `grid-template`),
        // or explicit along a single axis. If there are auto rows, then there cannot be explicit rows, for example.
        if Grid::is_valid(
          rows_val,
          columns_val,
          areas_val,
          auto_rows_val,
          auto_columns_val,
          auto_flow_val,
        ) {
          let needs_separate_areas = *areas_val != GridTemplateAreas::None
            && ((*rows_val == TrackSizing::None && auto_flow_val.direction() == GridAutoFlow::Row)
              || (*columns_val == TrackSizing::None && auto_flow_val.direction() == GridAutoFlow::Column));

          // Pad areas with "." for missing rows. But don't pad if we're using auto-flow syntax,
          // because grid-template-areas should remain as-is in that case.
          // Use tuple to avoid double cloning when needs_separate_areas is true.
          let (areas_for_grid, areas_for_output) = if needs_separate_areas {
            // Take the original areas directly to avoid cloning when needs_separate_areas is true
            (areas_val.clone(), Some(areas_val.clone()))
          } else {
            (GridHandler::pad_grid_template_areas(rows_val, areas_val.clone()), None)
          };

          dest.push(Property::Grid(Grid {
            rows: rows_val.clone(),
            columns: columns_val.clone(),
            areas: areas_for_grid,
            auto_rows: auto_rows_val.clone(),
            auto_columns: auto_columns_val.clone(),
            auto_flow: auto_flow_val.clone(),
          }));

          has_template = false;
          auto_rows = None;
          auto_columns = None;
          auto_flow = None;

          // When areas is set but rows/columns is None (auto-flow syntax), also output
          // grid-template-areas separately since grid shorthand can't represent this combination.
          if let Some(areas) = areas_for_output {
            dest.push(Property::GridTemplateAreas(areas));
          }
        }
      }

      // The `grid-template` shorthand supports only explicit track values (i.e. no `repeat()`)
      // combined with grid-template-areas. If there are no areas, then any track values are allowed.
      if has_template && GridTemplate::is_valid(rows_val, columns_val, areas_val) {
        // Pad areas with "." for missing rows
        let padded_areas = GridHandler::pad_grid_template_areas(rows_val, areas_val.clone());

        dest.push(Property::GridTemplate(GridTemplate {
          rows: rows_val.clone(),
          columns: columns_val.clone(),
          areas: padded_areas,
        }));

        has_template = false;
      }

      if !has_template {
        rows = None;
        columns = None;
        areas = None;
      }
    }

    if row_start.is_some() && row_end.is_some() && column_start.is_some() && column_end.is_some() {
      dest.push(Property::GridArea(GridArea {
        row_start: std::mem::take(&mut row_start).unwrap(),
        row_end: std::mem::take(&mut row_end).unwrap(),
        column_start: std::mem::take(&mut column_start).unwrap(),
        column_end: std::mem::take(&mut column_end).unwrap(),
      }))
    } else {
      if row_start.is_some() && row_end.is_some() {
        dest.push(Property::GridRow(GridRow {
          start: std::mem::take(&mut row_start).unwrap(),
          end: std::mem::take(&mut row_end).unwrap(),
        }))
      }

      if column_start.is_some() && column_end.is_some() {
        dest.push(Property::GridColumn(GridColumn {
          start: std::mem::take(&mut column_start).unwrap(),
          end: std::mem::take(&mut column_end).unwrap(),
        }))
      }
    }

    macro_rules! single_property {
      ($prop: ident, $key: ident) => {
        if let Some(val) = $key {
          dest.push(Property::$prop(val))
        }
      };
    }

    single_property!(GridTemplateRows, rows);
    single_property!(GridTemplateColumns, columns);
    single_property!(GridTemplateAreas, areas);
    single_property!(GridAutoRows, auto_rows);
    single_property!(GridAutoColumns, auto_columns);
    single_property!(GridAutoFlow, auto_flow);
    single_property!(GridRowStart, row_start);
    single_property!(GridRowEnd, row_end);
    single_property!(GridColumnStart, column_start);
    single_property!(GridColumnEnd, column_end);
  }
}

/// Pads grid template areas with "." (None) for missing rows.
/// All the remaining unnamed areas in a grid can be referred using null cell
/// tokens. A null cell token is a sequence of one or more . (U+002E FULL STOP)
/// characters, e.g., ., ..., or ..... etc. A null cell token can be used to
/// create empty spaces in the grid.
/// Spec: https://drafts.csswg.org/css-grid/#ref-for-string-value①
impl GridHandler<'_> {
  fn pad_grid_template_areas(rows: &TrackSizing, areas: GridTemplateAreas) -> GridTemplateAreas {
    match (rows, areas) {
      (TrackSizing::TrackList(rows_list), GridTemplateAreas::Areas { columns, areas }) => {
        let rows_count = rows_list.items.len();
        let areas_rows_count = areas.len() / columns as usize;
        if areas_rows_count < rows_count {
          let mut padded_areas = areas;
          // Fill each missing row with "." (represented as None)
          for _ in areas_rows_count..rows_count {
            for _ in 0..columns {
              padded_areas.push(None);
            }
          }
          GridTemplateAreas::Areas {
            columns,
            areas: padded_areas,
          }
        } else {
          GridTemplateAreas::Areas { columns, areas }
        }
      }
      (_, areas) => areas,
    }
  }
}

#[inline]
fn is_grid_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::GridTemplateColumns
    | PropertyId::GridTemplateRows
    | PropertyId::GridTemplateAreas
    | PropertyId::GridAutoColumns
    | PropertyId::GridAutoRows
    | PropertyId::GridAutoFlow
    | PropertyId::GridTemplate
    | PropertyId::Grid
    | PropertyId::GridRowStart
    | PropertyId::GridRowEnd
    | PropertyId::GridColumnStart
    | PropertyId::GridColumnEnd
    | PropertyId::GridRow
    | PropertyId::GridColumn
    | PropertyId::GridArea => true,
    _ => false,
  }
}
