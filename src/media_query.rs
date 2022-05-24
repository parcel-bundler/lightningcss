//! Media queries.

use crate::compat::Feature;
use crate::error::{ErrorWithLocation, MinifyError, MinifyErrorKind, ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::rules::custom_media::CustomMediaRule;
use crate::rules::Location;
use crate::traits::{Parse, ToCss};
use crate::values::number::CSSNumber;
use crate::values::string::CowArcStr;
use crate::values::{length::Length, ratio::Ratio, resolution::Resolution};
use cssparser::*;
use std::collections::{HashMap, HashSet};

/// A [media query list](https://drafts.csswg.org/mediaqueries/#mq-list).
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MediaList<'i> {
  /// The list of media queries.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub media_queries: Vec<MediaQuery<'i>>,
}

impl<'i> MediaList<'i> {
  /// Creates an empty media query list.
  pub fn new() -> Self {
    MediaList { media_queries: vec![] }
  }

  /// Parse a media query list from CSS.
  pub fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut media_queries = vec![];
    loop {
      match input.parse_until_before(Delimiter::Comma, |i| MediaQuery::parse(i)) {
        Ok(mq) => {
          media_queries.push(mq);
        }
        Err(err) => match err.kind {
          ParseErrorKind::Basic(BasicParseErrorKind::EndOfInput) => break,
          _ => return Err(err),
        },
      }

      match input.next() {
        Ok(&Token::Comma) => {}
        Ok(_) => unreachable!(),
        Err(_) => break,
      }
    }

    Ok(MediaList { media_queries })
  }

  pub(crate) fn transform_custom_media(
    &mut self,
    loc: Location,
    custom_media: &HashMap<CowArcStr<'i>, CustomMediaRule<'i>>,
  ) -> Result<(), MinifyError> {
    for query in self.media_queries.iter_mut() {
      query.transform_custom_media(loc, custom_media)?;
    }
    Ok(())
  }

  /// Returns whether the media query list always matches.
  pub fn always_matches(&self) -> bool {
    // If the media list is empty, it always matches.
    self.media_queries.is_empty() || self.media_queries.iter().all(|mq| mq.always_matches())
  }

  /// Returns whether the media query list never matches.
  pub fn never_matches(&self) -> bool {
    !self.media_queries.is_empty() && self.media_queries.iter().all(|mq| mq.never_matches())
  }

  /// Attempts to combine the given media query list into this one. The resulting media query
  /// list matches if both the original media query lists would have matched.
  ///
  /// Returns an error if the boolean logic is not possible.
  pub fn and(&mut self, b: &MediaList<'i>) -> Result<(), ()> {
    if self.media_queries.is_empty() {
      self.media_queries.extend(b.media_queries.iter().cloned());
      return Ok(());
    }

    for b in &b.media_queries {
      if self.media_queries.contains(&b) {
        continue;
      }

      for a in &mut self.media_queries {
        a.and(&b)?;
      }
    }

    Ok(())
  }

  /// Combines the given media query list into this one. The resulting media query list
  /// matches if either of the original media query lists would have matched.
  pub fn or(&mut self, b: &MediaList<'i>) {
    for mq in &b.media_queries {
      if !self.media_queries.contains(&mq) {
        self.media_queries.push(mq.clone())
      }
    }
  }
}

impl<'i> ToCss for MediaList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.media_queries.is_empty() {
      dest.write_str("not all")?;
      return Ok(());
    }

    let mut first = true;
    for query in &self.media_queries {
      if !first {
        dest.delim(',', false)?;
      }
      first = false;
      query.to_css(dest)?;
    }
    Ok(())
  }
}

enum_property! {
  /// A [media query qualifier](https://drafts.csswg.org/mediaqueries/#mq-prefix).
  pub enum Qualifier {
    /// Prevents older browsers from matching the media query.
    Only,
    /// Negates a media query.
    Not,
  }
}

/// A [media type](https://drafts.csswg.org/mediaqueries/#media-types) within a media query.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MediaType<'i> {
  /// Matches all devices.
  All,
  /// Matches printers, and devices intended to reproduce a printed
  /// display, such as a web browser showing a document in “Print Preview”.
  Print,
  /// Matches all devices that aren’t matched by print.
  Screen,
  /// An unknown media type.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Custom(CowArcStr<'i>),
}

impl<'i> Parse<'i> for MediaType<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = input.expect_ident()?;
    match_ignore_ascii_case! { &*name,
      "all" => Ok(MediaType::All),
      "print" => Ok(MediaType::Print),
      "screen" => Ok(MediaType::Screen),
      _ => Ok(MediaType::Custom(name.into()))
    }
  }
}

/// A [media query](https://drafts.csswg.org/mediaqueries/#media).
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MediaQuery<'i> {
  /// The qualifier for this query.
  pub qualifier: Option<Qualifier>,
  /// The media type for this query, that can be known, unknown, or "all".
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub media_type: MediaType<'i>,
  /// The condition that this media query contains. This cannot have `or`
  /// in the first level.
  pub condition: Option<MediaCondition<'i>>,
}

impl<'i> MediaQuery<'i> {
  /// Parse a media query given css input.
  ///
  /// Returns an error if any of the expressions is unknown.
  pub fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (qualifier, explicit_media_type) = input
      .try_parse(|input| -> Result<_, ParseError<'i, ParserError<'i>>> {
        let qualifier = input.try_parse(Qualifier::parse).ok();
        let media_type = MediaType::parse(input)?;
        Ok((qualifier, Some(media_type)))
      })
      .unwrap_or_default();

    let condition = if explicit_media_type.is_none() {
      Some(MediaCondition::parse(input, true)?)
    } else if input.try_parse(|i| i.expect_ident_matching("and")).is_ok() {
      Some(MediaCondition::parse(input, false)?)
    } else {
      None
    };

    let media_type = explicit_media_type.unwrap_or(MediaType::All);
    Ok(Self {
      qualifier,
      media_type,
      condition,
    })
  }

  fn transform_custom_media(
    &mut self,
    loc: Location,
    custom_media: &HashMap<CowArcStr<'i>, CustomMediaRule<'i>>,
  ) -> Result<(), MinifyError> {
    if let Some(condition) = &mut self.condition {
      let used = process_condition(
        loc,
        custom_media,
        &mut self.media_type,
        &mut self.qualifier,
        condition,
        &mut HashSet::new(),
      )?;
      if !used {
        self.condition = None;
      }
    }
    Ok(())
  }

  /// Returns whether the media query is guaranteed to always match.
  pub fn always_matches(&self) -> bool {
    self.qualifier == None && self.media_type == MediaType::All && self.condition == None
  }

  /// Returns whether the media query is guaranteed to never match.
  pub fn never_matches(&self) -> bool {
    self.qualifier == Some(Qualifier::Not) && self.media_type == MediaType::All && self.condition == None
  }

  /// Attempts to combine the given media query into this one. The resulting media query
  /// matches if both of the original media queries would have matched.
  ///
  /// Returns an error if the boolean logic is not possible.
  pub fn and<'a>(&mut self, b: &MediaQuery<'i>) -> Result<(), ()> {
    let at = (&self.qualifier, &self.media_type);
    let bt = (&b.qualifier, &b.media_type);
    let (qualifier, media_type) = match (at, bt) {
      // `not all and screen` => not all
      // `screen and not all` => not all
      ((&Some(Qualifier::Not), &MediaType::All), _) |
      (_, (&Some(Qualifier::Not), &MediaType::All)) => (Some(Qualifier::Not), MediaType::All),
      // `not screen and not print` => ERROR
      // `not screen and not screen` => not screen
      ((&Some(Qualifier::Not), a), (&Some(Qualifier::Not), b)) => {
        if a == b {
          (Some(Qualifier::Not), a.clone())
        } else {
          return Err(())
        }
      },
      // `all and print` => print
      // `print and all` => print
      // `all and not print` => not print
      ((_, MediaType::All), (q, t)) |
      ((q, t), (_, MediaType::All)) |
      // `not screen and print` => print
      // `print and not screen` => print
      ((&Some(Qualifier::Not), _), (q, t)) |
      ((q, t), (&Some(Qualifier::Not), _)) => (q.clone(), t.clone()),
      // `print and screen` => not all
      ((_, a), (_, b)) if a != b => (Some(Qualifier::Not), MediaType::All),
      ((_, a), _) => (None, a.clone())
    };

    self.qualifier = qualifier;
    self.media_type = media_type;

    if let Some(cond) = &b.condition {
      self.condition = if let Some(condition) = &self.condition {
        if condition != cond {
          macro_rules! parenthesize {
            ($condition: ident) => {
              if matches!($condition, MediaCondition::Operation(_, Operator::Or)) {
                MediaCondition::InParens(Box::new($condition.clone()))
              } else {
                $condition.clone()
              }
            };
          }
          Some(MediaCondition::Operation(
            vec![parenthesize!(condition), parenthesize!(cond)],
            Operator::And,
          ))
        } else {
          Some(condition.clone())
        }
      } else {
        Some(cond.clone())
      }
    }

    Ok(())
  }
}

impl<'i> ToCss for MediaQuery<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if let Some(qual) = self.qualifier {
      qual.to_css(dest)?;
      dest.write_char(' ')?;
    }

    match self.media_type {
      MediaType::All => {
        // We need to print "all" if there's a qualifier, or there's
        // just an empty list of expressions.
        //
        // Otherwise, we'd serialize media queries like "(min-width:
        // 40px)" in "all (min-width: 40px)", which is unexpected.
        if self.qualifier.is_some() || self.condition.is_none() {
          dest.write_str("all")?;
        }
      }
      MediaType::Print => dest.write_str("print")?,
      MediaType::Screen => dest.write_str("screen")?,
      MediaType::Custom(ref desc) => dest.write_str(desc)?,
    }

    let condition = match self.condition {
      Some(ref c) => c,
      None => return Ok(()),
    };

    if self.media_type != MediaType::All || self.qualifier.is_some() {
      dest.write_str(" and ")?;
    }

    condition.to_css(dest)
  }
}

enum_property! {
  /// A binary `and` or `or` operator.
  pub enum Operator {
    /// The `and` operator.
    And,
    /// The `or` operator.
    Or,
  }
}

/// Represents a media condition.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MediaCondition<'i> {
  /// A media feature, implicitly parenthesized.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Feature(MediaFeature<'i>),
  /// A negation of a condition.
  Not(Box<MediaCondition<'i>>),
  /// A set of joint operations.
  Operation(Vec<MediaCondition<'i>>, Operator),
  /// A condition wrapped in parenthesis.
  InParens(Box<MediaCondition<'i>>),
}

impl<'i> MediaCondition<'i> {
  /// Parse a single media condition.
  pub fn parse<'t>(input: &mut Parser<'i, 't>, allow_or: bool) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let is_negation = match *input.next()? {
      Token::ParenthesisBlock => false,
      Token::Ident(ref ident) if ident.eq_ignore_ascii_case("not") => true,
      ref t => return Err(location.new_unexpected_token_error(t.clone())),
    };

    if is_negation {
      let inner_condition = Self::parse_in_parens(input)?;
      return Ok(MediaCondition::Not(Box::new(inner_condition)));
    }

    // ParenthesisBlock.
    let first_condition = Self::parse_paren_block(input)?;
    let operator = match input.try_parse(Operator::parse) {
      Ok(op) => op,
      Err(..) => return Ok(first_condition),
    };

    if !allow_or && operator == Operator::Or {
      return Err(location.new_custom_error(ParserError::InvalidMediaQuery));
    }

    let mut conditions = vec![];
    conditions.push(first_condition);
    conditions.push(Self::parse_in_parens(input)?);

    let delim = match operator {
      Operator::And => "and",
      Operator::Or => "or",
    };

    loop {
      if input.try_parse(|i| i.expect_ident_matching(delim)).is_err() {
        return Ok(MediaCondition::Operation(conditions, operator));
      }

      conditions.push(Self::parse_in_parens(input)?);
    }
  }

  /// Parse a media condition in parentheses.
  pub fn parse_in_parens<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.expect_parenthesis_block()?;
    Self::parse_paren_block(input)
  }

  fn parse_paren_block<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_nested_block(|input| {
      if let Ok(inner) = input.try_parse(|i| Self::parse(i, true)) {
        return Ok(MediaCondition::InParens(Box::new(inner)));
      }

      let feature = MediaFeature::parse(input)?;
      Ok(MediaCondition::Feature(feature))
    })
  }
}

impl<'i> ToCss for MediaCondition<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match *self {
      MediaCondition::Feature(ref f) => f.to_css(dest),
      MediaCondition::Not(ref c) => {
        dest.write_str("not ")?;
        c.to_css(dest)
      }
      MediaCondition::InParens(ref c) => {
        dest.write_char('(')?;
        c.to_css(dest)?;
        dest.write_char(')')
      }
      MediaCondition::Operation(ref list, op) => {
        let mut iter = list.iter();
        iter.next().unwrap().to_css(dest)?;
        for item in iter {
          dest.write_char(' ')?;
          op.to_css(dest)?;
          dest.write_char(' ')?;
          item.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

/// A [comparator](https://drafts.csswg.org/mediaqueries/#typedef-mf-comparison) within a media query.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MediaFeatureComparison {
  /// `=`
  Equal,
  /// `>`
  GreaterThan,
  /// `>=`
  GreaterThanEqual,
  /// `<`
  LessThan,
  /// `<=`
  LessThanEqual,
}

impl ToCss for MediaFeatureComparison {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use MediaFeatureComparison::*;
    match self {
      Equal => dest.delim('=', true),
      GreaterThan => dest.delim('>', true),
      GreaterThanEqual => {
        dest.whitespace()?;
        dest.write_str(">=")?;
        dest.whitespace()
      }
      LessThan => dest.delim('<', true),
      LessThanEqual => {
        dest.whitespace()?;
        dest.write_str("<=")?;
        dest.whitespace()
      }
    }
  }
}

impl MediaFeatureComparison {
  fn opposite(&self) -> MediaFeatureComparison {
    match self {
      MediaFeatureComparison::GreaterThan => MediaFeatureComparison::LessThan,
      MediaFeatureComparison::GreaterThanEqual => MediaFeatureComparison::LessThanEqual,
      MediaFeatureComparison::LessThan => MediaFeatureComparison::GreaterThan,
      MediaFeatureComparison::LessThanEqual => MediaFeatureComparison::GreaterThanEqual,
      MediaFeatureComparison::Equal => MediaFeatureComparison::Equal,
    }
  }
}

/// A [media feature](https://drafts.csswg.org/mediaqueries/#typedef-media-feature)
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MediaFeature<'i> {
  /// A plain media feature, e.g. `(min-width: 240px)`.
  Plain {
    /// The name of the feature.
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: CowArcStr<'i>,
    /// The feature value.
    value: MediaFeatureValue<'i>,
  },
  /// A boolean feature, e.g. `(hover)`.
  Boolean(CowArcStr<'i>),
  /// A range, e.g. `(width > 240px)`.
  Range {
    /// The name of the feature.
    name: CowArcStr<'i>,
    /// A comparator.
    operator: MediaFeatureComparison,
    /// The feature value.
    value: MediaFeatureValue<'i>,
  },
  /// An interval, e.g. `(120px < width < 240px)`.
  Interval {
    /// The name of the feature.
    name: CowArcStr<'i>,
    /// A start value.
    start: MediaFeatureValue<'i>,
    /// A comparator for the start value.
    start_operator: MediaFeatureComparison,
    /// The end value.
    end: MediaFeatureValue<'i>,
    /// A comparator for the end value.
    end_operator: MediaFeatureComparison,
  },
}

impl<'i> Parse<'i> for MediaFeature<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(res) = input.try_parse(Self::parse_name_first) {
      return Ok(res);
    }

    Self::parse_value_first(input)
  }
}

impl<'i> MediaFeature<'i> {
  fn parse_name_first<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = input.expect_ident()?.into();

    let operator = input.try_parse(|input| consume_operation_or_colon(input, true));
    let operator = match operator {
      Err(..) => return Ok(MediaFeature::Boolean(name)),
      Ok(operator) => operator,
    };

    let value = MediaFeatureValue::parse(input)?;

    if let Some(operator) = operator {
      Ok(MediaFeature::Range { name, operator, value })
    } else {
      Ok(MediaFeature::Plain { name, value })
    }
  }

  fn parse_value_first<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = MediaFeatureValue::parse(input)?;
    let operator = consume_operation_or_colon(input, false)?;
    let name = input.expect_ident()?.into();

    if let Ok(end_operator) = input.try_parse(|input| consume_operation_or_colon(input, false)) {
      let start_operator = operator.unwrap();
      let end_operator = end_operator.unwrap();
      // Start and end operators must be matching.
      match (start_operator, end_operator) {
        (MediaFeatureComparison::GreaterThan, MediaFeatureComparison::GreaterThan)
        | (MediaFeatureComparison::GreaterThan, MediaFeatureComparison::GreaterThanEqual)
        | (MediaFeatureComparison::GreaterThanEqual, MediaFeatureComparison::GreaterThanEqual)
        | (MediaFeatureComparison::GreaterThanEqual, MediaFeatureComparison::GreaterThan)
        | (MediaFeatureComparison::LessThan, MediaFeatureComparison::LessThan)
        | (MediaFeatureComparison::LessThan, MediaFeatureComparison::LessThanEqual)
        | (MediaFeatureComparison::LessThanEqual, MediaFeatureComparison::LessThanEqual)
        | (MediaFeatureComparison::LessThanEqual, MediaFeatureComparison::LessThan) => {}
        _ => return Err(input.new_custom_error(ParserError::InvalidMediaQuery)),
      };
      let end_value = MediaFeatureValue::parse(input)?;
      Ok(MediaFeature::Interval {
        name,
        start: value,
        start_operator,
        end: end_value,
        end_operator,
      })
    } else {
      let operator = operator.unwrap().opposite();
      Ok(MediaFeature::Range { name, operator, value })
    }
  }
}

impl<'i> ToCss for MediaFeature<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_char('(')?;

    match self {
      MediaFeature::Boolean(name) => {
        serialize_identifier(name, dest)?;
      }
      MediaFeature::Plain { name, value } => {
        serialize_identifier(name, dest)?;
        dest.delim(':', false)?;
        value.to_css(dest)?;
      }
      MediaFeature::Range { name, operator, value } => {
        // If range syntax is unsupported, use min/max prefix if possible.
        if let Some(targets) = dest.targets {
          if !Feature::MediaRangeSyntax.is_compatible(targets) {
            return write_min_max(operator, name, value, dest);
          }
        }

        serialize_identifier(name, dest)?;
        operator.to_css(dest)?;
        value.to_css(dest)?;
      }
      MediaFeature::Interval {
        name,
        start,
        start_operator,
        end,
        end_operator,
      } => {
        if let Some(targets) = dest.targets {
          if !Feature::MediaIntervalSyntax.is_compatible(targets) {
            write_min_max(&start_operator.opposite(), name, start, dest)?;
            dest.write_str(" and (")?;
            return write_min_max(end_operator, name, end, dest);
          }
        }

        start.to_css(dest)?;
        start_operator.to_css(dest)?;
        serialize_identifier(name, dest)?;
        end_operator.to_css(dest)?;
        end.to_css(dest)?;
      }
    }

    dest.write_char(')')
  }
}

#[inline]
fn write_min_max<W>(
  operator: &MediaFeatureComparison,
  name: &str,
  value: &MediaFeatureValue,
  dest: &mut Printer<W>,
) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  let prefix = match operator {
    MediaFeatureComparison::GreaterThan | MediaFeatureComparison::GreaterThanEqual => Some("min-"),
    MediaFeatureComparison::LessThan | MediaFeatureComparison::LessThanEqual => Some("max-"),
    MediaFeatureComparison::Equal => None,
  };

  if let Some(prefix) = prefix {
    dest.write_str(prefix)?;
  }

  serialize_identifier(name, dest)?;
  dest.delim(':', false)?;

  let adjusted = match operator {
    MediaFeatureComparison::GreaterThan => Some(value.clone() + 0.001),
    MediaFeatureComparison::LessThan => Some(value.clone() + -0.001),
    _ => None,
  };

  if let Some(value) = adjusted {
    value.to_css(dest)?;
  } else {
    value.to_css(dest)?;
  }

  dest.write_char(')')?;
  Ok(())
}

/// [media feature value](https://drafts.csswg.org/mediaqueries/#typedef-mf-value) within a media query.
///
/// See [MediaFeature](MediaFeature).
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MediaFeatureValue<'i> {
  /// A length value.
  Length(Length),
  /// A number value.
  Number(CSSNumber),
  /// A resolution.
  Resolution(Resolution),
  /// A ratio.
  Ratio(Ratio),
  /// An indentifier.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Ident(CowArcStr<'i>),
}

impl<'i> Parse<'i> for MediaFeatureValue<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Ratios are ambigous with numbers because the second param is optional (e.g. 2/1 == 2).
    // We require the / delimeter when parsing ratios so that 2/1 ends up as a ratio and 2 is
    // parsed as a number.
    if let Ok(ratio) = input.try_parse(Ratio::parse_required) {
      return Ok(MediaFeatureValue::Ratio(ratio));
    }

    // Parse number next so that unitless values are not parsed as lengths.
    if let Ok(num) = input.try_parse(CSSNumber::parse) {
      return Ok(MediaFeatureValue::Number(num));
    }

    if let Ok(length) = input.try_parse(Length::parse) {
      return Ok(MediaFeatureValue::Length(length));
    }

    if let Ok(res) = input.try_parse(Resolution::parse) {
      return Ok(MediaFeatureValue::Resolution(res));
    }

    let ident = input.expect_ident()?;
    Ok(MediaFeatureValue::Ident(ident.into()))
  }
}

impl<'i> ToCss for MediaFeatureValue<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      MediaFeatureValue::Length(len) => len.to_css(dest),
      MediaFeatureValue::Number(num) => num.to_css(dest),
      MediaFeatureValue::Resolution(res) => res.to_css(dest),
      MediaFeatureValue::Ratio(ratio) => ratio.to_css(dest),
      MediaFeatureValue::Ident(id) => {
        serialize_identifier(id, dest)?;
        Ok(())
      }
    }
  }
}

impl<'i> std::ops::Add<f32> for MediaFeatureValue<'i> {
  type Output = Self;

  fn add(self, other: f32) -> Self {
    match self {
      MediaFeatureValue::Length(len) => MediaFeatureValue::Length(len + Length::px(other)),
      MediaFeatureValue::Number(num) => MediaFeatureValue::Number(num + other),
      MediaFeatureValue::Resolution(res) => MediaFeatureValue::Resolution(res + other),
      MediaFeatureValue::Ratio(ratio) => MediaFeatureValue::Ratio(ratio + other),
      MediaFeatureValue::Ident(id) => MediaFeatureValue::Ident(id),
    }
  }
}

/// Consumes an operation or a colon, or returns an error.
fn consume_operation_or_colon<'i, 't>(
  input: &mut Parser<'i, 't>,
  allow_colon: bool,
) -> Result<Option<MediaFeatureComparison>, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  let first_delim = {
    let location = input.current_source_location();
    let next_token = input.next()?;
    match next_token {
      Token::Colon if allow_colon => return Ok(None),
      Token::Delim(oper) => oper,
      t => return Err(location.new_unexpected_token_error(t.clone())),
    }
  };
  Ok(Some(match first_delim {
    '=' => MediaFeatureComparison::Equal,
    '>' => {
      if input.try_parse(|i| i.expect_delim('=')).is_ok() {
        MediaFeatureComparison::GreaterThanEqual
      } else {
        MediaFeatureComparison::GreaterThan
      }
    }
    '<' => {
      if input.try_parse(|i| i.expect_delim('=')).is_ok() {
        MediaFeatureComparison::LessThanEqual
      } else {
        MediaFeatureComparison::LessThan
      }
    }
    d => return Err(location.new_unexpected_token_error(Token::Delim(*d))),
  }))
}

fn process_condition<'i>(
  loc: Location,
  custom_media: &HashMap<CowArcStr<'i>, CustomMediaRule<'i>>,
  media_type: &mut MediaType<'i>,
  qualifier: &mut Option<Qualifier>,
  condition: &mut MediaCondition<'i>,
  seen: &mut HashSet<CowArcStr<'i>>,
) -> Result<bool, MinifyError> {
  match condition {
    MediaCondition::Not(cond) => {
      let used = process_condition(loc, custom_media, media_type, qualifier, &mut *cond, seen)?;
      if !used {
        // If unused, only a media type remains so apply a not qualifier.
        // If it is already not, then it cancels out.
        *qualifier = if *qualifier == Some(Qualifier::Not) {
          None
        } else {
          Some(Qualifier::Not)
        };
        return Ok(false);
      }

      // Unwrap nested nots
      if let MediaCondition::Not(cond) = &**cond {
        *condition = (**cond).clone();
      }
    }
    MediaCondition::InParens(cond) => {
      let res = process_condition(loc, custom_media, media_type, qualifier, &mut *cond, seen);
      if let MediaCondition::InParens(cond) = &**cond {
        *condition = (**cond).clone();
      }
      return res;
    }
    MediaCondition::Operation(conditions, _) => {
      let mut res = Ok(true);
      conditions.retain_mut(|condition| {
        let r = process_condition(loc, custom_media, media_type, qualifier, condition, seen);
        if let Ok(used) = r {
          used
        } else {
          res = r;
          false
        }
      });
      return res;
    }
    MediaCondition::Feature(MediaFeature::Boolean(name)) => {
      if !name.starts_with("--") {
        return Ok(true);
      }

      if seen.contains(name) {
        return Err(ErrorWithLocation {
          kind: MinifyErrorKind::CircularCustomMedia { name: name.to_string() },
          loc,
        });
      }

      let rule = custom_media.get(name).ok_or_else(|| ErrorWithLocation {
        kind: MinifyErrorKind::CustomMediaNotDefined { name: name.to_string() },
        loc,
      })?;

      seen.insert(name.clone());

      let mut res = Ok(true);
      let mut conditions: Vec<MediaCondition> = rule
        .query
        .media_queries
        .iter()
        .filter_map(|query| {
          if query.media_type != MediaType::All || query.qualifier != None {
            if *media_type == MediaType::All {
              // `not all` will never match.
              if *qualifier == Some(Qualifier::Not) {
                res = Ok(false);
                return None;
              }

              // Propagate media type and qualifier to @media rule.
              *media_type = query.media_type.clone();
              *qualifier = query.qualifier.clone();
            } else if query.media_type != *media_type || query.qualifier != *qualifier {
              // Boolean logic with media types is hard to emulate, so we error for now.
              res = Err(ErrorWithLocation {
                kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
                  custom_media_loc: rule.loc,
                },
                loc,
              });
              return None;
            }
          }

          if let Some(condition) = &query.condition {
            let mut condition = condition.clone();
            let r = process_condition(loc, custom_media, media_type, qualifier, &mut condition, seen);
            if r.is_err() {
              res = r;
            }
            Some(condition)
          } else {
            None
          }
        })
        .collect();

      seen.remove(name);

      if res.is_err() {
        return res;
      }

      if conditions.is_empty() {
        return Ok(false);
      }

      if conditions.len() == 1 {
        *condition = conditions.pop().unwrap();
      } else {
        *condition = MediaCondition::InParens(Box::new(MediaCondition::Operation(conditions, Operator::Or)));
      }
    }
    _ => {}
  }

  Ok(true)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::stylesheet::PrinterOptions;

  fn parse(s: &str) -> MediaQuery {
    let mut input = ParserInput::new(&s);
    let mut parser = Parser::new(&mut input);
    MediaQuery::parse(&mut parser).unwrap()
  }

  fn and(a: &str, b: &str) -> String {
    let mut a = parse(a);
    let b = parse(b);
    a.and(&b).unwrap();
    a.to_css_string(PrinterOptions::default()).unwrap()
  }

  #[test]
  fn test_and() {
    assert_eq!(and("(min-width: 250px)", "(color)"), "(min-width: 250px) and (color)");
    assert_eq!(
      and("(min-width: 250px) or (color)", "(orientation: landscape)"),
      "((min-width: 250px) or (color)) and (orientation: landscape)"
    );
    assert_eq!(
      and("(min-width: 250px) and (color)", "(orientation: landscape)"),
      "(min-width: 250px) and (color) and (orientation: landscape)"
    );
    assert_eq!(and("all", "print"), "print");
    assert_eq!(and("print", "all"), "print");
    assert_eq!(and("all", "not print"), "not print");
    assert_eq!(and("not print", "all"), "not print");
    assert_eq!(and("not all", "print"), "not all");
    assert_eq!(and("print", "not all"), "not all");
    assert_eq!(and("print", "screen"), "not all");
    assert_eq!(and("not print", "screen"), "screen");
    assert_eq!(and("print", "not screen"), "print");
    assert_eq!(and("not screen", "print"), "print");
    assert_eq!(and("not screen", "not all"), "not all");
    assert_eq!(and("print", "(min-width: 250px)"), "print and (min-width: 250px)");
    assert_eq!(and("(min-width: 250px)", "print"), "print and (min-width: 250px)");
    assert_eq!(
      and("print and (min-width: 250px)", "(color)"),
      "print and (min-width: 250px) and (color)"
    );
    assert_eq!(and("all", "only screen"), "only screen");
    assert_eq!(and("only screen", "all"), "only screen");
    assert_eq!(and("print", "print"), "print");
  }
}
