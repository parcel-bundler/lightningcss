use crate::values::string::CowArcStr;
use cssparser::*;
use crate::rules::custom_media::CustomMediaRule;
use crate::traits::{ToCss, Parse};
use crate::printer::Printer;
use crate::macros::enum_property;
use crate::values::{
  length::Length,
  resolution::Resolution,
  ratio::Ratio
};
use crate::compat::Feature;
use crate::error::{ParserError, MinifyError, PrinterError};
use std::collections::{HashMap, HashSet};
use retain_mut::RetainMut;

/// A type that encapsulates a media query list.
#[derive(Clone, Debug, PartialEq)]
pub struct MediaList<'i> {
  /// The list of media queries.
  pub media_queries: Vec<MediaQuery<'i>>,
}

impl<'i> MediaList<'i> {
  pub fn new() -> Self {
    MediaList {
      media_queries: vec![]
    }
  }
  /// Parse a media query list from CSS.
  ///
  /// Always returns a media query list. Invalid media queries are
  /// omitted. If the list is empty, it is equivalent to "not all".
  ///
  /// <https://drafts.csswg.org/mediaqueries/#error-handling>
  pub fn parse<'t>(input: &mut Parser<'i, 't>) -> Self {
    let mut media_queries = vec![];
    loop {
      if let Ok(mq) = input.parse_until_before(Delimiter::Comma, |i| MediaQuery::parse(i)) {
        media_queries.push(mq);
      }
        
      match input.next() {
        Ok(&Token::Comma) => {},
        Ok(_) => unreachable!(),
        Err(_) => break,
      }
    }
      
    MediaList { media_queries }
  }

  pub(crate) fn transform_custom_media(&mut self, loc: SourceLocation, custom_media: &HashMap<CowArcStr<'i>, CustomMediaRule<'i>>) -> Result<(), MinifyError> {
    for query in self.media_queries.iter_mut() {
      query.transform_custom_media(loc, custom_media)?;
    }
    Ok(())
  }

  pub fn never_matches(&self) -> bool {
    self.media_queries.is_empty() 
      || self.media_queries.iter().all(|mq| mq.never_matches())
  }

  pub fn and(&mut self, b: &MediaList<'i>) {
    if self.media_queries.is_empty() {
      self.media_queries.extend(b.media_queries.iter().cloned());
      return
    }
  
    for b in &b.media_queries {
      if self.media_queries.contains(&b) {
        continue;
      }
  
      for a in &mut self.media_queries {
        a.and(&b)
      }
    }
  }

  pub fn or(&mut self, b: &MediaList<'i>) {
    for mq in &b.media_queries {
      if !self.media_queries.contains(&mq) {
        self.media_queries.push(mq.clone())
      }
    }
  }
}
    
impl<'i> ToCss for MediaList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    if self.media_queries.is_empty() {
      dest.write_str("not all")?;
      return Ok(())
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
  /// <https://drafts.csswg.org/mediaqueries/#mq-prefix>
  pub enum Qualifier {
    Only,
    Not,
  }
}

/// <http://dev.w3.org/csswg/mediaqueries-3/#media0>
#[derive(Clone, Debug, PartialEq)]
pub enum MediaType<'i> {
  /// A media type that matches every device.
  All,
  Print,
  Screen,
  /// A specific media type.
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

/// https://drafts.csswg.org/mediaqueries/
#[derive(Clone, Debug, PartialEq)]
pub struct MediaQuery<'i> {
  /// The qualifier for this query.
  pub qualifier: Option<Qualifier>,
  /// The media type for this query, that can be known, unknown, or "all".
  pub media_type: MediaType<'i>,
  /// The condition that this media query contains. This cannot have `or`
  /// in the first level.
  pub condition: Option<MediaCondition<'i>>,
}

impl<'i> MediaQuery<'i> {
  /// Parse a media query given css input.
  ///
  /// Returns an error if any of the expressions is unknown.
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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

  fn transform_custom_media(&mut self, loc: SourceLocation, custom_media: &HashMap<CowArcStr<'i>, CustomMediaRule<'i>>) -> Result<(), MinifyError> {
    if let Some(condition) = &mut self.condition {
      let used = process_condition(
        loc,
        custom_media,
        &mut self.media_type,
        &mut self.qualifier,
        condition,
        &mut HashSet::new()
      )?;
      if !used {
        self.condition = None;
      }
    }
    Ok(())
  }

  pub fn never_matches(&self) -> bool {
    self.qualifier == Some(Qualifier::Not) && self.media_type == MediaType::All
  }

  pub fn and<'a>(&mut self, b: &MediaQuery<'i>) {
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
          // ERROR
          todo!()
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
            }
          }
          Some(MediaCondition::Operation(vec![parenthesize!(condition), parenthesize!(cond)], Operator::And))
        } else {
          Some(condition.clone())
        }
      } else {
        Some(cond.clone())
      }
    }
  }
}

impl<'i> ToCss for MediaQuery<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
      },
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
    And,
    Or,
  }
}

/// Represents a media condition.
#[derive(Clone, Debug, PartialEq)]
pub enum MediaCondition<'i> {
  /// A media feature, implicitly parenthesized.
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
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
    allow_or: bool
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
        return Ok(MediaCondition::Operation(
          conditions,
          operator,
        ));
      }
      
      conditions.push(Self::parse_in_parens(input)?);
    }
  }
  
  /// Parse a media condition in parentheses.
  pub fn parse_in_parens<'t>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.expect_parenthesis_block()?;
    Self::parse_paren_block(input)
  }
  
  fn parse_paren_block<'t>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match *self {
      MediaCondition::Feature(ref f) => f.to_css(dest),
      MediaCondition::Not(ref c) => {
        dest.write_str("not ")?;
        c.to_css(dest)
      },
      MediaCondition::InParens(ref c) => {
        dest.write_char('(')?;
        c.to_css(dest)?;
        dest.write_char(')')
      },
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
      },
    }
  }
}

/// https://drafts.csswg.org/mediaqueries/#typedef-mf-comparison
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MediaFeatureComparison {
  /// =
  Equal,
  /// >
  GreaterThan,
  /// >=
  GreaterThanEqual,
  /// <
  LessThan,
  /// <=
  LessThanEqual,
}

impl ToCss for MediaFeatureComparison {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use MediaFeatureComparison::*;
    match self {
      Equal => dest.delim('=', true),
      GreaterThan => dest.delim('>', true),
      GreaterThanEqual => {
        dest.whitespace()?;
        dest.write_str(">=")?;
        dest.whitespace()
      },
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
      MediaFeatureComparison::Equal => MediaFeatureComparison::Equal
    }
  }
}

/// https://drafts.csswg.org/mediaqueries/#typedef-media-feature
#[derive(Clone, Debug, PartialEq)]
pub enum MediaFeature<'i> {
  // e.g. (min-width: 240px)
  Plain {
    name: CowArcStr<'i>,
    value: MediaFeatureValue<'i>
  },
  // e.g. (hover)
  Boolean(CowArcStr<'i>),
  // e.g. (width > 240px)
  Range {
    name: CowArcStr<'i>,
    operator: MediaFeatureComparison,
    value: MediaFeatureValue<'i>
  },
  /// e.g. (120px < width < 240px)
  Interval {
    name: CowArcStr<'i>,
    start: MediaFeatureValue<'i>,
    start_operator: MediaFeatureComparison,
    end: MediaFeatureValue<'i>,
    end_operator: MediaFeatureComparison
  }
}

impl<'i> Parse<'i> for MediaFeature<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(res) = input.try_parse(Self::parse_name_first) {
      return Ok(res)
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
      Ok(MediaFeature::Range {
        name,
        operator,
        value
      })
    } else {
      Ok(MediaFeature::Plain {
        name,
        value
      })
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
        (MediaFeatureComparison::GreaterThan, MediaFeatureComparison::GreaterThan) |
        (MediaFeatureComparison::GreaterThan, MediaFeatureComparison::GreaterThanEqual) |
        (MediaFeatureComparison::GreaterThanEqual, MediaFeatureComparison::GreaterThanEqual) |
        (MediaFeatureComparison::GreaterThanEqual, MediaFeatureComparison::GreaterThan) |
        (MediaFeatureComparison::LessThan, MediaFeatureComparison::LessThan) |
        (MediaFeatureComparison::LessThan, MediaFeatureComparison::LessThanEqual) |
        (MediaFeatureComparison::LessThanEqual, MediaFeatureComparison::LessThanEqual) |
        (MediaFeatureComparison::LessThanEqual, MediaFeatureComparison::LessThan) => {},
        _ => return Err(input.new_custom_error(ParserError::InvalidMediaQuery))
      };
      let end_value = MediaFeatureValue::parse(input)?;
      Ok(MediaFeature::Interval {
        name,
        start: value,
        start_operator,
        end: end_value,
        end_operator
      })
    } else {
      let operator = operator.unwrap().opposite();
      Ok(MediaFeature::Range {
        name,
        operator,
        value
      })
    }
  }
}

impl<'i> ToCss for MediaFeature<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
            return write_min_max(operator, name, value, dest)
          }
        }

        serialize_identifier(name, dest)?;
        operator.to_css(dest)?;
        value.to_css(dest)?;
      }
      MediaFeature::Interval { name, start, start_operator, end, end_operator } => {
        if let Some(targets) = dest.targets {
          if !Feature::MediaIntervalSyntax.is_compatible(targets) {
            write_min_max(&start_operator.opposite(), name, start, dest)?;
            dest.write_str(" and (")?;
            return write_min_max(end_operator, name, end, dest)
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
fn write_min_max<W>(operator: &MediaFeatureComparison, name: &str, value: &MediaFeatureValue, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
  let prefix = match operator {
    MediaFeatureComparison::GreaterThan |
    MediaFeatureComparison::GreaterThanEqual => Some("min-"),
    MediaFeatureComparison::LessThan |
    MediaFeatureComparison::LessThanEqual => Some("max-"),
    MediaFeatureComparison::Equal => None
  };

  if let Some(prefix) = prefix {
    dest.write_str(prefix)?;
  }

  serialize_identifier(name, dest)?;
  dest.delim(':', false)?;

  let adjusted = match operator {
    MediaFeatureComparison::GreaterThan => Some(value.clone() + 0.001),
    MediaFeatureComparison::LessThan => Some(value.clone() + -0.001),
    _ => None
  };

  if let Some(value) = adjusted {
    value.to_css(dest)?;
  } else {
    value.to_css(dest)?;
  }

  dest.write_char(')')?;
  Ok(())
}

#[derive(Clone, Debug, PartialEq)]
pub enum MediaFeatureValue<'i> {
  Length(Length),
  Number(f32),
  Resolution(Resolution),
  Ratio(Ratio),
  Ident(CowArcStr<'i>)
}

impl<'i> Parse<'i> for MediaFeatureValue<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Ratios are ambigous with numbers because the second param is optional (e.g. 2/1 == 2).
    // We require the / delimeter when parsing ratios so that 2/1 ends up as a ratio and 2 is
    // parsed as a number.
    if let Ok(ratio) = input.try_parse(Ratio::parse_required) {
      return Ok(MediaFeatureValue::Ratio(ratio))
    }

    // Parse number next so that unitless values are not parsed as lengths.
    if let Ok(num) = input.try_parse(f32::parse) {
      return Ok(MediaFeatureValue::Number(num))
    }
    
    if let Ok(length) = input.try_parse(Length::parse) {
      return Ok(MediaFeatureValue::Length(length))
    }
    
    if let Ok(res) = input.try_parse(Resolution::parse) {
      return Ok(MediaFeatureValue::Resolution(res))
    }
    
    let ident = input.expect_ident()?;
    Ok(MediaFeatureValue::Ident(ident.into()))
  }
}

impl<'i> ToCss for MediaFeatureValue<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
      MediaFeatureValue::Ident(id) => MediaFeatureValue::Ident(id)
    }
  }
}

/// Consumes an operation or a colon, or returns an error.
fn consume_operation_or_colon<'i, 't>(input: &mut Parser<'i, 't>, allow_colon: bool) -> Result<Option<MediaFeatureComparison>, ParseError<'i, ParserError<'i>>> {
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
    },
    '<' => {
      if input.try_parse(|i| i.expect_delim('=')).is_ok() {
        MediaFeatureComparison::LessThanEqual
      } else {
        MediaFeatureComparison::LessThan
      }
    },
    d => return Err(location.new_unexpected_token_error(Token::Delim(*d))),
  }))
}

fn process_condition<'i>(
  loc: SourceLocation,
  custom_media: &HashMap<CowArcStr<'i>, CustomMediaRule<'i>>,
  media_type: &mut MediaType<'i>,
  qualifier: &mut Option<Qualifier>,
  condition: &mut MediaCondition<'i>,
  seen: &mut HashSet<CowArcStr<'i>>
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
        return Ok(false)
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
        return Ok(true)
      }

      if seen.contains(name) {
        return Err(MinifyError::CircularCustomMedia {
          name: name.to_string(),
          loc
        });
      }
      
      let rule = custom_media.get(name)
        .ok_or_else(|| MinifyError::CustomMediaNotDefined {
          name: name.to_string(),
          loc
        })?;

      seen.insert(name.clone());

      let mut res = Ok(true);
      let mut conditions: Vec<MediaCondition> = rule.query.media_queries.iter().filter_map(|query| {
        if query.media_type != MediaType::All || query.qualifier != None {
          if *media_type == MediaType::All {
            // `not all` will never match.
            if *qualifier == Some(Qualifier::Not) {
              res = Ok(false);
              return None
            }
            
            // Propagate media type and qualifier to @media rule.
            *media_type = query.media_type.clone();
            *qualifier = query.qualifier.clone();
          } else if query.media_type != *media_type || query.qualifier != *qualifier {
            // Boolean logic with media types is hard to emulate, so we error for now.
            res = Err(MinifyError::UnsupportedCustomMediaBooleanLogic {
              media_loc: loc,
              custom_media_loc: rule.loc
            });
            return None
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
      }).collect();

      seen.remove(name);

      if res.is_err() {
        return res;
      }

      if conditions.is_empty() {
        return Ok(false)
      }

      if conditions.len() == 1 {
        *condition = conditions.pop().unwrap();
      } else {
        *condition = MediaCondition::InParens(
          Box::new(MediaCondition::Operation(conditions, Operator::Or))
        );
      }
    }
    _ => {}
  }

  Ok(true)
}

#[cfg(test)]
mod tests {
  use super::*;

  fn parse(s: &str) -> MediaQuery {
    let mut input = ParserInput::new(&s);
    let mut parser = Parser::new(&mut input);
    MediaQuery::parse(&mut parser).unwrap()
  }

  fn and(a: &str, b: &str) -> String {
    let mut a = parse(a);
    let b = parse(b);
    a.and(&b);
    a.to_css_string()
  }

  #[test]
  fn test_and() {
    assert_eq!(and("(min-width: 250px)", "(color)"), "(min-width: 250px) and (color)");
    assert_eq!(and("(min-width: 250px) or (color)", "(orientation: landscape)"), "((min-width: 250px) or (color)) and (orientation: landscape)");
    assert_eq!(and("(min-width: 250px) and (color)", "(orientation: landscape)"), "(min-width: 250px) and (color) and (orientation: landscape)");
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
    assert_eq!(and("print and (min-width: 250px)", "(color)"), "print and (min-width: 250px) and (color)");
    assert_eq!(and("all", "only screen"), "only screen");
    assert_eq!(and("only screen", "all"), "only screen");
    assert_eq!(and("print", "print"), "print");
  }
}
