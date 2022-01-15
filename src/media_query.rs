use cssparser::*;
use crate::traits::{ToCss, Parse};
use crate::printer::Printer;
use crate::macros::enum_property;
use crate::values::{
  length::Length,
  resolution::Resolution,
  ratio::Ratio
};
use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};

/// A type that encapsulates a media query list.
#[derive(Clone, Debug, PartialEq)]
pub struct MediaList {
  /// The list of media queries.
  pub media_queries: Vec<MediaQuery>,
}

impl MediaList {
  /// Parse a media query list from CSS.
  ///
  /// Always returns a media query list. Invalid media queries are
  /// omitted. If the list is empty, it is equivalent to "not all".
  ///
  /// <https://drafts.csswg.org/mediaqueries/#error-handling>
  pub fn parse(input: &mut Parser) -> Self {
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
}
    
impl ToCss for MediaList {
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
pub enum MediaType {
  /// A media type that matches every device.
  All,
  Print,
  Screen,
  /// A specific media type.
  Custom(String),
}

impl MediaType {
  fn parse(name: &str) -> Result<Self, ()> {
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
pub struct MediaQuery {
  /// The qualifier for this query.
  pub qualifier: Option<Qualifier>,
  /// The media type for this query, that can be known, unknown, or "all".
  pub media_type: MediaType,
  /// The condition that this media query contains. This cannot have `or`
  /// in the first level.
  pub condition: Option<MediaCondition>,
}

impl MediaQuery {
  /// Parse a media query given css input.
  ///
  /// Returns an error if any of the expressions is unknown.
  pub fn parse<'i, 't>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (qualifier, explicit_media_type) = input
      .try_parse(|input| -> Result<_, ()> {
        let qualifier = input.try_parse(Qualifier::parse).ok();
        let ident = input.expect_ident().map_err(|_| ())?;
        let media_type = MediaType::parse(&ident)?;
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
}

impl ToCss for MediaQuery {
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
pub enum MediaCondition {
  /// A media feature, implicitly parenthesized.
  Feature(MediaFeature),
  /// A negation of a condition.
  Not(Box<MediaCondition>),
  /// A set of joint operations.
  Operation(Box<[MediaCondition]>, Operator),
  /// A condition wrapped in parenthesis.
  InParens(Box<MediaCondition>),
}

impl MediaCondition {
  /// Parse a single media condition.
  pub fn parse<'i, 't>(
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
          conditions.into_boxed_slice(),
          operator,
        ));
      }
      
      conditions.push(Self::parse_in_parens(input)?);
    }
  }
  
  /// Parse a media condition in parentheses.
  pub fn parse_in_parens<'i, 't>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.expect_parenthesis_block()?;
    Self::parse_paren_block(input)
  }
  
  fn parse_paren_block<'i, 't>(
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

impl ToCss for MediaCondition {
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
pub enum MediaFeature {
  // e.g. (min-width: 240px)
  Plain {
    name: String,
    value: MediaFeatureValue
  },
  // e.g. (hover)
  Boolean(String),
  // e.g. (width > 240px)
  Range {
    name: String,
    operator: MediaFeatureComparison,
    value: MediaFeatureValue
  },
  /// e.g. (120px < width < 240px)
  Interval {
    name: String,
    start: MediaFeatureValue,
    start_operator: MediaFeatureComparison,
    end: MediaFeatureValue,
    end_operator: MediaFeatureComparison
  }
}

impl Parse for MediaFeature {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(res) = input.try_parse(Self::parse_name_first) {
      return Ok(res)
    }
    
    Self::parse_value_first(input)
  }
}

impl MediaFeature {
  fn parse_name_first<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = input.expect_ident()?.as_ref().to_owned();
    
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
  
  fn parse_value_first<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = MediaFeatureValue::parse(input)?;
    let operator = consume_operation_or_colon(input, false)?;
    let name = input.expect_ident()?.as_ref().to_owned();
    
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

impl ToCss for MediaFeature {
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
pub enum MediaFeatureValue {
  Length(Length),
  Number(f32),
  Resolution(Resolution),
  Ratio(Ratio),
  Ident(String)
}

impl Parse for MediaFeatureValue {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
    
    let ident = input.expect_ident()?.as_ref().to_owned();
    Ok(MediaFeatureValue::Ident(ident))
  }
}

impl ToCss for MediaFeatureValue {
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

impl std::ops::Add<f32> for MediaFeatureValue {
  type Output = Self;

  fn add(self, other: f32) -> MediaFeatureValue {
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
