//! Media queries.
use crate::error::{ErrorWithLocation, MinifyError, MinifyErrorKind, ParserError, PrinterError};
use crate::macros::enum_property;
use crate::parser::starts_with_ignore_ascii_case;
use crate::printer::Printer;
use crate::properties::custom::{EnvironmentVariable, TokenList};
#[cfg(feature = "visitor")]
use crate::rules::container::{ContainerSizeFeatureId, ScrollStateFeatureId};
use crate::rules::custom_media::CustomMediaRule;
use crate::rules::Location;
use crate::stylesheet::ParserOptions;
use crate::targets::{should_compile, Targets};
use crate::traits::{Parse, ParseWithOptions, ToCss};
use crate::values::ident::{DashedIdent, Ident};
use crate::values::number::{CSSInteger, CSSNumber};
use crate::values::string::CowArcStr;
use crate::values::{length::Length, ratio::Ratio, resolution::Resolution};
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use bitflags::bitflags;
use cssparser::*;
#[cfg(feature = "into_owned")]
use static_self::IntoOwned;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

#[cfg(feature = "serde")]
use crate::serialization::ValueWrapper;

/// A [media query list](https://drafts.csswg.org/mediaqueries/#mq-list).
#[derive(Clone, Debug, PartialEq, Default)]
#[cfg_attr(feature = "visitor", derive(Visit), visit(visit_media_list, MEDIA_QUERIES))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
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
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut media_queries = vec![];
    if input.is_exhausted() {
      return Ok(MediaList { media_queries });
    }

    loop {
      match input.parse_until_before(Delimiter::Comma, |i| MediaQuery::parse_with_options(i, options)) {
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

  pub(crate) fn transform_resolution(&mut self, targets: Targets) {
    let mut i = 0;
    while i < self.media_queries.len() {
      let query = &self.media_queries[i];
      let mut prefixes = query.get_necessary_prefixes(targets);
      prefixes.remove(VendorPrefix::None);
      if !prefixes.is_empty() {
        let query = query.clone();
        for prefix in prefixes {
          let mut transformed = query.clone();
          transformed.transform_resolution(prefix);
          if !self.media_queries.contains(&transformed) {
            self.media_queries.insert(i, transformed);
          }
          i += 1;
        }
      }

      i += 1;
    }
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
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case", into = "CowArcStr", from = "CowArcStr")
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

impl<'i> From<CowArcStr<'i>> for MediaType<'i> {
  fn from(name: CowArcStr<'i>) -> Self {
    match_ignore_ascii_case! { &*name,
      "all" => MediaType::All,
      "print" => MediaType::Print,
      "screen" => MediaType::Screen,
      _ => MediaType::Custom(name)
    }
  }
}

impl<'i> Into<CowArcStr<'i>> for MediaType<'i> {
  fn into(self) -> CowArcStr<'i> {
    match self {
      MediaType::All => "all".into(),
      MediaType::Print => "print".into(),
      MediaType::Screen => "screen".into(),
      MediaType::Custom(desc) => desc,
    }
  }
}

impl<'i> Parse<'i> for MediaType<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name: CowArcStr = input.expect_ident()?.into();
    Ok(Self::from(name))
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for MediaType<'a> {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    str::json_schema(gen)
  }

  fn schema_name() -> String {
    "MediaType".into()
  }
}

/// A [media query](https://drafts.csswg.org/mediaqueries/#media).
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "visitor", visit(visit_media_query, MEDIA_QUERIES))]
#[cfg_attr(feature = "serde", derive(serde::Serialize), serde(rename_all = "camelCase"))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
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

impl<'i> ParseWithOptions<'i> for MediaQuery<'i> {
  fn parse_with_options<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (qualifier, explicit_media_type) = input
      .try_parse(|input| -> Result<_, ParseError<'i, ParserError<'i>>> {
        let qualifier = input.try_parse(Qualifier::parse).ok();
        let media_type = MediaType::parse(input)?;
        Ok((qualifier, Some(media_type)))
      })
      .unwrap_or_default();

    let condition = if explicit_media_type.is_none() {
      Some(MediaCondition::parse_with_flags(
        input,
        QueryConditionFlags::ALLOW_OR,
        options,
      )?)
    } else if input.try_parse(|i| i.expect_ident_matching("and")).is_ok() {
      Some(MediaCondition::parse_with_flags(
        input,
        QueryConditionFlags::empty(),
        options,
      )?)
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

impl<'i> MediaQuery<'i> {
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

  fn get_necessary_prefixes(&self, targets: Targets) -> VendorPrefix {
    if let Some(condition) = &self.condition {
      condition.get_necessary_prefixes(targets)
    } else {
      VendorPrefix::empty()
    }
  }

  fn transform_resolution(&mut self, prefix: VendorPrefix) {
    if let Some(condition) = &mut self.condition {
      condition.transform_resolution(prefix)
    }
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
          Some(MediaCondition::Operation {
            conditions: vec![condition.clone(), cond.clone()],
            operator: Operator::And,
          })
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

    let needs_parens = if self.media_type != MediaType::All || self.qualifier.is_some() {
      dest.write_str(" and ")?;
      matches!(condition, MediaCondition::Operation { operator, .. } if *operator != Operator::And)
    } else {
      false
    };

    to_css_with_parens_if_needed(condition, dest, needs_parens)
  }
}

#[cfg(feature = "serde")]
#[derive(serde::Deserialize)]
#[serde(untagged)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum MediaQueryOrRaw<'i> {
  #[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
  MediaQuery {
    qualifier: Option<Qualifier>,
    #[cfg_attr(feature = "serde", serde(borrow))]
    media_type: MediaType<'i>,
    condition: Option<MediaCondition<'i>>,
  },
  Raw {
    raw: CowArcStr<'i>,
  },
}

#[cfg(feature = "serde")]
impl<'i, 'de: 'i> serde::Deserialize<'de> for MediaQuery<'i> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let mq = MediaQueryOrRaw::deserialize(deserializer)?;
    match mq {
      MediaQueryOrRaw::MediaQuery {
        qualifier,
        media_type,
        condition,
      } => Ok(MediaQuery {
        qualifier,
        media_type,
        condition,
      }),
      MediaQueryOrRaw::Raw { raw } => {
        let res = MediaQuery::parse_string_with_options(raw.as_ref(), ParserOptions::default())
          .map_err(|_| serde::de::Error::custom("Could not parse value"))?;
        Ok(res.into_owned())
      }
    }
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
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum MediaCondition<'i> {
  /// A media feature, implicitly parenthesized.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<MediaFeature>"))]
  Feature(MediaFeature<'i>),
  /// A negation of a condition.
  #[cfg_attr(feature = "visitor", skip_type)]
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<Box<MediaCondition>>"))]
  Not(Box<MediaCondition<'i>>),
  /// A set of joint operations.
  #[cfg_attr(feature = "visitor", skip_type)]
  Operation {
    /// The operator for the conditions.
    operator: Operator,
    /// The conditions for the operator.
    conditions: Vec<MediaCondition<'i>>,
  },
  /// Unknown tokens.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<TokenList>"))]
  Unknown(TokenList<'i>),
}

/// A trait for conditions such as media queries and container queries.
pub(crate) trait QueryCondition<'i>: Sized {
  fn parse_feature<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>>;
  fn create_negation(condition: Box<Self>) -> Self;
  fn create_operation(operator: Operator, conditions: Vec<Self>) -> Self;
  fn parse_style_query<'t>(
    input: &mut Parser<'i, 't>,
    _options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Err(input.new_error_for_next_token())
  }

  fn parse_scroll_state_query<'t>(
    input: &mut Parser<'i, 't>,
    _options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Err(input.new_error_for_next_token())
  }

  fn needs_parens(&self, parent_operator: Option<Operator>, targets: &Targets) -> bool;
}

impl<'i> QueryCondition<'i> for MediaCondition<'i> {
  #[inline]
  fn parse_feature<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let feature = MediaFeature::parse_with_options(input, options)?;
    Ok(Self::Feature(feature))
  }

  #[inline]
  fn create_negation(condition: Box<MediaCondition<'i>>) -> Self {
    Self::Not(condition)
  }

  #[inline]
  fn create_operation(operator: Operator, conditions: Vec<MediaCondition<'i>>) -> Self {
    Self::Operation { operator, conditions }
  }

  fn needs_parens(&self, parent_operator: Option<Operator>, targets: &Targets) -> bool {
    match self {
      MediaCondition::Not(_) => true,
      MediaCondition::Operation { operator, .. } => Some(*operator) != parent_operator,
      MediaCondition::Feature(f) => f.needs_parens(parent_operator, targets),
      MediaCondition::Unknown(_) => false,
    }
  }
}

bitflags! {
  /// Flags for `parse_query_condition`.
  #[derive(PartialEq, Eq, Clone, Copy)]
  pub(crate) struct QueryConditionFlags: u8 {
    /// Whether to allow top-level "or" boolean logic.
    const ALLOW_OR = 1 << 0;
    /// Whether to allow style container queries.
    const ALLOW_STYLE = 1 << 1;
    /// Whether to allow scroll state container queries.
    const ALLOW_SCROLL_STATE = 1 << 2;
  }
}

impl<'i> MediaCondition<'i> {
  /// Parse a single media condition.
  fn parse_with_flags<'t>(
    input: &mut Parser<'i, 't>,
    flags: QueryConditionFlags,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input
      .try_parse(|input| parse_query_condition(input, flags, options))
      .or_else(|e| {
        if options.error_recovery {
          options.warn(e);
          Ok(MediaCondition::Unknown(TokenList::parse(input, options, 0)?))
        } else {
          Err(e)
        }
      })
  }

  fn get_necessary_prefixes(&self, targets: Targets) -> VendorPrefix {
    match self {
      MediaCondition::Feature(MediaFeature::Range {
        name: MediaFeatureName::Standard(MediaFeatureId::Resolution),
        ..
      }) => targets.prefixes(VendorPrefix::None, crate::prefixes::Feature::AtResolution),
      MediaCondition::Not(not) => not.get_necessary_prefixes(targets),
      MediaCondition::Operation { conditions, .. } => {
        let mut prefixes = VendorPrefix::empty();
        for condition in conditions {
          prefixes |= condition.get_necessary_prefixes(targets);
        }
        prefixes
      }
      _ => VendorPrefix::empty(),
    }
  }

  fn transform_resolution(&mut self, prefix: VendorPrefix) {
    match self {
      MediaCondition::Feature(MediaFeature::Range {
        name: MediaFeatureName::Standard(MediaFeatureId::Resolution),
        operator,
        value: MediaFeatureValue::Resolution(value),
      }) => match prefix {
        VendorPrefix::WebKit | VendorPrefix::Moz => {
          *self = MediaCondition::Feature(MediaFeature::Range {
            name: MediaFeatureName::Standard(match prefix {
              VendorPrefix::WebKit => MediaFeatureId::WebKitDevicePixelRatio,
              VendorPrefix::Moz => MediaFeatureId::MozDevicePixelRatio,
              _ => unreachable!(),
            }),
            operator: *operator,
            value: MediaFeatureValue::Number(match value {
              Resolution::Dpi(dpi) => *dpi / 96.0,
              Resolution::Dpcm(dpcm) => *dpcm * 2.54 / 96.0,
              Resolution::Dppx(dppx) => *dppx,
            }),
          });
        }
        _ => {}
      },
      MediaCondition::Not(not) => not.transform_resolution(prefix),
      MediaCondition::Operation { conditions, .. } => {
        for condition in conditions {
          condition.transform_resolution(prefix);
        }
      }
      _ => {}
    }
  }
}

impl<'i> ParseWithOptions<'i> for MediaCondition<'i> {
  fn parse_with_options<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Self::parse_with_flags(input, QueryConditionFlags::ALLOW_OR, options)
  }
}

/// Parse a single query condition.
pub(crate) fn parse_query_condition<'t, 'i, P: QueryCondition<'i>>(
  input: &mut Parser<'i, 't>,
  flags: QueryConditionFlags,
  options: &ParserOptions<'_, 'i>,
) -> Result<P, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  enum QueryFunction {
    None,
    Style,
    ScrollState,
  }

  let (is_negation, function) = match *input.next()? {
    Token::ParenthesisBlock => (false, QueryFunction::None),
    Token::Ident(ref ident) if ident.eq_ignore_ascii_case("not") => (true, QueryFunction::None),
    Token::Function(ref f)
      if flags.contains(QueryConditionFlags::ALLOW_STYLE) && f.eq_ignore_ascii_case("style") =>
    {
      (false, QueryFunction::Style)
    }
    Token::Function(ref f)
      if flags.contains(QueryConditionFlags::ALLOW_SCROLL_STATE) && f.eq_ignore_ascii_case("scroll-state") =>
    {
      (false, QueryFunction::ScrollState)
    }
    ref t => return Err(location.new_unexpected_token_error(t.clone())),
  };

  let first_condition = match (is_negation, function) {
    (true, QueryFunction::None) => {
      let inner_condition = parse_parens_or_function(input, flags, options)?;
      return Ok(P::create_negation(Box::new(inner_condition)));
    }
    (true, QueryFunction::Style) => {
      let inner_condition = P::parse_style_query(input, options)?;
      return Ok(P::create_negation(Box::new(inner_condition)));
    }
    (true, QueryFunction::ScrollState) => {
      let inner_condition = P::parse_scroll_state_query(input, options)?;
      return Ok(P::create_negation(Box::new(inner_condition)));
    }
    (false, QueryFunction::None) => parse_paren_block(input, flags, options)?,
    (false, QueryFunction::Style) => P::parse_style_query(input, options)?,
    (false, QueryFunction::ScrollState) => P::parse_scroll_state_query(input, options)?,
  };

  let operator = match input.try_parse(Operator::parse) {
    Ok(op) => op,
    Err(..) => return Ok(first_condition),
  };

  if !flags.contains(QueryConditionFlags::ALLOW_OR) && operator == Operator::Or {
    return Err(location.new_unexpected_token_error(Token::Ident("or".into())));
  }

  let mut conditions = vec![];
  conditions.push(first_condition);
  conditions.push(parse_parens_or_function(input, flags, options)?);

  let delim = match operator {
    Operator::And => "and",
    Operator::Or => "or",
  };

  loop {
    if input.try_parse(|i| i.expect_ident_matching(delim)).is_err() {
      return Ok(P::create_operation(operator, conditions));
    }

    conditions.push(parse_parens_or_function(input, flags, options)?);
  }
}

/// Parse a media condition in parentheses, or a style() function.
fn parse_parens_or_function<'t, 'i, P: QueryCondition<'i>>(
  input: &mut Parser<'i, 't>,
  flags: QueryConditionFlags,
  options: &ParserOptions<'_, 'i>,
) -> Result<P, ParseError<'i, ParserError<'i>>> {
  let location = input.current_source_location();
  match *input.next()? {
    Token::ParenthesisBlock => parse_paren_block(input, flags, options),
    Token::Function(ref f)
      if flags.contains(QueryConditionFlags::ALLOW_STYLE) && f.eq_ignore_ascii_case("style") =>
    {
      P::parse_style_query(input, options)
    }
    Token::Function(ref f)
      if flags.contains(QueryConditionFlags::ALLOW_SCROLL_STATE) && f.eq_ignore_ascii_case("scroll-state") =>
    {
      P::parse_scroll_state_query(input, options)
    }
    ref t => return Err(location.new_unexpected_token_error(t.clone())),
  }
}

fn parse_paren_block<'t, 'i, P: QueryCondition<'i>>(
  input: &mut Parser<'i, 't>,
  flags: QueryConditionFlags,
  options: &ParserOptions<'_, 'i>,
) -> Result<P, ParseError<'i, ParserError<'i>>> {
  input.parse_nested_block(|input| {
    // Detect empty brackets and provide a clearer error message.
    if input.is_exhausted() {
      return Err(input.new_custom_error(ParserError::EmptyBracketInCondition));
    }

    if let Ok(inner) =
      input.try_parse(|i| parse_query_condition(i, flags | QueryConditionFlags::ALLOW_OR, options))
    {
      return Ok(inner);
    }

    P::parse_feature(input, options)
  })
}

pub(crate) fn to_css_with_parens_if_needed<V: ToCss, W>(
  value: V,
  dest: &mut Printer<W>,
  needs_parens: bool,
) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  if needs_parens {
    dest.write_char('(')?;
  }
  value.to_css(dest)?;
  if needs_parens {
    dest.write_char(')')?;
  }
  Ok(())
}

pub(crate) fn operation_to_css<'i, V: ToCss + QueryCondition<'i>, W>(
  operator: Operator,
  conditions: &Vec<V>,
  dest: &mut Printer<W>,
) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  let mut iter = conditions.iter();
  let first = iter.next().unwrap();
  to_css_with_parens_if_needed(first, dest, first.needs_parens(Some(operator), &dest.targets.current))?;
  for item in iter {
    dest.write_char(' ')?;
    operator.to_css(dest)?;
    dest.write_char(' ')?;
    to_css_with_parens_if_needed(item, dest, item.needs_parens(Some(operator), &dest.targets.current))?;
  }

  Ok(())
}

impl<'i> MediaCondition<'i> {
  fn negate(&self) -> Option<MediaCondition<'i>> {
    match self {
      MediaCondition::Not(not) => Some((**not).clone()),
      MediaCondition::Feature(f) => f.negate().map(MediaCondition::Feature),
      _ => None,
    }
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
        if let Some(negated) = c.negate() {
          negated.to_css(dest)
        } else {
          dest.write_str("not ")?;
          to_css_with_parens_if_needed(&**c, dest, c.needs_parens(None, &dest.targets.current))
        }
      }
      MediaCondition::Operation {
        ref conditions,
        operator,
      } => operation_to_css(operator, conditions, dest),
      MediaCondition::Unknown(ref tokens) => tokens.to_css(dest, false),
    }
  }
}

/// A [comparator](https://drafts.csswg.org/mediaqueries/#typedef-mf-comparison) within a media query.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
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

  fn negate(&self) -> MediaFeatureComparison {
    match self {
      MediaFeatureComparison::GreaterThan => MediaFeatureComparison::LessThanEqual,
      MediaFeatureComparison::GreaterThanEqual => MediaFeatureComparison::LessThan,
      MediaFeatureComparison::LessThan => MediaFeatureComparison::GreaterThanEqual,
      MediaFeatureComparison::LessThanEqual => MediaFeatureComparison::GreaterThan,
      MediaFeatureComparison::Equal => MediaFeatureComparison::Equal,
    }
  }
}

/// A generic media feature or container feature.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
  feature = "visitor",
  derive(Visit),
  visit(visit_media_feature, MEDIA_QUERIES, <'i, MediaFeatureId>),
  visit(<'i, ContainerSizeFeatureId>),
  visit(<'i, ScrollStateFeatureId>)
)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum QueryFeature<'i, FeatureId> {
  /// A plain media feature, e.g. `(min-width: 240px)`.
  Plain {
    /// The name of the feature.
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: MediaFeatureName<'i, FeatureId>,
    /// The feature value.
    value: MediaFeatureValue<'i>,
  },
  /// A boolean feature, e.g. `(hover)`.
  Boolean {
    /// The name of the feature.
    name: MediaFeatureName<'i, FeatureId>,
  },
  /// A range, e.g. `(width > 240px)`.
  Range {
    /// The name of the feature.
    name: MediaFeatureName<'i, FeatureId>,
    /// A comparator.
    operator: MediaFeatureComparison,
    /// The feature value.
    value: MediaFeatureValue<'i>,
  },
  /// An interval, e.g. `(120px < width < 240px)`.
  #[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
  Interval {
    /// The name of the feature.
    name: MediaFeatureName<'i, FeatureId>,
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

/// A [media feature](https://drafts.csswg.org/mediaqueries/#typedef-media-feature)
pub type MediaFeature<'i> = QueryFeature<'i, MediaFeatureId>;

impl<'i, FeatureId> ParseWithOptions<'i> for QueryFeature<'i, FeatureId>
where
  FeatureId: for<'x> Parse<'x> + std::fmt::Debug + PartialEq + ValueType + Clone,
{
  fn parse_with_options<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.try_parse(|input| Self::parse_name_first(input, options)) {
      Ok(res) => Ok(res),
      Err(
        err @ ParseError {
          kind: ParseErrorKind::Custom(ParserError::InvalidMediaQuery),
          ..
        },
      ) => Err(err),
      _ => Self::parse_value_first(input),
    }
  }
}

impl<'i, FeatureId> QueryFeature<'i, FeatureId>
where
  FeatureId: for<'x> Parse<'x> + std::fmt::Debug + PartialEq + ValueType + Clone,
{
  fn parse_name_first<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let (name, legacy_op) = MediaFeatureName::parse(input)?;

    let operator = input.try_parse(|input| consume_operation_or_colon(input, true));
    let operator = match operator {
      Err(..) => return Ok(QueryFeature::Boolean { name }),
      Ok(operator) => operator,
    };

    if operator.is_some() && legacy_op.is_some() {
      return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
    }

    let value = MediaFeatureValue::parse(input, name.value_type())?;
    if !value.check_type(name.value_type()) {
      if options.error_recovery {
        options.warn(ParseError {
          kind: ParseErrorKind::Custom(ParserError::InvalidMediaQuery),
          location: input.current_source_location(),
        });
      } else {
        return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
      }
    }

    if let Some(operator) = operator.or(legacy_op) {
      if !name.value_type().allows_ranges() {
        return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
      }

      Ok(QueryFeature::Range { name, operator, value })
    } else {
      Ok(QueryFeature::Plain { name, value })
    }
  }

  fn parse_value_first<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // We need to find the feature name first so we know the type.
    let start = input.state();
    let name = loop {
      if let Ok((name, legacy_op)) = MediaFeatureName::parse(input) {
        if legacy_op.is_some() {
          return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
        }
        break name;
      }
      if input.is_exhausted() {
        return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
      }
    };

    input.reset(&start);

    // Now we can parse the first value.
    let value = MediaFeatureValue::parse(input, name.value_type())?;
    let operator = consume_operation_or_colon(input, false)?;

    // Skip over the feature name again.
    {
      let (feature_name, _) = MediaFeatureName::parse(input)?;
      debug_assert_eq!(name, feature_name);
    }

    if !name.value_type().allows_ranges() || !value.check_type(name.value_type()) {
      return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
    }

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

      let end_value = MediaFeatureValue::parse(input, name.value_type())?;
      if !end_value.check_type(name.value_type()) {
        return Err(input.new_custom_error(ParserError::InvalidMediaQuery));
      }

      Ok(QueryFeature::Interval {
        name,
        start: value,
        start_operator,
        end: end_value,
        end_operator,
      })
    } else {
      let operator = operator.unwrap().opposite();
      Ok(QueryFeature::Range { name, operator, value })
    }
  }

  pub(crate) fn needs_parens(&self, parent_operator: Option<Operator>, targets: &Targets) -> bool {
    match self {
      QueryFeature::Interval { .. } => {
        should_compile!(targets, MediaIntervalSyntax) && parent_operator != Some(Operator::And)
      }
      QueryFeature::Range { operator, .. } => {
        should_compile!(targets, MediaRangeSyntax)
          && matches!(
            operator,
            MediaFeatureComparison::GreaterThan | MediaFeatureComparison::LessThan
          )
      }
      _ => false,
    }
  }

  fn negate(&self) -> Option<QueryFeature<'i, FeatureId>> {
    match self {
      QueryFeature::Range { name, operator, value } => Some(QueryFeature::Range {
        name: (*name).clone(),
        operator: operator.negate(),
        value: value.clone(),
      }),
      _ => None,
    }
  }
}

impl<'i, FeatureId: FeatureToCss> ToCss for QueryFeature<'i, FeatureId> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      QueryFeature::Boolean { name } => {
        dest.write_char('(')?;
        name.to_css(dest)?;
      }
      QueryFeature::Plain { name, value } => {
        dest.write_char('(')?;
        name.to_css(dest)?;
        dest.delim(':', false)?;
        value.to_css(dest)?;
      }
      QueryFeature::Range { name, operator, value } => {
        // If range syntax is unsupported, use min/max prefix if possible.
        if should_compile!(dest.targets.current, MediaRangeSyntax) {
          return write_min_max(operator, name, value, dest, false);
        }

        dest.write_char('(')?;
        name.to_css(dest)?;
        operator.to_css(dest)?;
        value.to_css(dest)?;
      }
      QueryFeature::Interval {
        name,
        start,
        start_operator,
        end,
        end_operator,
      } => {
        if should_compile!(dest.targets.current, MediaIntervalSyntax) {
          write_min_max(&start_operator.opposite(), name, start, dest, true)?;
          dest.write_str(" and ")?;
          return write_min_max(end_operator, name, end, dest, true);
        }

        dest.write_char('(')?;
        start.to_css(dest)?;
        start_operator.to_css(dest)?;
        name.to_css(dest)?;
        end_operator.to_css(dest)?;
        end.to_css(dest)?;
      }
    }

    dest.write_char(')')
  }
}

/// A media feature name.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(untagged))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum MediaFeatureName<'i, FeatureId> {
  /// A standard media query feature identifier.
  Standard(FeatureId),
  /// A custom author-defined environment variable.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Custom(DashedIdent<'i>),
  /// An unknown environment variable.
  Unknown(Ident<'i>),
}

impl<'i, FeatureId: for<'x> Parse<'x>> MediaFeatureName<'i, FeatureId> {
  /// Parses a media feature name.
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
  ) -> Result<(Self, Option<MediaFeatureComparison>), ParseError<'i, ParserError<'i>>> {
    let ident = input.expect_ident()?;

    if ident.starts_with("--") {
      return Ok((MediaFeatureName::Custom(DashedIdent(ident.into())), None));
    }

    let mut name = ident.as_ref();

    // Webkit places its prefixes before "min" and "max". Remove it first, and
    // re-add after removing min/max.
    let is_webkit = starts_with_ignore_ascii_case(&name, "-webkit-");
    if is_webkit {
      name = &name[8..];
    }

    let comparator = if starts_with_ignore_ascii_case(&name, "min-") {
      name = &name[4..];
      Some(MediaFeatureComparison::GreaterThanEqual)
    } else if starts_with_ignore_ascii_case(&name, "max-") {
      name = &name[4..];
      Some(MediaFeatureComparison::LessThanEqual)
    } else {
      None
    };

    let name = if is_webkit {
      Cow::Owned(format!("-webkit-{}", name))
    } else {
      Cow::Borrowed(name)
    };

    if let Ok(standard) = FeatureId::parse_string(&name) {
      return Ok((MediaFeatureName::Standard(standard), comparator));
    }

    Ok((MediaFeatureName::Unknown(Ident(ident.into())), None))
  }
}

mod private {
  use super::*;

  /// A trait for feature ids which can get a value type.
  pub trait ValueType {
    /// Returns the value type for this feature id.
    fn value_type(&self) -> MediaFeatureType;
  }
}

pub(crate) use private::ValueType;

impl<'i, FeatureId: ValueType> ValueType for MediaFeatureName<'i, FeatureId> {
  fn value_type(&self) -> MediaFeatureType {
    match self {
      Self::Standard(standard) => standard.value_type(),
      _ => MediaFeatureType::Unknown,
    }
  }
}

impl<'i, FeatureId: FeatureToCss> ToCss for MediaFeatureName<'i, FeatureId> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Self::Standard(v) => v.to_css(dest),
      Self::Custom(v) => v.to_css(dest),
      Self::Unknown(v) => v.to_css(dest),
    }
  }
}

impl<'i, FeatureId: FeatureToCss> FeatureToCss for MediaFeatureName<'i, FeatureId> {
  fn to_css_with_prefix<W>(&self, prefix: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Self::Standard(v) => v.to_css_with_prefix(prefix, dest),
      Self::Custom(v) => {
        dest.write_str(prefix)?;
        v.to_css(dest)
      }
      Self::Unknown(v) => {
        dest.write_str(prefix)?;
        v.to_css(dest)
      }
    }
  }
}

/// The type of a media feature.
#[derive(PartialEq)]
pub enum MediaFeatureType {
  /// A length value.
  Length,
  /// A number value.
  Number,
  /// An integer value.
  Integer,
  /// A boolean value, either 0 or 1.
  Boolean,
  /// A resolution.
  Resolution,
  /// A ratio.
  Ratio,
  /// An identifier.
  Ident,
  /// An unknown type.
  Unknown,
}

impl MediaFeatureType {
  fn allows_ranges(&self) -> bool {
    use MediaFeatureType::*;
    match self {
      Length => true,
      Number => true,
      Integer => true,
      Boolean => false,
      Resolution => true,
      Ratio => true,
      Ident => false,
      Unknown => true,
    }
  }
}

macro_rules! define_query_features {
  (
    $(#[$outer:meta])*
    $vis:vis enum $name:ident {
      $(
        $(#[$meta: meta])*
        $str: literal: $id: ident = $ty: ident,
      )+
    }
  ) => {
    crate::macros::enum_property! {
      $(#[$outer])*
      $vis enum $name {
        $(
          $(#[$meta])*
          $str: $id,
        )+
      }
    }

    impl ValueType for $name {
      fn value_type(&self) -> MediaFeatureType {
        match self {
          $(
            Self::$id => MediaFeatureType::$ty,
          )+
        }
      }
    }
  }
}

pub(crate) use define_query_features;

define_query_features! {
  /// A media query feature identifier.
  pub enum MediaFeatureId {
    /// The [width](https://w3c.github.io/csswg-drafts/mediaqueries-5/#width) media feature.
    "width": Width = Length,
    /// The [height](https://w3c.github.io/csswg-drafts/mediaqueries-5/#height) media feature.
    "height": Height = Length,
    /// The [aspect-ratio](https://w3c.github.io/csswg-drafts/mediaqueries-5/#aspect-ratio) media feature.
    "aspect-ratio": AspectRatio = Ratio,
    /// The [orientation](https://w3c.github.io/csswg-drafts/mediaqueries-5/#orientation) media feature.
    "orientation": Orientation = Ident,
    /// The [overflow-block](https://w3c.github.io/csswg-drafts/mediaqueries-5/#overflow-block) media feature.
    "overflow-block": OverflowBlock = Ident,
    /// The [overflow-inline](https://w3c.github.io/csswg-drafts/mediaqueries-5/#overflow-inline) media feature.
    "overflow-inline": OverflowInline = Ident,
    /// The [horizontal-viewport-segments](https://w3c.github.io/csswg-drafts/mediaqueries-5/#horizontal-viewport-segments) media feature.
    "horizontal-viewport-segments": HorizontalViewportSegments = Integer,
    /// The [vertical-viewport-segments](https://w3c.github.io/csswg-drafts/mediaqueries-5/#vertical-viewport-segments) media feature.
    "vertical-viewport-segments": VerticalViewportSegments = Integer,
    /// The [display-mode](https://w3c.github.io/csswg-drafts/mediaqueries-5/#display-mode) media feature.
    "display-mode": DisplayMode = Ident,
    /// The [resolution](https://w3c.github.io/csswg-drafts/mediaqueries-5/#resolution) media feature.
    "resolution": Resolution = Resolution, // | infinite??
    /// The [scan](https://w3c.github.io/csswg-drafts/mediaqueries-5/#scan) media feature.
    "scan": Scan = Ident,
    /// The [grid](https://w3c.github.io/csswg-drafts/mediaqueries-5/#grid) media feature.
    "grid": Grid = Boolean,
    /// The [update](https://w3c.github.io/csswg-drafts/mediaqueries-5/#update) media feature.
    "update": Update = Ident,
    /// The [environment-blending](https://w3c.github.io/csswg-drafts/mediaqueries-5/#environment-blending) media feature.
    "environment-blending": EnvironmentBlending = Ident,
    /// The [color](https://w3c.github.io/csswg-drafts/mediaqueries-5/#color) media feature.
    "color": Color = Integer,
    /// The [color-index](https://w3c.github.io/csswg-drafts/mediaqueries-5/#color-index) media feature.
    "color-index": ColorIndex = Integer,
    /// The [monochrome](https://w3c.github.io/csswg-drafts/mediaqueries-5/#monochrome) media feature.
    "monochrome": Monochrome = Integer,
    /// The [color-gamut](https://w3c.github.io/csswg-drafts/mediaqueries-5/#color-gamut) media feature.
    "color-gamut": ColorGamut = Ident,
    /// The [dynamic-range](https://w3c.github.io/csswg-drafts/mediaqueries-5/#dynamic-range) media feature.
    "dynamic-range": DynamicRange = Ident,
    /// The [inverted-colors](https://w3c.github.io/csswg-drafts/mediaqueries-5/#inverted-colors) media feature.
    "inverted-colors": InvertedColors = Ident,
    /// The [pointer](https://w3c.github.io/csswg-drafts/mediaqueries-5/#pointer) media feature.
    "pointer": Pointer = Ident,
    /// The [hover](https://w3c.github.io/csswg-drafts/mediaqueries-5/#hover) media feature.
    "hover": Hover = Ident,
    /// The [any-pointer](https://w3c.github.io/csswg-drafts/mediaqueries-5/#any-pointer) media feature.
    "any-pointer": AnyPointer = Ident,
    /// The [any-hover](https://w3c.github.io/csswg-drafts/mediaqueries-5/#any-hover) media feature.
    "any-hover": AnyHover = Ident,
    /// The [nav-controls](https://w3c.github.io/csswg-drafts/mediaqueries-5/#nav-controls) media feature.
    "nav-controls": NavControls = Ident,
    /// The [video-color-gamut](https://w3c.github.io/csswg-drafts/mediaqueries-5/#video-color-gamut) media feature.
    "video-color-gamut": VideoColorGamut = Ident,
    /// The [video-dynamic-range](https://w3c.github.io/csswg-drafts/mediaqueries-5/#video-dynamic-range) media feature.
    "video-dynamic-range": VideoDynamicRange = Ident,
    /// The [scripting](https://w3c.github.io/csswg-drafts/mediaqueries-5/#scripting) media feature.
    "scripting": Scripting = Ident,
    /// The [prefers-reduced-motion](https://w3c.github.io/csswg-drafts/mediaqueries-5/#prefers-reduced-motion) media feature.
    "prefers-reduced-motion": PrefersReducedMotion = Ident,
    /// The [prefers-reduced-transparency](https://w3c.github.io/csswg-drafts/mediaqueries-5/#prefers-reduced-transparency) media feature.
    "prefers-reduced-transparency": PrefersReducedTransparency = Ident,
    /// The [prefers-contrast](https://w3c.github.io/csswg-drafts/mediaqueries-5/#prefers-contrast) media feature.
    "prefers-contrast": PrefersContrast = Ident,
    /// The [forced-colors](https://w3c.github.io/csswg-drafts/mediaqueries-5/#forced-colors) media feature.
    "forced-colors": ForcedColors = Ident,
    /// The [prefers-color-scheme](https://w3c.github.io/csswg-drafts/mediaqueries-5/#prefers-color-scheme) media feature.
    "prefers-color-scheme": PrefersColorScheme = Ident,
    /// The [prefers-reduced-data](https://w3c.github.io/csswg-drafts/mediaqueries-5/#prefers-reduced-data) media feature.
    "prefers-reduced-data": PrefersReducedData = Ident,
    /// The [device-width](https://w3c.github.io/csswg-drafts/mediaqueries-5/#device-width) media feature.
    "device-width": DeviceWidth = Length,
    /// The [device-height](https://w3c.github.io/csswg-drafts/mediaqueries-5/#device-height) media feature.
    "device-height": DeviceHeight = Length,
    /// The [device-aspect-ratio](https://w3c.github.io/csswg-drafts/mediaqueries-5/#device-aspect-ratio) media feature.
    "device-aspect-ratio": DeviceAspectRatio = Ratio,

    /// The non-standard -webkit-device-pixel-ratio media feature.
    "-webkit-device-pixel-ratio": WebKitDevicePixelRatio = Number,
    /// The non-standard -moz-device-pixel-ratio media feature.
    "-moz-device-pixel-ratio": MozDevicePixelRatio = Number,

    // TODO: parse non-standard media queries?
    // -moz-device-orientation
    // -webkit-transform-3d
  }
}

pub(crate) trait FeatureToCss: ToCss {
  fn to_css_with_prefix<W>(&self, prefix: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write;
}

impl FeatureToCss for MediaFeatureId {
  fn to_css_with_prefix<W>(&self, prefix: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      MediaFeatureId::WebKitDevicePixelRatio => {
        dest.write_str("-webkit-")?;
        dest.write_str(prefix)?;
        dest.write_str("device-pixel-ratio")
      }
      _ => {
        dest.write_str(prefix)?;
        self.to_css(dest)
      }
    }
  }
}

#[inline]
fn write_min_max<W, FeatureId: FeatureToCss>(
  operator: &MediaFeatureComparison,
  name: &MediaFeatureName<FeatureId>,
  value: &MediaFeatureValue,
  dest: &mut Printer<W>,
  is_range: bool,
) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  let prefix = match operator {
    MediaFeatureComparison::GreaterThan => {
      if is_range {
        dest.write_char('(')?;
      }
      dest.write_str("not ")?;
      Some("max-")
    }
    MediaFeatureComparison::GreaterThanEqual => Some("min-"),
    MediaFeatureComparison::LessThan => {
      if is_range {
        dest.write_char('(')?;
      }
      dest.write_str("not ")?;
      Some("min-")
    }
    MediaFeatureComparison::LessThanEqual => Some("max-"),
    MediaFeatureComparison::Equal => None,
  };

  dest.write_char('(')?;
  if let Some(prefix) = prefix {
    name.to_css_with_prefix(prefix, dest)?;
  } else {
    name.to_css(dest)?;
  }

  dest.delim(':', false)?;
  value.to_css(dest)?;

  if is_range
    && matches!(
      operator,
      MediaFeatureComparison::GreaterThan | MediaFeatureComparison::LessThan
    )
  {
    dest.write_char(')')?;
  }

  dest.write_char(')')?;
  Ok(())
}

/// [media feature value](https://drafts.csswg.org/mediaqueries/#typedef-mf-value) within a media query.
///
/// See [MediaFeature](MediaFeature).
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit), visit(visit_media_feature_value, MEDIA_QUERIES))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum MediaFeatureValue<'i> {
  /// A length value.
  Length(Length),
  /// A number value.
  Number(CSSNumber),
  /// An integer value.
  Integer(CSSInteger),
  /// A boolean value.
  Boolean(bool),
  /// A resolution.
  Resolution(Resolution),
  /// A ratio.
  Ratio(Ratio),
  /// An identifier.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Ident(Ident<'i>),
  /// An environment variable reference.
  Env(EnvironmentVariable<'i>),
}

impl<'i> MediaFeatureValue<'i> {
  fn value_type(&self) -> MediaFeatureType {
    use MediaFeatureValue::*;
    match self {
      Length(..) => MediaFeatureType::Length,
      Number(..) => MediaFeatureType::Number,
      Integer(..) => MediaFeatureType::Integer,
      Boolean(..) => MediaFeatureType::Boolean,
      Resolution(..) => MediaFeatureType::Resolution,
      Ratio(..) => MediaFeatureType::Ratio,
      Ident(..) => MediaFeatureType::Ident,
      Env(..) => MediaFeatureType::Unknown,
    }
  }

  fn check_type(&self, expected_type: MediaFeatureType) -> bool {
    match (expected_type, self.value_type()) {
      (_, MediaFeatureType::Unknown) | (MediaFeatureType::Unknown, _) => true,
      (a, b) => a == b,
    }
  }
}

impl<'i> MediaFeatureValue<'i> {
  /// Parses a single media query feature value, with an expected type.
  /// If the type is unknown, pass MediaFeatureType::Unknown instead.
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
    expected_type: MediaFeatureType,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(value) = input.try_parse(|input| Self::parse_known(input, expected_type)) {
      return Ok(value);
    }

    Self::parse_unknown(input)
  }

  fn parse_known<'t>(
    input: &mut Parser<'i, 't>,
    expected_type: MediaFeatureType,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match expected_type {
      MediaFeatureType::Boolean => {
        let value = CSSInteger::parse(input)?;
        if value != 0 && value != 1 {
          return Err(input.new_custom_error(ParserError::InvalidValue));
        }
        Ok(MediaFeatureValue::Boolean(value == 1))
      }
      MediaFeatureType::Number => Ok(MediaFeatureValue::Number(CSSNumber::parse(input)?)),
      MediaFeatureType::Integer => Ok(MediaFeatureValue::Integer(CSSInteger::parse(input)?)),
      MediaFeatureType::Length => Ok(MediaFeatureValue::Length(Length::parse(input)?)),
      MediaFeatureType::Resolution => Ok(MediaFeatureValue::Resolution(Resolution::parse(input)?)),
      MediaFeatureType::Ratio => Ok(MediaFeatureValue::Ratio(Ratio::parse(input)?)),
      MediaFeatureType::Ident => Ok(MediaFeatureValue::Ident(Ident::parse(input)?)),
      MediaFeatureType::Unknown => Err(input.new_custom_error(ParserError::InvalidValue)),
    }
  }

  fn parse_unknown<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Ratios are ambiguous with numbers because the second param is optional (e.g. 2/1 == 2).
    // We require the / delimiter when parsing ratios so that 2/1 ends up as a ratio and 2 is
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

    if let Ok(env) = input.try_parse(|input| EnvironmentVariable::parse(input, &ParserOptions::default(), 0)) {
      return Ok(MediaFeatureValue::Env(env));
    }

    let ident = Ident::parse(input)?;
    Ok(MediaFeatureValue::Ident(ident))
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
      MediaFeatureValue::Integer(num) => num.to_css(dest),
      MediaFeatureValue::Boolean(b) => {
        if *b {
          dest.write_char('1')
        } else {
          dest.write_char('0')
        }
      }
      MediaFeatureValue::Resolution(res) => res.to_css(dest),
      MediaFeatureValue::Ratio(ratio) => ratio.to_css(dest),
      MediaFeatureValue::Ident(id) => {
        id.to_css(dest)?;
        Ok(())
      }
      MediaFeatureValue::Env(env) => env.to_css(dest, false),
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
  seen: &mut HashSet<DashedIdent<'i>>,
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
      match &**cond {
        MediaCondition::Not(cond) => {
          *condition = (**cond).clone();
        }
        _ => {}
      }
    }
    MediaCondition::Operation { conditions, .. } => {
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
    MediaCondition::Feature(QueryFeature::Boolean { name }) => {
      let name = match name {
        MediaFeatureName::Custom(name) => name,
        _ => return Ok(true),
      };

      if seen.contains(name) {
        return Err(ErrorWithLocation {
          kind: MinifyErrorKind::CircularCustomMedia { name: name.to_string() },
          loc,
        });
      }

      let rule = custom_media.get(&name.0).ok_or_else(|| ErrorWithLocation {
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
            // Parentheses are required around the condition unless there is a single media feature.
            match condition {
              MediaCondition::Feature(..) => Some(condition),
              _ => Some(condition),
            }
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
        *condition = MediaCondition::Operation {
          conditions,
          operator: Operator::Or,
        };
      }
    }
    _ => {}
  }

  Ok(true)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{
    stylesheet::PrinterOptions,
    targets::{Browsers, Targets},
  };

  fn parse(s: &str) -> MediaQuery<'_> {
    let mut input = ParserInput::new(&s);
    let mut parser = Parser::new(&mut input);
    MediaQuery::parse_with_options(&mut parser, &ParserOptions::default()).unwrap()
  }

  fn and(a: &str, b: &str) -> String {
    let mut a = parse(a);
    let b = parse(b);
    a.and(&b).unwrap();
    a.to_css_string(PrinterOptions::default()).unwrap()
  }

  #[test]
  fn test_and() {
    assert_eq!(and("(min-width: 250px)", "(color)"), "(width >= 250px) and (color)");
    assert_eq!(
      and("(min-width: 250px) or (color)", "(orientation: landscape)"),
      "((width >= 250px) or (color)) and (orientation: landscape)"
    );
    assert_eq!(
      and("(min-width: 250px) and (color)", "(orientation: landscape)"),
      "(width >= 250px) and (color) and (orientation: landscape)"
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
    assert_eq!(and("print", "(min-width: 250px)"), "print and (width >= 250px)");
    assert_eq!(and("(min-width: 250px)", "print"), "print and (width >= 250px)");
    assert_eq!(
      and("print and (min-width: 250px)", "(color)"),
      "print and (width >= 250px) and (color)"
    );
    assert_eq!(and("all", "only screen"), "only screen");
    assert_eq!(and("only screen", "all"), "only screen");
    assert_eq!(and("print", "print"), "print");
  }

  #[test]
  fn test_negated_interval_parens() {
    let media_query = parse("screen and not (200px <= width < 500px)");
    let printer_options = PrinterOptions {
      targets: Targets {
        browsers: Some(Browsers {
          chrome: Some(95 << 16),
          ..Default::default()
        }),
        ..Default::default()
      },
      ..Default::default()
    };
    assert_eq!(
      media_query.to_css_string(printer_options).unwrap(),
      "screen and not ((min-width: 200px) and (not (min-width: 500px)))"
    );
  }
}
