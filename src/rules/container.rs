//! The `@container` rule.

#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use cssparser::*;

use super::Location;
use super::{CssRuleList, MinifyContext};
use crate::error::{MinifyError, ParserError, PrinterError};
use crate::media_query::{
  define_query_features, operation_to_css, parse_query_condition, to_css_with_parens_if_needed, FeatureToCss,
  MediaFeatureType, Operator, QueryCondition, QueryConditionFlags, QueryFeature, ValueType,
};
use crate::parser::{DefaultAtRule, ParserOptions};
use crate::printer::Printer;
use crate::properties::custom::TokenList;
use crate::properties::{Property, PropertyId};
#[cfg(feature = "serde")]
use crate::serialization::ValueWrapper;
use crate::targets::{Features, Targets};
use crate::traits::{Parse, ParseWithOptions, ToCss};
use crate::values::ident::{CustomIdent, DashedIdent};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@container](https://drafts.csswg.org/css-contain-3/#container-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct ContainerRule<'i, R = DefaultAtRule> {
  /// The name of the container.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Option<ContainerName<'i>>,
  /// The container condition.
  pub condition: Option<ContainerCondition<'i>>,
  /// The rules within the `@container` rule.
  pub rules: CssRuleList<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

/// Represents a container condition.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ContainerCondition<'i> {
  /// A size container feature, implicitly parenthesized.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<ContainerSizeFeature>"))]
  Feature(ContainerSizeFeature<'i>),
  /// A negation of a condition.
  #[cfg_attr(feature = "visitor", skip_type)]
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<Box<ContainerCondition>>"))]
  Not(Box<ContainerCondition<'i>>),
  /// A set of joint operations.
  #[cfg_attr(feature = "visitor", skip_type)]
  Operation {
    /// The operator for the conditions.
    operator: Operator,
    /// The conditions for the operator.
    conditions: Vec<ContainerCondition<'i>>,
  },
  /// A style query.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<StyleQuery>"))]
  Style(StyleQuery<'i>),
  /// A scroll state query.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<ScrollStateQuery>"))]
  ScrollState(ScrollStateQuery<'i>),
  /// An anchor positioning query.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<AnchoredQuery>"))]
  Anchored(AnchoredQuery<'i>),
  /// Unknown tokens.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<TokenList>"))]
  Unknown(TokenList<'i>),
}

/// A container query size feature.
pub type ContainerSizeFeature<'i> = QueryFeature<'i, ContainerSizeFeatureId>;

define_query_features! {
  /// A container query size feature identifier.
  pub enum ContainerSizeFeatureId {
    /// The [width](https://w3c.github.io/csswg-drafts/css-contain-3/#width) size container feature.
    "width": Width = Length,
    /// The [height](https://w3c.github.io/csswg-drafts/css-contain-3/#height) size container feature.
    "height": Height = Length,
    /// The [inline-size](https://w3c.github.io/csswg-drafts/css-contain-3/#inline-size) size container feature.
    "inline-size": InlineSize = Length,
    /// The [block-size](https://w3c.github.io/csswg-drafts/css-contain-3/#block-size) size container feature.
    "block-size": BlockSize = Length,
    /// The [aspect-ratio](https://w3c.github.io/csswg-drafts/css-contain-3/#aspect-ratio) size container feature.
    "aspect-ratio": AspectRatio = Ratio,
    /// The [orientation](https://w3c.github.io/csswg-drafts/css-contain-3/#orientation) size container feature.
    "orientation": Orientation = Ident,
  }
}

impl FeatureToCss for ContainerSizeFeatureId {
  fn to_css_with_prefix<W>(&self, prefix: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_str(prefix)?;
    self.to_css(dest)
  }
}

/// Represents a style query within a container condition.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum StyleQuery<'i> {
  /// A property declaration.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<Property>"))]
  Declaration(Property<'i>),
  /// A property name, without a value.
  /// This matches if the property value is different from the initial value.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<PropertyId>"))]
  Property(PropertyId<'i>),
  /// A negation of a condition.
  #[cfg_attr(feature = "visitor", skip_type)]
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<Box<StyleQuery>>"))]
  Not(Box<StyleQuery<'i>>),
  /// A set of joint operations.
  #[cfg_attr(feature = "visitor", skip_type)]
  Operation {
    /// The operator for the conditions.
    operator: Operator,
    /// The conditions for the operator.
    conditions: Vec<StyleQuery<'i>>,
  },
}

/// Represents a scroll state query within a container condition.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ScrollStateQuery<'i> {
  /// A size container feature, implicitly parenthesized.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<ScrollStateFeature>"))]
  Feature(ScrollStateFeature<'i>),
  /// A negation of a condition.
  #[cfg_attr(feature = "visitor", skip_type)]
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<Box<ScrollStateQuery>>"))]
  Not(Box<ScrollStateQuery<'i>>),
  /// A set of joint operations.
  #[cfg_attr(feature = "visitor", skip_type)]
  Operation {
    /// The operator for the conditions.
    operator: Operator,
    /// The conditions for the operator.
    conditions: Vec<ScrollStateQuery<'i>>,
  },
}

/// A container query size feature.
pub type ScrollStateFeature<'i> = QueryFeature<'i, ScrollStateFeatureId>;

define_query_features! {
  /// A container query scroll state feature identifier.
  pub enum ScrollStateFeatureId {
    /// The [stuck](https://drafts.csswg.org/css-conditional-5/#stuck) scroll state feature.
    "stuck": Stuck = Ident,
    /// The [snapped](https://drafts.csswg.org/css-conditional-5/#snapped) scroll state feature.
    "snapped": Snapped = Ident,
    /// The [scrollable](https://drafts.csswg.org/css-conditional-5/#scrollable) scroll state feature.
    "scrollable": Scrollable = Ident,
    /// The [scrolled](https://drafts.csswg.org/css-conditional-5/#scrolled) scroll state feature.
    "scrolled": Scrolled = Ident,
  }
}

impl FeatureToCss for ScrollStateFeatureId {
  fn to_css_with_prefix<W>(&self, prefix: &str, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_str(prefix)?;
    self.to_css(dest)
  }
}

bitflags! {
  /// A [`<try-tactic>`](https://drafts.csswg.org/css-anchor-position-1/#typedef-try-tactic) value,
  /// used in the `position-try-fallbacks` property and the `anchored(fallback: ...)` container query feature.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "Vec<TryTacticFlag>", into = "Vec<TryTacticFlag>"))]
  #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct TryTactic: u8 {
    /// Flips the position across the block axis.
    const FlipBlock  = 0b001;
    /// Flips the position across the inline axis.
    const FlipInline = 0b010;
    /// Swaps the block and inline axes.
    const FlipStart  = 0b100;
  }
}

impl<'i> Parse<'i> for TryTactic {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut value = TryTactic::empty();
    while let Ok(ident) = input.try_parse(|input| input.expect_ident_cloned()) {
      let location = input.current_source_location();
      let flag = match_ignore_ascii_case! { &ident,
        "flip-block" if !value.contains(TryTactic::FlipBlock) => TryTactic::FlipBlock,
        "flip-inline" if !value.contains(TryTactic::FlipInline) => TryTactic::FlipInline,
        "flip-start" if !value.contains(TryTactic::FlipStart) => TryTactic::FlipStart,
        _ => return Err(location.new_unexpected_token_error(Token::Ident(ident.clone()))),
      };
      value |= flag;
    }

    if value.is_empty() {
      return Err(input.new_error_for_next_token());
    }
    Ok(value)
  }
}

impl ToCss for TryTactic {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut needs_space = false;
    macro_rules! val {
      ($flag:ident, $name:expr) => {
        #[allow(unused_assignments)]
        if self.contains(TryTactic::$flag) {
          if needs_space {
            dest.write_char(' ')?;
          }
          dest.write_str($name)?;
          needs_space = true;
        }
      };
    }
    val!(FlipBlock, "flip-block");
    val!(FlipInline, "flip-inline");
    val!(FlipStart, "flip-start");
    Ok(())
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "kebab-case"))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum TryTacticFlag {
  FlipBlock,
  FlipInline,
  FlipStart,
}

impl From<TryTactic> for Vec<TryTacticFlag> {
  fn from(t: TryTactic) -> Self {
    let mut v = Vec::new();
    if t.contains(TryTactic::FlipBlock) {
      v.push(TryTacticFlag::FlipBlock);
    }
    if t.contains(TryTactic::FlipInline) {
      v.push(TryTacticFlag::FlipInline);
    }
    if t.contains(TryTactic::FlipStart) {
      v.push(TryTacticFlag::FlipStart);
    }
    v
  }
}

impl From<Vec<TryTacticFlag>> for TryTactic {
  fn from(flags: Vec<TryTacticFlag>) -> Self {
    let mut res = TryTactic::empty();
    for f in flags {
      res |= match f {
        TryTacticFlag::FlipBlock => TryTactic::FlipBlock,
        TryTacticFlag::FlipInline => TryTactic::FlipInline,
        TryTacticFlag::FlipStart => TryTactic::FlipStart,
      }
    }
    res
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for TryTactic {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    Vec::<TryTacticFlag>::json_schema(gen)
  }

  fn schema_name() -> String {
    "TryTactic".into()
  }
}

/// The value of the [`anchored(fallback: ...)`](https://drafts.csswg.org/css-anchor-position-2/#fallback-feature)
/// container query feature.
///
/// Note: the `<position-area>` form is not yet supported.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AnchoredFallbackValue<'i> {
  /// The `none` keyword.
  None,
  /// A `<dashed-ident>` referencing a `@position-try` rule name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Ident(DashedIdent<'i>),
  /// A `<try-tactic>` keyword combination.
  Tactic(TryTactic),
}

impl<'i> Parse<'i> for AnchoredFallbackValue<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(AnchoredFallbackValue::None);
    }

    if let Ok(ident) = input.try_parse(DashedIdent::parse) {
      return Ok(AnchoredFallbackValue::Ident(ident));
    }

    Ok(AnchoredFallbackValue::Tactic(TryTactic::parse(input)?))
  }
}

impl<'i> ToCss for AnchoredFallbackValue<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      AnchoredFallbackValue::None => dest.write_str("none"),
      AnchoredFallbackValue::Ident(ident) => ident.to_css(dest),
      AnchoredFallbackValue::Tactic(tactic) => tactic.to_css(dest),
    }
  }
}

/// An anchored container feature.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "name", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AnchoredFeature<'i> {
  /// The [fallback](https://drafts.csswg.org/css-anchor-position-2/#fallback-feature) feature.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Fallback(AnchoredFallbackValue<'i>),
}

impl<'i> Parse<'i> for AnchoredFeature<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let name = input.expect_ident_cloned()?;
    match_ignore_ascii_case! { &name,
      "fallback" => {
        input.expect_colon()?;
        input.skip_whitespace();
        Ok(AnchoredFeature::Fallback(AnchoredFallbackValue::parse(input)?))
      },
      _ => Err(location.new_unexpected_token_error(Token::Ident(name.clone()))),
    }
  }
}

impl<'i> ToCss for AnchoredFeature<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      AnchoredFeature::Fallback(value) => {
        dest.write_str("fallback")?;
        dest.delim(':', false)?;
        value.to_css(dest)
      }
    }
  }
}

/// Represents an anchored query within a container condition.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AnchoredQuery<'i> {
  /// An anchored feature query.
  #[cfg_attr(feature = "serde", serde(borrow, with = "ValueWrapper::<AnchoredFeature>"))]
  Feature(AnchoredFeature<'i>),
  /// A negation of a condition.
  #[cfg_attr(feature = "visitor", skip_type)]
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<Box<AnchoredQuery>>"))]
  Not(Box<AnchoredQuery<'i>>),
  /// A set of joint operations.
  #[cfg_attr(feature = "visitor", skip_type)]
  Operation {
    /// The operator for the conditions.
    operator: Operator,
    /// The conditions for the operator.
    conditions: Vec<AnchoredQuery<'i>>,
  },
}

impl<'i> QueryCondition<'i> for ContainerCondition<'i> {
  #[inline]
  fn parse_feature<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let feature = QueryFeature::parse_with_options(input, options)?;
    Ok(Self::Feature(feature))
  }

  #[inline]
  fn create_negation(condition: Box<ContainerCondition<'i>>) -> Self {
    Self::Not(condition)
  }

  #[inline]
  fn create_operation(operator: Operator, conditions: Vec<Self>) -> Self {
    Self::Operation { operator, conditions }
  }

  fn parse_style_query<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_nested_block(|input| {
      if let Ok(res) =
        input.try_parse(|input| parse_query_condition(input, QueryConditionFlags::ALLOW_OR, options))
      {
        return Ok(Self::Style(res));
      }

      Ok(Self::Style(StyleQuery::parse_feature(input, options)?))
    })
  }

  fn parse_scroll_state_query<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_nested_block(|input| {
      if let Ok(res) =
        input.try_parse(|input| parse_query_condition(input, QueryConditionFlags::ALLOW_OR, options))
      {
        return Ok(Self::ScrollState(res));
      }

      Ok(Self::ScrollState(ScrollStateQuery::parse_feature(input, options)?))
    })
  }

  fn parse_anchored_query<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_nested_block(|input| {
      if let Ok(res) = input.try_parse(|input| {
        parse_query_condition::<AnchoredQuery<'i>>(input, QueryConditionFlags::ALLOW_OR, options)
      }) {
        return Ok(Self::Anchored(res));
      }

      Ok(Self::Anchored(AnchoredQuery::parse_feature(input, options)?))
    })
  }

  fn needs_parens(&self, parent_operator: Option<Operator>, targets: &Targets) -> bool {
    match self {
      ContainerCondition::Not(_) => true,
      ContainerCondition::Operation { operator, .. } => Some(*operator) != parent_operator,
      ContainerCondition::Feature(f) => f.needs_parens(parent_operator, targets),
      ContainerCondition::Style(_) => false,
      ContainerCondition::ScrollState(_) => false,
      ContainerCondition::Anchored(_) => false,
      ContainerCondition::Unknown(_) => false,
    }
  }
}

impl<'i> QueryCondition<'i> for AnchoredQuery<'i> {
  #[inline]
  fn parse_feature<'t>(
    input: &mut Parser<'i, 't>,
    _options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    Ok(Self::Feature(AnchoredFeature::parse(input)?))
  }

  #[inline]
  fn create_negation(condition: Box<Self>) -> Self {
    Self::Not(condition)
  }

  #[inline]
  fn create_operation(operator: Operator, conditions: Vec<Self>) -> Self {
    Self::Operation { operator, conditions }
  }

  fn needs_parens(&self, parent_operator: Option<Operator>, _targets: &Targets) -> bool {
    match self {
      AnchoredQuery::Not(_) => true,
      AnchoredQuery::Operation { operator, .. } => Some(*operator) != parent_operator,
      AnchoredQuery::Feature(_) => true,
    }
  }
}

impl<'i> QueryCondition<'i> for ScrollStateQuery<'i> {
  #[inline]
  fn parse_feature<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let feature = QueryFeature::parse_with_options(input, options)?;
    Ok(Self::Feature(feature))
  }

  #[inline]
  fn create_negation(condition: Box<Self>) -> Self {
    Self::Not(condition)
  }

  #[inline]
  fn create_operation(operator: Operator, conditions: Vec<Self>) -> Self {
    Self::Operation { operator, conditions }
  }

  fn needs_parens(&self, parent_operator: Option<Operator>, targets: &Targets) -> bool {
    match self {
      ScrollStateQuery::Not(_) => true,
      ScrollStateQuery::Operation { operator, .. } => Some(*operator) != parent_operator,
      ScrollStateQuery::Feature(f) => f.needs_parens(parent_operator, targets),
    }
  }
}

impl<'i> QueryCondition<'i> for StyleQuery<'i> {
  #[inline]
  fn parse_feature<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let property_id = PropertyId::parse(input)?;
    if input.try_parse(|input| input.expect_colon()).is_ok() {
      input.skip_whitespace();
      let feature = Self::Declaration(Property::parse(property_id, input, options)?);
      let _ = input.try_parse(|input| parse_important(input));
      Ok(feature)
    } else {
      Ok(Self::Property(property_id))
    }
  }

  #[inline]
  fn create_negation(condition: Box<Self>) -> Self {
    Self::Not(condition)
  }

  #[inline]
  fn create_operation(operator: Operator, conditions: Vec<Self>) -> Self {
    Self::Operation { operator, conditions }
  }

  fn needs_parens(&self, parent_operator: Option<Operator>, _targets: &Targets) -> bool {
    match self {
      StyleQuery::Not(_) => true,
      StyleQuery::Operation { operator, .. } => Some(*operator) != parent_operator,
      StyleQuery::Declaration(_) | StyleQuery::Property(_) => true,
    }
  }
}

impl<'i> ParseWithOptions<'i> for ContainerCondition<'i> {
  fn parse_with_options<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input
      .try_parse(|input| {
        parse_query_condition(
          input,
          QueryConditionFlags::ALLOW_OR
            | QueryConditionFlags::ALLOW_STYLE
            | QueryConditionFlags::ALLOW_SCROLL_STATE
            | QueryConditionFlags::ALLOW_ANCHORED,
          options,
        )
      })
      .or_else(|e| {
        if options.error_recovery {
          options.warn(e);
          Ok(ContainerCondition::Unknown(TokenList::parse(input, options, 0)?))
        } else {
          Err(e)
        }
      })
  }
}

impl<'i> ToCss for ContainerCondition<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match *self {
      ContainerCondition::Feature(ref f) => f.to_css(dest),
      ContainerCondition::Not(ref c) => {
        dest.write_str("not ")?;
        to_css_with_parens_if_needed(&**c, dest, c.needs_parens(None, &dest.targets.current))
      }
      ContainerCondition::Operation {
        ref conditions,
        operator,
      } => operation_to_css(operator, conditions, dest),
      ContainerCondition::Style(ref query) => {
        dest.write_str("style(")?;
        query.to_css(dest)?;
        dest.write_char(')')
      }
      ContainerCondition::ScrollState(ref query) => {
        let needs_parens = !matches!(query, ScrollStateQuery::Feature(_));
        dest.write_str("scroll-state")?;
        if needs_parens {
          dest.write_char('(')?;
        }
        query.to_css(dest)?;
        if needs_parens {
          dest.write_char(')')?;
        }
        Ok(())
      }
      ContainerCondition::Anchored(ref query) => {
        dest.write_str("anchored(")?;
        query.to_css(dest)?;
        dest.write_char(')')
      }
      ContainerCondition::Unknown(ref tokens) => tokens.to_css(dest, false),
    }
  }
}

impl<'i> ToCss for StyleQuery<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match *self {
      StyleQuery::Declaration(ref f) => f.to_css(dest, false),
      StyleQuery::Property(ref f) => f.to_css(dest),
      StyleQuery::Not(ref c) => {
        dest.write_str("not ")?;
        to_css_with_parens_if_needed(&**c, dest, c.needs_parens(None, &dest.targets.current))
      }
      StyleQuery::Operation {
        ref conditions,
        operator,
      } => operation_to_css(operator, conditions, dest),
    }
  }
}

impl<'i> ToCss for ScrollStateQuery<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match *self {
      ScrollStateQuery::Feature(ref f) => f.to_css(dest),
      ScrollStateQuery::Not(ref c) => {
        dest.write_str("not ")?;
        to_css_with_parens_if_needed(&**c, dest, c.needs_parens(None, &dest.targets.current))
      }
      ScrollStateQuery::Operation {
        ref conditions,
        operator,
      } => operation_to_css(operator, conditions, dest),
    }
  }
}

impl<'i> ToCss for AnchoredQuery<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match *self {
      AnchoredQuery::Feature(ref f) => f.to_css(dest),
      AnchoredQuery::Not(ref c) => {
        dest.write_str("not ")?;
        to_css_with_parens_if_needed(&**c, dest, c.needs_parens(None, &dest.targets.current))
      }
      AnchoredQuery::Operation {
        ref conditions,
        operator,
      } => operation_to_css(operator, conditions, dest),
    }
  }
}

/// A [`<container-name>`](https://drafts.csswg.org/css-contain-3/#typedef-container-name) in a `@container` rule.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ContainerName<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CustomIdent<'i>);

impl<'i> Parse<'i> for ContainerName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let ident = CustomIdent::parse(input)?;
    match_ignore_ascii_case! { &*ident.0,
      "none" | "and" | "not" | "or" => Err(input.new_unexpected_token_error(Token::Ident(ident.0.as_ref().to_owned().into()))),
      _ => Ok(ContainerName(ident))
    }
  }
}

impl<'i> ToCss for ContainerName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    // Container name should not be hashed
    // https://github.com/vercel/next.js/issues/71233
    self.0.to_css_with_options(
      dest,
      match &dest.css_module {
        Some(css_module) => css_module.config.container,
        None => false,
      },
    )
  }
}

impl<'i, T: Clone> ContainerRule<'i, T> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.rules.minify(context, parent_is_unused)?;
    Ok(self.rules.0.is_empty())
  }
}

impl<'a, 'i, T: ToCss> ToCss for ContainerRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@container ")?;
    let has_condition = self.condition.is_some();

    if let Some(name) = &self.name {
      name.to_css(dest)?;
      if has_condition {
        dest.write_char(' ')?;
      }
    }

    if let Some(condition) = &self.condition {
      // Don't downlevel range syntax in container queries.
      let exclude = dest.targets.current.exclude;
      dest.targets.current.exclude.insert(Features::MediaQueries);
      condition.to_css(dest)?;
      dest.targets.current.exclude = exclude;
    }

    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    dest.newline()?;
    self.rules.to_css(dest)?;
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}
