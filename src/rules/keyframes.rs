//! The `@keyframes` rule.

use super::supports::SupportsRule;
use super::MinifyContext;
use super::{CssRule, CssRuleList, Location};
use crate::context::DeclarationContext;
use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::parser::ParserOptions;
use crate::printer::Printer;
use crate::properties::animation::TimelineRangeName;
use crate::properties::custom::{CustomProperty, UnparsedProperty};
use crate::properties::Property;
use crate::targets::Targets;
use crate::traits::{Parse, ToCss};
use crate::values::color::ColorFallbackKind;
use crate::values::ident::CustomIdent;
use crate::values::percentage::Percentage;
use crate::values::string::CowArcStr;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A [@keyframes](https://drafts.csswg.org/css-animations/#keyframes) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct KeyframesRule<'i> {
  /// The animation name.
  /// <keyframes-name> = <custom-ident> | <string>
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: KeyframesName<'i>,
  /// A list of keyframes in the animation.
  pub keyframes: Vec<Keyframe<'i>>,
  /// A vendor prefix for the rule, e.g. `@-webkit-keyframes`.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub vendor_prefix: VendorPrefix,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

/// KeyframesName
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum KeyframesName<'i> {
  /// `<custom-ident>` of a `@keyframes` name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Ident(CustomIdent<'i>),

  /// `<string>` of a `@keyframes` name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Custom(CowArcStr<'i>),
}

impl<'i> Parse<'i> for KeyframesName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.next()?.clone() {
      Token::Ident(ref s) => {
        // CSS-wide keywords without quotes throws an error.
        match_ignore_ascii_case! { &*s,
          "none" | "initial" | "inherit" | "unset" | "default" | "revert" | "revert-layer" => {
            Err(input.new_unexpected_token_error(Token::Ident(s.clone())))
          },
          _ => {
            Ok(KeyframesName::Ident(CustomIdent(s.into())))
          }
        }
      }

      Token::QuotedString(ref s) => Ok(KeyframesName::Custom(s.into())),
      t => return Err(input.new_unexpected_token_error(t.clone())),
    }
  }
}

impl<'i> ToCss for KeyframesName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let css_module_animation_enabled =
      dest.css_module.as_ref().map_or(false, |css_module| css_module.config.animation);

    match self {
      KeyframesName::Ident(ident) => {
        dest.write_ident(ident.0.as_ref(), css_module_animation_enabled)?;
      }
      KeyframesName::Custom(s) => {
        // CSS-wide keywords and `none` cannot remove quotes.
        match_ignore_ascii_case! { &*s,
          "none" | "initial" | "inherit" | "unset" | "default" | "revert" | "revert-layer" => {
            serialize_string(&s, dest)?;
          },
          _ => {
            dest.write_ident(s.as_ref(), css_module_animation_enabled)?;
          }
        }
      }
    }
    Ok(())
  }
}

impl<'i> KeyframesRule<'i> {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext<'_, 'i>) {
    context.handler_context.context = DeclarationContext::Keyframes;

    for keyframe in &mut self.keyframes {
      keyframe
        .declarations
        .minify(context.handler, context.important_handler, &mut context.handler_context)
    }

    context.handler_context.context = DeclarationContext::None;
  }

  pub(crate) fn get_fallbacks<T>(&mut self, targets: &Targets) -> Vec<CssRule<'i, T>> {
    let mut fallbacks = ColorFallbackKind::empty();
    for keyframe in &self.keyframes {
      for property in &keyframe.declarations.declarations {
        match property {
          Property::Custom(CustomProperty { value, .. }) | Property::Unparsed(UnparsedProperty { value, .. }) => {
            fallbacks |= value.get_necessary_fallbacks(*targets);
          }
          _ => {}
        }
      }
    }

    let mut res = Vec::new();
    let lowest_fallback = fallbacks.lowest();
    fallbacks.remove(lowest_fallback);

    if fallbacks.contains(ColorFallbackKind::P3) {
      res.push(self.get_fallback(ColorFallbackKind::P3));
    }

    if fallbacks.contains(ColorFallbackKind::LAB)
      || (!lowest_fallback.is_empty() && lowest_fallback != ColorFallbackKind::LAB)
    {
      res.push(self.get_fallback(ColorFallbackKind::LAB));
    }

    if !lowest_fallback.is_empty() {
      for keyframe in &mut self.keyframes {
        for property in &mut keyframe.declarations.declarations {
          match property {
            Property::Custom(CustomProperty { value, .. })
            | Property::Unparsed(UnparsedProperty { value, .. }) => {
              *value = value.get_fallback(lowest_fallback);
            }
            _ => {}
          }
        }
      }
    }

    res
  }

  fn get_fallback<T>(&self, kind: ColorFallbackKind) -> CssRule<'i, T> {
    let keyframes = self
      .keyframes
      .iter()
      .map(|keyframe| Keyframe {
        selectors: keyframe.selectors.clone(),
        declarations: DeclarationBlock {
          important_declarations: vec![],
          declarations: keyframe
            .declarations
            .declarations
            .iter()
            .map(|property| match property {
              Property::Custom(custom) => Property::Custom(CustomProperty {
                name: custom.name.clone(),
                value: custom.value.get_fallback(kind),
              }),
              Property::Unparsed(unparsed) => Property::Unparsed(UnparsedProperty {
                property_id: unparsed.property_id.clone(),
                value: unparsed.value.get_fallback(kind),
              }),
              _ => property.clone(),
            })
            .collect(),
        },
      })
      .collect();

    CssRule::Supports(SupportsRule {
      condition: kind.supports_condition(),
      rules: CssRuleList(vec![CssRule::Keyframes(KeyframesRule {
        name: self.name.clone(),
        keyframes,
        vendor_prefix: self.vendor_prefix,
        loc: self.loc.clone(),
      })]),
      loc: self.loc.clone(),
    })
  }
}

impl<'i> ToCss for KeyframesRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    let mut first_rule = true;
    macro_rules! write_prefix {
      ($prefix: ident) => {
        if self.vendor_prefix.contains(VendorPrefix::$prefix) {
          #[allow(unused_assignments)]
          if first_rule {
            first_rule = false;
          } else {
            if !dest.minify {
              dest.write_char('\n')?; // no indent
            }
            dest.newline()?;
          }
          dest.write_char('@')?;
          VendorPrefix::$prefix.to_css(dest)?;
          dest.write_str("keyframes ")?;
          self.name.to_css(dest)?;
          dest.whitespace()?;
          dest.write_char('{')?;
          dest.indent();
          let mut first = true;
          for keyframe in &self.keyframes {
            if first {
              first = false;
            } else if !dest.minify {
              dest.write_char('\n')?; // no indent
            }
            dest.newline()?;
            keyframe.to_css(dest)?;
          }
          dest.dedent();
          dest.newline()?;
          dest.write_char('}')?;
        }
      };
    }

    write_prefix!(WebKit);
    write_prefix!(Moz);
    write_prefix!(O);
    write_prefix!(None);
    Ok(())
  }
}

/// A percentage of a given timeline range
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct TimelineRangePercentage {
  /// The name of the timeline range.
  name: TimelineRangeName,
  /// The percentage progress between the start and end of the range.
  percentage: Percentage,
}

impl<'i> Parse<'i> for TimelineRangePercentage {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = TimelineRangeName::parse(input)?;
    let percentage = Percentage::parse(input)?;
    Ok(TimelineRangePercentage { name, percentage })
  }
}

/// A [keyframe selector](https://drafts.csswg.org/css-animations/#typedef-keyframe-selector)
/// within an `@keyframes` rule.
#[derive(Debug, PartialEq, Clone, Parse)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum KeyframeSelector {
  /// An explicit percentage.
  Percentage(Percentage),
  /// The `from` keyword. Equivalent to 0%.
  From,
  /// The `to` keyword. Equivalent to 100%.
  To,
  /// A [named timeline range selector](https://drafts.csswg.org/scroll-animations-1/#named-range-keyframes)
  TimelineRangePercentage(TimelineRangePercentage),
}

impl ToCss for KeyframeSelector {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      KeyframeSelector::Percentage(p) => {
        if dest.minify && *p == Percentage(1.0) {
          dest.write_str("to")
        } else {
          p.to_css(dest)
        }
      }
      KeyframeSelector::From => {
        if dest.minify {
          dest.write_str("0%")
        } else {
          dest.write_str("from")
        }
      }
      KeyframeSelector::To => dest.write_str("to"),
      KeyframeSelector::TimelineRangePercentage(TimelineRangePercentage {
        name: timeline_range_name,
        percentage,
      }) => {
        timeline_range_name.to_css(dest)?;
        dest.write_char(' ')?;
        percentage.to_css(dest)
      }
    }
  }
}

/// An individual keyframe within an `@keyframes` rule.
///
/// See [KeyframesRule](KeyframesRule).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Keyframe<'i> {
  /// A list of keyframe selectors to associate with the declarations in this keyframe.
  pub selectors: Vec<KeyframeSelector>,
  /// The declarations for this keyframe.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub declarations: DeclarationBlock<'i>,
}

impl<'i> ToCss for Keyframe<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    for selector in &self.selectors {
      if !first {
        dest.delim(',', false)?;
      }
      first = false;
      selector.to_css(dest)?;
    }

    self.declarations.to_css_block(dest)
  }
}

pub(crate) struct KeyframeListParser;

impl<'a, 'i> AtRuleParser<'i> for KeyframeListParser {
  type Prelude = ();
  type AtRule = Keyframe<'i>;
  type Error = ParserError<'i>;
}

impl<'a, 'i> QualifiedRuleParser<'i> for KeyframeListParser {
  type Prelude = Vec<KeyframeSelector>;
  type QualifiedRule = Keyframe<'i>;
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, ParserError<'i>>> {
    input.parse_comma_separated(KeyframeSelector::parse)
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    _: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::QualifiedRule, ParseError<'i, ParserError<'i>>> {
    // For now there are no options that apply within @keyframes
    let options = ParserOptions::default();
    Ok(Keyframe {
      selectors,
      declarations: DeclarationBlock::parse(input, &options)?,
    })
  }
}

impl<'i> DeclarationParser<'i> for KeyframeListParser {
  type Declaration = Keyframe<'i>;
  type Error = ParserError<'i>;
}

impl<'i> RuleBodyItemParser<'i, Keyframe<'i>, ParserError<'i>> for KeyframeListParser {
  fn parse_qualified(&self) -> bool {
    true
  }

  fn parse_declarations(&self) -> bool {
    false
  }
}
