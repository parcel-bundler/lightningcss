//! The `@position-try` rule.

use cssparser::*;
use smallvec::SmallVec;

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::ident::DashedIdent;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@position-try](https://drafts.csswg.org/css-anchor-position-1/#position-try) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PositionTryRule<'i> {
  /// The name of the position-try fallback.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// Declarations in the `@position-try` rule.
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for PositionTryRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@position-try ")?;
    self.name.to_css(dest)?;
    self.declarations.to_css_block(dest)
  }
}

enum_property! {
  /// A single keyword within a [`<try-tactic>`](https://drafts.csswg.org/css-anchor-position-1/#typedef-position-try-fallbacks-try-tactic) value.
  pub enum TryTacticKeyword {
    /// Flips the position across the block axis.
    FlipBlock,
    /// Flips the position across the inline axis.
    FlipInline,
    /// Swaps the block and inline axes.
    FlipStart,
    /// Flips the position across the horizontal axis.
    FlipX,
    /// Flips the position across the vertical axis.
    FlipY,
  }
}

/// A [`<try-tactic>`](https://drafts.csswg.org/css-anchor-position-1/#typedef-position-try-fallbacks-try-tactic) value,
/// used in the `position-try-fallbacks` property and the `anchored(fallback: ...)` container query feature.
///
/// The transformations are composed in order, so the order of the keywords is significant:
/// `flip-start flip-block` is not the same as `flip-block flip-start`. Keywords are therefore
/// stored and serialized in source order. Each keyword may appear at most once.
#[derive(Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct TryTactic(pub SmallVec<[TryTacticKeyword; 5]>);

impl TryTactic {
  /// Parses zero or more consecutive try tactic keywords, rejecting duplicates.
  fn parse_keywords<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut keywords = SmallVec::new();
    loop {
      let state = input.state();
      let Ok(keyword) = input.try_parse(TryTacticKeyword::parse) else {
        break;
      };

      if keywords.contains(&keyword) {
        input.reset(&state);
        return Err(input.new_error_for_next_token());
      }

      keywords.push(keyword);
    }

    Ok(TryTactic(keywords))
  }
}

impl<'i> Parse<'i> for TryTactic {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let tactic = Self::parse_keywords(input)?;
    if tactic.0.is_empty() {
      return Err(input.new_error_for_next_token());
    }

    Ok(tactic)
  }
}

impl ToCss for TryTactic {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    for keyword in &self.0 {
      if first {
        first = false;
      } else {
        dest.write_char(' ')?;
      }
      keyword.to_css(dest)?;
    }
    Ok(())
  }
}

/// A `[<dashed-ident> || <try-tactic>]` fallback, as used in the
/// [position-try-fallbacks](https://drafts.csswg.org/css-anchor-position-1/#position-try-fallbacks) property
/// and the [`anchored(fallback: ...)`](https://drafts.csswg.org/css-anchor-position-2/#fallback-feature)
/// container query feature.
///
/// At least one of `name` and `tactic` is present.
/// Note: the `<position-area>` form is not yet supported.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PositionTryFallback<'i> {
  /// A `<dashed-ident>` referencing a `@position-try` rule name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Option<DashedIdent<'i>>,
  /// The `<try-tactic>` keywords, in source order. Empty if only a name is given.
  pub tactic: TryTactic,
}

impl<'i> Parse<'i> for PositionTryFallback<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // The name may come before or after the try tactic, but not in between its keywords.
    let mut name = input.try_parse(DashedIdent::parse).ok();
    let tactic = TryTactic::parse_keywords(input)?;

    if name.is_none() {
      if tactic.0.is_empty() {
        return Err(input.new_error_for_next_token());
      }

      name = input.try_parse(DashedIdent::parse).ok();
    }

    Ok(PositionTryFallback { name, tactic })
  }
}

impl<'i> ToCss for PositionTryFallback<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if let Some(name) = &self.name {
      name.to_css(dest)?;
      if !self.tactic.0.is_empty() {
        dest.write_char(' ')?;
      }
    }
    self.tactic.to_css(dest)
  }
}
