//! The `@position-try` rule.

#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use cssparser::*;

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
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

bitflags! {
  /// A [`<try-tactic>`](https://drafts.csswg.org/css-anchor-position-1/#typedef-position-try-fallbacks-try-tactic) value,
  /// used in the `position-try-fallbacks` property and the `anchored(fallback: ...)` container query feature.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "Vec<TryTacticFlag>", into = "Vec<TryTacticFlag>"))]
  #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct TryTactic: u8 {
    /// Flips the position across the block axis.
    const FlipBlock  = 0b00001;
    /// Flips the position across the inline axis.
    const FlipInline = 0b00010;
    /// Swaps the block and inline axes.
    const FlipStart  = 0b00100;
    /// Flips the position across the horizontal axis.
    const FlipX      = 0b01000;
    /// Flips the position across the vertical axis.
    const FlipY      = 0b10000;
  }
}

impl TryTactic {
  fn from_ident(ident: &str) -> Option<TryTactic> {
    Some(match_ignore_ascii_case! { ident,
      "flip-block" => TryTactic::FlipBlock,
      "flip-inline" => TryTactic::FlipInline,
      "flip-start" => TryTactic::FlipStart,
      "flip-x" => TryTactic::FlipX,
      "flip-y" => TryTactic::FlipY,
      _ => return None,
    })
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
    val!(FlipX, "flip-x");
    val!(FlipY, "flip-y");
    Ok(())
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "kebab-case"))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum TryTacticFlag {
  FlipBlock,
  FlipInline,
  FlipStart,
  FlipX,
  FlipY,
}

impl From<TryTactic> for Vec<TryTacticFlag> {
  fn from(t: TryTactic) -> Self {
    let mut v = Vec::new();
    macro_rules! flag {
      ($t:ident) => {
        if t.contains(TryTactic::$t) {
          v.push(TryTacticFlag::$t);
        }
      };
    }
    flag!(FlipBlock);
    flag!(FlipInline);
    flag!(FlipStart);
    flag!(FlipX);
    flag!(FlipY);
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
        TryTacticFlag::FlipX => TryTactic::FlipX,
        TryTacticFlag::FlipY => TryTactic::FlipY,
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
  /// A `<try-tactic>` keyword combination. Empty if only a name is given.
  pub tactic: TryTactic,
}

impl<'i> Parse<'i> for PositionTryFallback<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut name = None;
    let mut tactic = TryTactic::empty();

    loop {
      if name.is_none() {
        if let Ok(ident) = input.try_parse(DashedIdent::parse) {
          name = Some(ident);
          continue;
        }
      }

      let flag = input.try_parse(|input| -> Result<_, ParseError<'i, ParserError<'i>>> {
        let location = input.current_source_location();
        let ident = input.expect_ident_cloned()?;
        match TryTactic::from_ident(&ident) {
          Some(flag) if !tactic.contains(flag) => Ok(flag),
          _ => Err(location.new_unexpected_token_error(Token::Ident(ident.clone()))),
        }
      });

      match flag {
        Ok(flag) => tactic |= flag,
        Err(..) => break,
      }
    }

    if name.is_none() && tactic.is_empty() {
      return Err(input.new_error_for_next_token());
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
      if !self.tactic.is_empty() {
        dest.write_char(' ')?;
      }
    }
    self.tactic.to_css(dest)
  }
}
