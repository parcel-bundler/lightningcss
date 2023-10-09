//! The `@layer` rule.

use super::{CssRuleList, Location, MinifyContext};
use crate::error::{MinifyError, ParserError, PrinterError};
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;

/// A [`<layer-name>`](https://drafts.csswg.org/css-cascade-5/#typedef-layer-name) within
/// a `@layer` or `@import` rule.
///
/// Nested layers are represented using a list of identifiers. In CSS syntax, these are dot-separated.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct LayerName<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub SmallVec<[CowArcStr<'i>; 1]>);

macro_rules! expect_non_whitespace {
  ($parser: ident, $($branches: tt)+) => {{
    let start_location = $parser.current_source_location();
    match *$parser.next_including_whitespace()? {
      $($branches)+
      ref token => {
        return Err(start_location.new_basic_unexpected_token_error(token.clone()))
      }
    }
  }}
}

impl<'i> Parse<'i> for LayerName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut parts = SmallVec::new();
    let ident = input.expect_ident()?;
    parts.push(ident.into());

    loop {
      let name = input.try_parse(|input| {
        expect_non_whitespace! {input,
          Token::Delim('.') => Ok(()),
        }?;

        expect_non_whitespace! {input,
          Token::Ident(ref id) => Ok(id.into()),
        }
      });

      match name {
        Ok(name) => parts.push(name),
        Err(_) => break,
      }
    }

    Ok(LayerName(parts))
  }
}

impl<'i> ToCss for LayerName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    for name in &self.0 {
      if first {
        first = false;
      } else {
        dest.write_char('.')?;
      }

      serialize_identifier(name, dest)?;
    }

    Ok(())
  }
}

/// A [@layer statement](https://drafts.csswg.org/css-cascade-5/#layer-empty) rule.
///
/// See also [LayerBlockRule](LayerBlockRule).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct LayerStatementRule<'i> {
  /// The layer names to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub names: Vec<LayerName<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for LayerStatementRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@layer ")?;
    self.names.to_css(dest)?;
    dest.write_char(';')
  }
}

/// A [@layer block](https://drafts.csswg.org/css-cascade-5/#layer-block) rule.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct LayerBlockRule<'i, R = DefaultAtRule> {
  /// The name of the layer to declare, or `None` to declare an anonymous layer.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub name: Option<LayerName<'i>>,
  /// The rules within the `@layer` rule.
  pub rules: CssRuleList<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i, T: Clone> LayerBlockRule<'i, T> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.rules.minify(context, parent_is_unused)?;

    Ok(self.rules.0.is_empty())
  }
}

impl<'a, 'i, T: ToCss> ToCss for LayerBlockRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@layer")?;
    if let Some(name) = &self.name {
      dest.write_char(' ')?;
      name.to_css(dest)?;
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
