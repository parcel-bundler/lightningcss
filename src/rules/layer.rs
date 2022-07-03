//! The `@layer` rule.

use super::{CssRuleList, Location, MinifyContext};
use crate::error::{MinifyError, ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
use cssparser::*;
use smallvec::SmallVec;

/// A [`<layer-name>`](https://drafts.csswg.org/css-cascade-5/#typedef-layer-name) within
/// a `@layer` or `@import` rule.
///
/// Nested layers are represented using a list of identifiers. In CSS syntax, these are dot-separated.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

      dest.write_str(name)?;
    }

    Ok(())
  }
}

/// A [@layer statement](https://drafts.csswg.org/css-cascade-5/#layer-empty) rule.
///
/// See also [LayerBlockRule](LayerBlockRule).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LayerStatementRule<'i> {
  /// The layer names to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub names: Vec<LayerName<'i>>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> ToCss for LayerStatementRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    dest.write_str("@layer ")?;
    self.names.to_css(dest)?;
    dest.write_char(';')
  }
}

/// A [@layer block](https://drafts.csswg.org/css-cascade-5/#layer-block) rule.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LayerBlockRule<'i> {
  /// The name of the layer to declare, or `None` to declare an anonymous layer.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Option<LayerName<'i>>,
  /// The rules within the `@layer` rule.
  pub rules: CssRuleList<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> LayerBlockRule<'i> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.rules.minify(context, parent_is_unused)?;

    Ok(self.rules.0.is_empty())
  }
}

impl<'i> ToCss for LayerBlockRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
