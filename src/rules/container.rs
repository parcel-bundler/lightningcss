//! The `@container` rule.

use cssparser::*;

use super::Location;
use super::{CssRuleList, MinifyContext};
use crate::error::{MinifyError, ParserError, PrinterError};
use crate::media_query::MediaCondition;
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::rules::{StyleContext, ToCssWithContext};
use crate::traits::{Parse, ToCss};
use crate::values::ident::CustomIdent;
use crate::visitor::Visit;

/// A [@container](https://drafts.csswg.org/css-contain-3/#container-rule) rule.
#[derive(Debug, PartialEq, Clone, Visit)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ContainerRule<'i, R = DefaultAtRule> {
  /// The name of the container.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Option<ContainerName<'i>>,
  /// The container condition.
  pub condition: MediaCondition<'i>,
  /// The rules within the `@container` rule.
  pub rules: CssRuleList<'i, R>,
  /// The location of the rule in the source file.
  #[skip_visit]
  pub loc: Location,
}

/// A [`<container-name>`](https://drafts.csswg.org/css-contain-3/#typedef-container-name) in a `@container` rule.
#[derive(Debug, Clone, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    self.0.to_css(dest)
  }
}

impl<'i, T> ContainerRule<'i, T> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.rules.minify(context, parent_is_unused)?;
    Ok(self.rules.0.is_empty())
  }
}

impl<'a, 'i, T: ToCss> ToCssWithContext<'a, 'i, T> for ContainerRule<'i, T> {
  fn to_css_with_context<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i, T>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    dest.write_str("@container ")?;
    if let Some(name) = &self.name {
      name.to_css(dest)?;
      dest.write_char(' ')?;
    }

    // Don't downlevel range syntax in container queries.
    let mut targets = None;
    std::mem::swap(&mut targets, &mut dest.targets);
    self.condition.to_css(dest)?;
    std::mem::swap(&mut targets, &mut dest.targets);

    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    dest.newline()?;
    self.rules.to_css_with_context(dest, context)?;
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}
