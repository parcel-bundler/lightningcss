//! The `@font-feature-values` rule.

use cssparser::*;

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::rules::CssRuleList;
use crate::traits::{Parse, ToCss};
use crate::values::ident::CustomIdent;
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@font-feature-values](https://drafts.csswg.org/css-fonts/#font-feature-values) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct FontFeatureValuesRule<'i, R = DefaultAtRule> {
  /// The name of the font feature values.
  pub name: Vec<FontFeatureValuesRuleName<'i>>,
  /// The declarations within the `@font-feature-values` rule.
  #[cfg_attr(feature = "serde", serde(default))]
  pub declarations: DeclarationBlock<'i>,
  /// The rules within the `@font-feature-values` rule.
  #[cfg_attr(feature = "serde", serde(default = "default_rule_list::<R>"))]
  pub rules: CssRuleList<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

#[cfg(feature = "serde")]
fn default_rule_list<'i, R>() -> CssRuleList<'i, R> {
  CssRuleList(Vec::new())
}

/// FontFeatureValuesRuleName
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum FontFeatureValuesRuleName<'i> {
  /// `<custom-ident>` of a `@font-feature-values` name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Ident(CustomIdent<'i>),

  /// `<string>` of a `@font-feature-values` name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Custom(CowArcStr<'i>),
}

impl<'i> Parse<'i> for FontFeatureValuesRuleName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    match input.next()?.clone() {
      Token::Ident(ref s) => {
        // CSS-wide keywords without quotes throws an error.
        match_ignore_ascii_case! { &*s,
          "none" | "initial" | "inherit" | "unset" | "default" | "revert" | "revert-layer" => {
            Err(input.new_unexpected_token_error(Token::Ident(s.clone())))
          },
          _ => {
            Ok(FontFeatureValuesRuleName::Ident(CustomIdent(s.into())))
          }
        }
      }

      Token::QuotedString(ref s) => Ok(FontFeatureValuesRuleName::Custom(s.into())),
      t => return Err(input.new_unexpected_token_error(t.clone())),
    }
  }
}

impl<'i> ToCss for FontFeatureValuesRuleName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let css_module_animation_enabled =
      dest.css_module.as_ref().map_or(false, |css_module| css_module.config.animation);

    match self {
      FontFeatureValuesRuleName::Ident(ident) => {
        dest.write_ident(ident.0.as_ref(), css_module_animation_enabled)?;
      }
      FontFeatureValuesRuleName::Custom(custom) => {
        // CSS-wide keywords and `none` cannot remove quotes.
        match_ignore_ascii_case! { &*custom,
          "none" | "initial" | "inherit" | "unset" | "default" | "revert" | "revert-layer" => {
            serialize_string(&custom, dest)?;
          },
          _ => {
            dest.write_ident(custom.as_ref(), css_module_animation_enabled)?;
          }
        }
      }
    }
    Ok(())
  }
}

impl<'i, T: ToCss> ToCss for FontFeatureValuesRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@font-feature-values ")?;
    self.name.to_css(dest)?;
    dest.write_char('{')?;
    if !self.declarations.is_empty() || !self.rules.0.is_empty() {
      dest.newline()?;
      self.declarations.to_css(dest)?;
      self.rules.to_css(dest)?;
    }
    dest.write_char('}')
  }
}
