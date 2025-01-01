//! The `@font-feature-values` rule.

use super::Location;
use crate::error::{ParserError, PrinterError};
use crate::parser::ParserOptions;
use crate::printer::Printer;
use crate::properties::font::FamilyName;
use crate::traits::{Parse, ToCss};
use crate::values::ident::Ident;
use crate::values::number::CSSInteger;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use indexmap::IndexMap;
use smallvec::SmallVec;
use std::fmt::Write;

/// A [@font-feature-values](https://drafts.csswg.org/css-fonts/#font-feature-values) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct FontFeatureValuesRule<'i> {
  /// The name of the font feature values.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Vec<FamilyName<'i>>,
  /// The rules within the `@font-feature-values` rule.
  pub rules: IndexMap<FontFeatureSubruleType, FontFeatureSubrule<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> FontFeatureValuesRule<'i> {
  pub(crate) fn parse<'t, 'o>(
    family_names: Vec<FamilyName<'i>>,
    input: &mut Parser<'i, 't>,
    loc: Location,
    options: &ParserOptions<'o, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut rules = IndexMap::new();
    let mut rule_parser = FontFeatureValuesRuleParser {
      rules: &mut rules,
      options,
    };
    let mut parser = RuleBodyParser::new(input, &mut rule_parser);

    while let Some(decl_or_rule) = parser.next() {
      if let Err((err, _)) = decl_or_rule {
        if parser.parser.options.error_recovery {
          parser.parser.options.warn(err);
          continue;
        }
        return Err(err);
      }
    }

    Ok(FontFeatureValuesRule {
      name: family_names,
      rules,
      loc,
    })
  }
}

struct FontFeatureValuesRuleParser<'a, 'o, 'i> {
  rules: &'a mut IndexMap<FontFeatureSubruleType, FontFeatureSubrule<'i>>,
  options: &'a ParserOptions<'o, 'i>,
}

impl<'a, 'o, 'i> cssparser::DeclarationParser<'i> for FontFeatureValuesRuleParser<'a, 'o, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;
}

impl<'a, 'o, 'i> cssparser::AtRuleParser<'i> for FontFeatureValuesRuleParser<'a, 'o, 'i> {
  type Prelude = FontFeatureSubruleType;
  type AtRule = ();
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let loc = input.current_source_location();
    FontFeatureSubruleType::parse_string(&name)
      .map_err(|_| loc.new_custom_error(ParserError::AtRuleInvalid(name.clone().into())))
  }

  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    let mut decls = IndexMap::new();
    let mut has_existing = false;
    let declarations = if let Some(rule) = self.rules.get_mut(&prelude) {
      has_existing = true;
      &mut rule.declarations
    } else {
      &mut decls
    };
    let mut decl_parser = FontFeatureDeclarationParser { declarations };
    let mut parser = RuleBodyParser::new(input, &mut decl_parser);
    while let Some(decl) = parser.next() {
      if let Err((err, _)) = decl {
        if self.options.error_recovery {
          self.options.warn(err);
          continue;
        }
        return Err(err);
      }
    }

    if !has_existing {
      self.rules.insert(
        prelude,
        FontFeatureSubrule {
          name: prelude,
          declarations: decls,
          loc: Location {
            source_index: self.options.source_index,
            line: loc.line,
            column: loc.column,
          },
        },
      );
    }

    Ok(())
  }
}

impl<'a, 'o, 'i> QualifiedRuleParser<'i> for FontFeatureValuesRuleParser<'a, 'o, 'i> {
  type Prelude = ();
  type QualifiedRule = ();
  type Error = ParserError<'i>;
}

impl<'a, 'o, 'i> RuleBodyItemParser<'i, (), ParserError<'i>> for FontFeatureValuesRuleParser<'a, 'o, 'i> {
  fn parse_declarations(&self) -> bool {
    false
  }

  fn parse_qualified(&self) -> bool {
    false
  }
}

impl<'i> ToCss for FontFeatureValuesRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@font-feature-values ")?;
    self.name.to_css(dest)?;
    dest.whitespace()?;
    dest.write_char('{')?;
    if !self.rules.is_empty() {
      dest.newline()?;
      for rule in self.rules.values() {
        rule.to_css(dest)?;
        dest.newline()?;
      }
    }
    dest.write_char('}')
  }
}

impl<'i> FontFeatureValuesRule<'i> {
  pub(crate) fn merge(&mut self, other: &FontFeatureValuesRule<'i>) {
    debug_assert_eq!(self.name, other.name);
    for (prelude, rule) in &other.rules {
      if let Some(existing) = self.rules.get_mut(prelude) {
        existing
          .declarations
          .extend(rule.declarations.iter().map(|(k, v)| (k.clone(), v.clone())));
      } else {
        self.rules.insert(*prelude, rule.clone());
      }
    }
  }
}

/// The name of the `@font-feature-values` sub-rule.
/// font-feature-value-type = <@stylistic> | <@historical-forms> | <@styleset> | <@character-variant>
///   | <@swash> | <@ornaments> | <@annotation>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum FontFeatureSubruleType {
  /// @stylistic = @stylistic { <declaration-list> }
  Stylistic,
  /// @historical-forms = @historical-forms { <declaration-list> }
  HistoricalForms,
  /// @styleset = @styleset { <declaration-list> }
  Styleset,
  /// @character-variant = @character-variant { <declaration-list> }
  CharacterVariant,
  /// @swash = @swash { <declaration-list> }
  Swash,
  /// @ornaments = @ornaments { <declaration-list> }
  Ornaments,
  /// @annotation = @annotation { <declaration-list> }
  Annotation,
}

/// A sub-rule of `@font-feature-values`
/// https://drafts.csswg.org/css-fonts/#font-feature-values-syntax
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct FontFeatureSubrule<'i> {
  /// The name of the `@font-feature-values` sub-rule.
  pub name: FontFeatureSubruleType,
  /// The declarations within the `@font-feature-values` sub-rules.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub declarations: IndexMap<Ident<'i>, SmallVec<[CSSInteger; 1]>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for FontFeatureSubrule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_char('@')?;
    self.name.to_css(dest)?;
    dest.write_char('{')?;
    dest.indent();
    let len = self.declarations.len();
    for (i, (name, value)) in self.declarations.iter().enumerate() {
      dest.newline()?;
      name.to_css(dest)?;
      dest.delim(':', false)?;

      let mut first = true;
      for index in value {
        if first {
          first = false;
        } else {
          dest.write_char(' ')?;
        }
        index.to_css(dest)?;
      }

      if i != len - 1 || !dest.minify {
        dest.write_char(';')?;
      }
    }
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

struct FontFeatureDeclarationParser<'a, 'i> {
  declarations: &'a mut IndexMap<Ident<'i>, SmallVec<[CSSInteger; 1]>>,
}

impl<'a, 'i> cssparser::DeclarationParser<'i> for FontFeatureDeclarationParser<'a, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    let mut indices = SmallVec::new();
    loop {
      if let Ok(value) = CSSInteger::parse(input) {
        indices.push(value);
      } else {
        break;
      }
    }

    if indices.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidValue));
    }

    self.declarations.insert(Ident(name.into()), indices);
    Ok(())
  }
}

/// Default methods reject all at rules.
impl<'a, 'i> AtRuleParser<'i> for FontFeatureDeclarationParser<'a, 'i> {
  type Prelude = ();
  type AtRule = ();
  type Error = ParserError<'i>;
}

impl<'a, 'i> QualifiedRuleParser<'i> for FontFeatureDeclarationParser<'a, 'i> {
  type Prelude = ();
  type QualifiedRule = ();
  type Error = ParserError<'i>;
}

impl<'a, 'i> RuleBodyItemParser<'i, (), ParserError<'i>> for FontFeatureDeclarationParser<'a, 'i> {
  fn parse_qualified(&self) -> bool {
    false
  }

  fn parse_declarations(&self) -> bool {
    true
  }
}
