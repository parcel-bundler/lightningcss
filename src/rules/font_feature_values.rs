//! The `@font-feature-values` rule.

use super::Location;
use crate::declaration::{parse_declaration, DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::parser::ParserOptions;
use crate::printer::Printer;
use crate::properties::font::FontFamily;
use crate::traits::{Parse, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use std::fmt::Write;

/// A [@font-feature-values](https://drafts.csswg.org/css-fonts/#font-feature-values) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct FontFeatureValuesRule<'i> {
  /// The name of the font feature values.
  pub name: Vec<FontFamily<'i>>,
  /// The declarations within the `@font-feature-values` rule.
  #[cfg_attr(feature = "serde", serde())]
  pub declarations: DeclarationBlock<'i>,
  /// The rules within the `@font-feature-values` rule.
  pub rules: Vec<FontFeatureSubrule<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> FontFeatureValuesRule<'i> {
  pub(crate) fn parse<'t, 'o>(
    family_names: Vec<FontFamily<'i>>,
    input: &mut Parser<'i, 't>,
    loc: Location,
    options: &ParserOptions<'o, 'i>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut declarations = DeclarationBlock::new();
    let mut rules = Vec::new();
    let mut rule_parser = FontFeatureValuesRuleParser {
      declarations: &mut declarations,
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

    declarations
      .declarations
      .retain(|decl| decl.property_id().name() == "font-display");
    declarations
      .important_declarations
      .retain(|decl| decl.property_id().name() == "font-display");

    Ok(FontFeatureValuesRule {
      name: family_names,
      declarations,
      rules,
      loc,
    })
  }
}

pub(crate) struct FontFeatureValuesRuleParser<'a, 'o, 'i> {
  declarations: &'a mut DeclarationBlock<'i>,
  rules: &'a mut Vec<FontFeatureSubrule<'i>>,
  options: &'a ParserOptions<'o, 'i>,
}

impl<'a, 'o, 'i> cssparser::DeclarationParser<'i> for FontFeatureValuesRuleParser<'a, 'o, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, '_>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    let mut declarations = DeclarationList::new();
    let mut important_decls = DeclarationList::new();
    parse_declaration(name, input, &mut declarations, &mut important_decls, self.options)?;

    declarations
      .iter()
      .filter(|it| it.property_id().name() == "font-display")
      .for_each(|decl| {
        self.declarations.declarations.push(decl.clone());
      });

    important_decls
      .iter()
      .filter(|it| it.property_id().name() == "font-display")
      .for_each(|decl| {
        self.declarations.important_declarations.push(decl.clone());
      });

    Ok(())
  }
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
    let declarations = DeclarationBlock::parse(input, self.options)?;
    self.rules.push(FontFeatureSubrule {
      name: prelude,
      declarations,
      loc: Location {
        source_index: self.options.source_index,
        line: loc.line,
        column: loc.column,
      },
    });

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
    true
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
    let mut i = 0;
    let len = self.declarations.len() + self.rules.len();

    macro_rules! write {
      ($decls: expr, $important: literal) => {
        for decl in &$decls {
          dest.newline()?;
          decl.to_css(dest, $important)?;
          if i != len - 1 || !dest.minify {
            dest.write_char(';')?;
          }
          i += 1;
        }
      };
    }

    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@font-feature-values ")?;
    self.name.to_css(dest)?;
    dest.write_char('{')?;
    if !self.declarations.is_empty() || !self.rules.is_empty() {
      dest.newline()?;
      write!(self.declarations.declarations, false);
      write!(self.declarations.important_declarations, true);
      for rule in &self.rules {
        rule.to_css(dest)?;
        dest.newline()?;
      }
    }
    dest.write_char('}')
  }
}

enum_property! {
    pub enum FontFeatureSubruleType {
        Stylistic ,
        HistoricalForms,
        Styleset,
        CharacterVariant,
        Swash,
        Ornaments,
        Annotation,
    }
}

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
  pub name: FontFeatureSubruleType,
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub declarations: DeclarationBlock<'i>,
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
    self.declarations.to_css_block(dest)
  }
}
