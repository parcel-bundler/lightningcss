use std::collections::HashMap;

use cssparser::*;
use lightningcss::{
  declaration::DeclarationBlock,
  error::ParserError,
  rules::CssRuleList,
  stylesheet::ParserOptions,
  traits::{AtRuleParser, ToCss},
  values::{
    string::CowArcStr,
    syntax::{ParsedComponent, SyntaxString},
  },
  visitor::{Visit, VisitTypes, Visitor},
};
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Deserialize, Debug, Clone)]
pub struct CustomAtRuleConfig {
  #[serde(default, deserialize_with = "deserialize_prelude")]
  prelude: Option<SyntaxString>,
  body: Option<CustomAtRuleBodyType>,
}

fn deserialize_prelude<'de, D>(deserializer: D) -> Result<Option<SyntaxString>, D::Error>
where
  D: Deserializer<'de>,
{
  let s = Option::<CowArcStr<'de>>::deserialize(deserializer)?;
  if let Some(s) = s {
    Ok(Some(
      SyntaxString::parse_string(&s).map_err(|_| serde::de::Error::custom("invalid syntax string"))?,
    ))
  } else {
    Ok(None)
  }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
enum CustomAtRuleBodyType {
  DeclarationList,
  RuleList,
  StyleBlock,
}

pub struct Prelude<'i> {
  name: CowArcStr<'i>,
  prelude: Option<ParsedComponent<'i>>,
}

#[derive(Serialize, Deserialize)]
pub struct AtRule<'i> {
  #[serde(borrow)]
  pub name: CowArcStr<'i>,
  pub prelude: Option<ParsedComponent<'i>>,
  pub body: Option<AtRuleBody<'i>>,
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "kebab-case")]
pub enum AtRuleBody<'i> {
  #[serde(borrow)]
  DeclarationList(DeclarationBlock<'i>),
  RuleList(CssRuleList<'i, AtRule<'i>>),
}

#[derive(Clone)]
pub struct CustomAtRuleParser {
  pub configs: HashMap<String, CustomAtRuleConfig>,
}

impl<'i> AtRuleParser<'i> for CustomAtRuleParser {
  type Prelude = Prelude<'i>;
  type Error = ParserError<'i>;
  type AtRule = AtRule<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
    _options: &ParserOptions<'_, 'i, Self>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    if let Some(config) = self.configs.get(name.as_ref()) {
      let prelude = if let Some(prelude) = &config.prelude {
        Some(prelude.parse_value(input)?)
      } else {
        None
      };
      Ok(Prelude {
        name: name.into(),
        prelude,
      })
    } else {
      Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    _start: &ParserState,
    input: &mut Parser<'i, 't>,
    options: &ParserOptions<'_, 'i, Self>,
  ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
    let config = self.configs.get(prelude.name.as_ref()).unwrap();
    let body = if let Some(body) = &config.body {
      match body {
        CustomAtRuleBodyType::DeclarationList => {
          Some(AtRuleBody::DeclarationList(DeclarationBlock::parse(input, options)?))
        }
        CustomAtRuleBodyType::RuleList => Some(AtRuleBody::RuleList(CssRuleList::parse(input, options)?)),
        CustomAtRuleBodyType::StyleBlock => {
          Some(AtRuleBody::RuleList(CssRuleList::parse_style_block(input, options)?))
        }
      }
    } else {
      None
    };

    Ok(AtRule {
      name: prelude.name,
      prelude: prelude.prelude,
      body,
    })
  }

  fn rule_without_block(
    &mut self,
    prelude: Self::Prelude,
    _start: &ParserState,
    _options: &ParserOptions<'_, 'i, Self>,
  ) -> Result<Self::AtRule, ()> {
    let config = self.configs.get(prelude.name.as_ref()).unwrap();
    if config.body.is_some() {
      return Err(());
    }

    Ok(AtRule {
      name: prelude.name,
      prelude: prelude.prelude,
      body: None,
    })
  }
}

impl<'i> ToCss for AtRule<'i> {
  fn to_css<W>(
    &self,
    dest: &mut lightningcss::printer::Printer<W>,
  ) -> Result<(), lightningcss::error::PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_char('@')?;
    serialize_identifier(&self.name, dest)?;
    if let Some(prelude) = &self.prelude {
      dest.write_char(' ')?;
      prelude.to_css(dest)?;
    }

    if let Some(body) = &self.body {
      match body {
        AtRuleBody::DeclarationList(decls) => {
          decls.to_css_block(dest)?;
        }
        AtRuleBody::RuleList(rules) => {
          dest.whitespace()?;
          dest.write_char('{')?;
          dest.indent();
          dest.newline()?;
          rules.to_css(dest)?;
          dest.dedent();
          dest.newline()?;
          dest.write_char('}')?;
        }
      }
    }

    Ok(())
  }
}

impl<'i, V: Visitor<'i, AtRule<'i>>> Visit<'i, AtRule<'i>, V> for AtRule<'i> {
  const CHILD_TYPES: VisitTypes = VisitTypes::empty();

  fn visit_children(&mut self, visitor: &mut V) {
    self.prelude.visit(visitor);
    match &mut self.body {
      Some(AtRuleBody::DeclarationList(decls)) => decls.visit(visitor),
      Some(AtRuleBody::RuleList(rules)) => rules.visit(visitor),
      None => {}
    }
  }
}
