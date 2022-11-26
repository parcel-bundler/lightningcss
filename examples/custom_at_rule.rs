use std::collections::HashMap;

use cssparser::*;
use lightningcss::{
  declaration::DeclarationBlock,
  error::PrinterError,
  printer::Printer,
  properties::custom::{Token, TokenOrValue},
  rules::{style::StyleRule, CssRule, CssRuleList, Location},
  selector::{Component, Selector},
  stylesheet::{ParserOptions, PrinterOptions, StyleSheet},
  targets::Browsers,
  traits::ToCss,
  values::{color::CssColor, length::LengthValue},
  vendor_prefix::VendorPrefix,
  visit_types,
  visitor::{Visit, VisitTypes, Visitor},
};

fn main() {
  let args: Vec<String> = std::env::args().collect();
  let source = std::fs::read_to_string(&args[1]).unwrap();
  let opts = ParserOptions {
    at_rule_parser: Some(TailwindAtRuleParser),
    filename: args[1].clone(),
    nesting: true,
    custom_media: false,
    css_modules: None,
    error_recovery: false,
    warnings: None,
    source_index: 0,
  };

  let mut stylesheet = StyleSheet::parse(&source, opts).unwrap();

  println!("{:?}", stylesheet);

  let mut style_rules = HashMap::new();
  stylesheet.visit(&mut StyleRuleCollector {
    rules: &mut style_rules,
  });
  println!("{:?}", style_rules);
  stylesheet.visit(&mut ApplyVisitor { rules: &style_rules });

  let result = stylesheet
    .to_css(PrinterOptions {
      targets: Some(Browsers {
        chrome: Some(100 << 16),
        ..Browsers::default()
      }),
      ..PrinterOptions::default()
    })
    .unwrap();
  println!("{}", result.code);
}

/// An @tailwind directive.
#[derive(Debug, Clone)]
enum TailwindDirective {
  Base,
  Components,
  Utilities,
  Variants,
}

/// A custom at rule prelude.
enum Prelude {
  Tailwind(TailwindDirective),
  Apply(Vec<String>),
}

/// A @tailwind rule.
#[derive(Debug, Clone)]
struct TailwindRule {
  directive: TailwindDirective,
  loc: SourceLocation,
}

/// An @apply rule.
#[derive(Debug, Clone)]
struct ApplyRule {
  names: Vec<String>,
  loc: SourceLocation,
}

/// A custom at rule.
#[derive(Debug, Clone)]
enum AtRule {
  Tailwind(TailwindRule),
  Apply(ApplyRule),
}

#[derive(Debug)]
struct TailwindAtRuleParser;
impl<'i> AtRuleParser<'i> for TailwindAtRuleParser {
  type Prelude = Prelude;
  type Error = ();
  type AtRule = AtRule;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! {&*name,
      "tailwind" => {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        let directive = match_ignore_ascii_case! { &*ident,
          "base" => TailwindDirective::Base,
          "components" => TailwindDirective::Components,
          "utilities" => TailwindDirective::Utilities,
          "variants" => TailwindDirective::Variants,
          _ => return Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        };
        Ok(Prelude::Tailwind(directive))
      },
      "apply" => {
        let mut names = Vec::new();
        loop {
          if let Ok(name) = input.try_parse(|input| input.expect_ident_cloned()) {
            names.push(name.as_ref().into());
          } else {
            break
          }
        }

        Ok(Prelude::Apply(names))
      },
      _ => Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
    }
  }

  fn rule_without_block(&mut self, prelude: Self::Prelude, start: &ParserState) -> Result<Self::AtRule, ()> {
    let loc = start.source_location();
    match prelude {
      Prelude::Tailwind(directive) => Ok(AtRule::Tailwind(TailwindRule { directive, loc })),
      Prelude::Apply(names) => Ok(AtRule::Apply(ApplyRule { names, loc })),
    }
  }
}

struct StyleRuleCollector<'i, 'a> {
  rules: &'a mut HashMap<String, DeclarationBlock<'i>>,
}

impl<'i, 'a> Visitor<'i, AtRule> for StyleRuleCollector<'i, 'a> {
  const TYPES: VisitTypes = VisitTypes::RULES;

  fn visit_rule(&mut self, rule: &mut lightningcss::rules::CssRule<'i, AtRule>) {
    match rule {
      CssRule::Style(rule) => {
        for selector in rule.selectors.0.iter() {
          if selector.len() != 1 {
            continue; // TODO
          }
          for component in selector.iter_raw_match_order() {
            match component {
              Component::Class(name) => {
                self.rules.insert(name.0.to_string(), rule.declarations.clone());
              }
              _ => {}
            }
          }
        }
      }
      _ => {}
    }

    rule.visit_children(self)
  }
}

struct ApplyVisitor<'a, 'i> {
  rules: &'a HashMap<String, DeclarationBlock<'i>>,
}

impl<'a, 'i> Visitor<'i, AtRule> for ApplyVisitor<'a, 'i> {
  const TYPES: VisitTypes = visit_types!(RULES | COLORS | LENGTHS | DASHED_IDENTS | SELECTORS | TOKENS);

  fn visit_rule(&mut self, rule: &mut CssRule<'i, AtRule>) {
    // Replace @apply rule with nested style rule.
    if let CssRule::Custom(AtRule::Apply(apply)) = rule {
      let mut declarations = DeclarationBlock::new();
      for name in &apply.names {
        let applied = self.rules.get(name).unwrap();
        declarations
          .important_declarations
          .extend(applied.important_declarations.iter().cloned());
        declarations.declarations.extend(applied.declarations.iter().cloned());
      }
      *rule = CssRule::Style(StyleRule {
        selectors: Component::Nesting.into(),
        vendor_prefix: VendorPrefix::None,
        declarations,
        rules: CssRuleList(vec![]),
        loc: Location {
          source_index: 0,
          line: apply.loc.line,
          column: apply.loc.column,
        },
      })
    }

    rule.visit_children(self)
  }

  fn visit_url(&mut self, url: &mut lightningcss::values::url::Url<'i>) {
    url.url = format!("https://mywebsite.com/{}", url.url).into()
  }

  fn visit_color(&mut self, color: &mut lightningcss::values::color::CssColor) {
    *color = color.to_lab()
  }

  fn visit_length(&mut self, length: &mut lightningcss::values::length::LengthValue) {
    match length {
      LengthValue::Px(px) => *length = LengthValue::Rem(*px / 16.0),
      _ => {}
    }
  }

  fn visit_dashed_ident(&mut self, ident: &mut lightningcss::values::ident::DashedIdent) {
    ident.0 = format!("--tw-{}", &ident.0[2..]).into()
  }

  fn visit_selector(&mut self, selector: &mut Selector<'i>) {
    for c in selector.iter_mut_raw_match_order() {
      match c {
        Component::Class(c) => {
          *c = format!("tw-{}", c).into();
        }
        _ => {}
      }
    }
  }

  fn visit_token(&mut self, token: &mut TokenOrValue<'i>) {
    match token {
      TokenOrValue::Function(f) if f.name == "theme" => match f.arguments.0.first() {
        Some(TokenOrValue::Token(Token::String(s))) => match s.as_ref() {
          "blue-500" => *token = TokenOrValue::Color(CssColor::RGBA(RGBA::new(0, 0, 255, 255))),
          "red-500" => *token = TokenOrValue::Color(CssColor::RGBA(RGBA::new(255, 0, 0, 255))),
          _ => {}
        },
        _ => {}
      },
      _ => {}
    }

    token.visit_children(self)
  }
}

impl<'i, V: Visitor<'i, AtRule>> Visit<'i, AtRule, V> for AtRule {
  const CHILD_TYPES: VisitTypes = VisitTypes::empty();

  fn visit_children(&mut self, _: &mut V) {}
}

impl ToCss for AtRule {
  fn to_css<W: std::fmt::Write>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> {
    match self {
      AtRule::Tailwind(rule) => {
        let _ = rule.loc; // TODO: source maps
        let directive = match rule.directive {
          TailwindDirective::Base => "TAILWIND BASE HERE",
          TailwindDirective::Components => "TAILWIND COMPONENTS HERE",
          TailwindDirective::Utilities => "TAILWIND UTILITIES HERE",
          TailwindDirective::Variants => "TAILWIND VARIANTS HERE",
        };
        dest.write_str(directive)
      }
      AtRule::Apply(_) => Ok(()),
    }
  }
}
