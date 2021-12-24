use cssparser::*;
use parcel_selectors::{SelectorList, parser::NestingRequirement};
use crate::media_query::*;
use crate::traits::Parse;
use crate::selector::{Selectors, SelectorParser};
use crate::rules::{
  CssRule,
  CssRuleList,
  keyframes::{KeyframeListParser, KeyframesRule},
  font_face::{FontFaceRule, FontFaceDeclarationParser},
  page::{PageSelector, PageRule},
  supports::{SupportsCondition, SupportsRule},
  counter_style::CounterStyleRule,
  namespace::NamespaceRule,
  import::ImportRule,
  media::MediaRule,
  style::StyleRule,
  document::MozDocumentRule,
  nesting::NestingRule
};
use crate::values::ident::CustomIdent;
use crate::declaration::{DeclarationBlock, Declaration};
use crate::vendor_prefix::VendorPrefix;
use std::collections::HashMap;

/// The parser for the top-level rules in a stylesheet.
pub struct TopLevelRuleParser {
  default_namespace: Option<String>,
  namespace_prefixes: HashMap<String, String>
}

impl<'b> TopLevelRuleParser {
  pub fn new() -> TopLevelRuleParser {
    TopLevelRuleParser {
      default_namespace: None,
      namespace_prefixes: HashMap::new()
    }
  }

  fn nested<'a: 'b>(&'a mut self) -> NestedRuleParser {
      NestedRuleParser {
        default_namespace: &mut self.default_namespace,
        namespace_prefixes: &mut self.namespace_prefixes
      }
  }
}

/// A rule prelude for at-rule with block.
#[derive(Debug)]
#[allow(dead_code)]
pub enum AtRulePrelude {
  /// A @font-face rule prelude.
  FontFace,
  /// A @font-feature-values rule prelude, with its FamilyName list.
  FontFeatureValues,//(Vec<FamilyName>),
  /// A @counter-style rule prelude, with its counter style name.
  CounterStyle(CustomIdent),
  /// A @media rule prelude, with its media queries.
  Media(MediaList),//(Arc<Locked<MediaList>>),
  /// An @supports rule, with its conditional
  Supports(SupportsCondition),
  /// A @viewport rule prelude.
  Viewport,
  /// A @keyframes rule, with its animation name and vendor prefix if exists.
  Keyframes(String, VendorPrefix),
  /// A @page rule prelude.
  Page(Vec<PageSelector>),
  /// A @-moz-document rule.
  MozDocument,
  /// A @import rule prelude.
  Import(String, MediaList, Option<SupportsCondition>),
  /// A @namespace rule prelude.
  Namespace(Option<String>, String),
  /// A @charset rule prelude.
  Charset,
  /// A @nest prelude.
  Nest(SelectorList<Selectors>)
}

impl<'a, 'i> AtRuleParser<'i> for TopLevelRuleParser {
  type Prelude = AtRulePrelude;
  type AtRule = (SourcePosition, CssRule);
  type Error = ();

  fn parse_prelude<'t>(
      &mut self,
      name: CowRcStr<'i>,
      input: &mut Parser<'i, 't>,
  ) -> Result<AtRulePrelude, ParseError<'i, Self::Error>> {
      match_ignore_ascii_case! { &*name,
        "import" => {
          let url_string = input.expect_url_or_string()?.as_ref().to_owned();
          let supports = if input.try_parse(|input| input.expect_function_matching("supports")).is_ok() {
            Some(input.parse_nested_block(|input| {
              input.try_parse(SupportsCondition::parse).or_else(|_| SupportsCondition::parse_declaration(input))
            })?)
          } else {
            None
          };
          let media = MediaList::parse(input);
          return Ok(AtRulePrelude::Import(url_string, media, supports));
        },
        "namespace" => {
          let prefix = input.try_parse(|input| input.expect_ident_cloned()).map(|v| v.as_ref().to_owned()).ok();
          let namespace = input.expect_url_or_string()?.as_ref().to_owned();
          let prelude = AtRulePrelude::Namespace(prefix, namespace);
          return Ok(prelude);
        },
        "charset" => {
          // @charset is removed by rust-cssparser if it’s the first rule in the stylesheet.
          // Anything left is technically invalid, however, users often concatenate CSS files
          // together, so we are more lenient and simply ignore @charset rules in the middle of a file.
          input.expect_string()?;
          return Ok(AtRulePrelude::Charset)
        },
        _ => {}
      }

      AtRuleParser::parse_prelude(&mut self.nested(), name, input)
  }

  #[inline]
  fn parse_block<'t>(
      &mut self,
      prelude: AtRulePrelude,
      start: &ParserState,
      input: &mut Parser<'i, 't>,
  ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
    let rule = AtRuleParser::parse_block(&mut self.nested(), prelude, start, input)?;
    Ok((start.position(), rule))
  }

  #[inline]
  fn rule_without_block(
      &mut self,
      prelude: AtRulePrelude,
      start: &ParserState,
  ) -> Result<Self::AtRule, ()> {
      let loc = start.source_location();
      let rule = match prelude {
        AtRulePrelude::Import(url, media, supports) => {
          CssRule::Import(ImportRule {
            url,
            supports,
            media,
            loc
          })
        },
        AtRulePrelude::Namespace(prefix, url) => {
          if let Some(prefix) = &prefix {
            self.namespace_prefixes.insert(prefix.clone(), url.clone());
          } else {
            self.default_namespace = Some(url.clone());
          }

          CssRule::Namespace(NamespaceRule {
            prefix,
            url,
            loc
          })
        },
        AtRulePrelude::Charset => CssRule::Ignored,
        _ => return Err(())
      };

      Ok((start.position(), rule))
  }
}

impl<'a, 'i> QualifiedRuleParser<'i> for TopLevelRuleParser {
  type Prelude = SelectorList<Selectors>;
  type QualifiedRule = (SourcePosition, CssRule);
  type Error = ();

  #[inline]
  fn parse_prelude<'t>(
      &mut self,
      input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    QualifiedRuleParser::parse_prelude(&mut self.nested(), input)
  }

  #[inline]
  fn parse_block<'t>(
      &mut self,
      prelude: Self::Prelude,
      start: &ParserState,
      input: &mut Parser<'i, 't>,
  ) -> Result<Self::QualifiedRule, ParseError<'i, Self::Error>> {
    let rule = QualifiedRuleParser::parse_block(&mut self.nested(), prelude, start, input)?;
    Ok((start.position(), rule))
  }
}

#[derive(Clone)]
struct NestedRuleParser<'a> {
  default_namespace: &'a Option<String>,
  namespace_prefixes: &'a HashMap<String, String>
}

impl<'a, 'b> NestedRuleParser<'a> {
  fn parse_nested_rules(&mut self, input: &mut Parser) -> CssRuleList {
    let nested_parser = NestedRuleParser {
      default_namespace: self.default_namespace,
      namespace_prefixes: self.namespace_prefixes
    };

    let mut iter = RuleListParser::new_for_nested_rule(input, nested_parser);
    let mut rules = Vec::new();
    while let Some(result) = iter.next() {
      match result {
        Ok(CssRule::Ignored) => {},
        Ok(rule) => rules.push(rule),
        Err(_) => {
          // TODO
        },
      }
    }

    CssRuleList(rules)
  }
}

impl<'a, 'b, 'i> AtRuleParser<'i> for NestedRuleParser<'a> {
  type Prelude = AtRulePrelude;
  type AtRule = CssRule;
  type Error = ();

  fn parse_prelude<'t>(
      &mut self,
      name: CowRcStr<'i>,
      input: &mut Parser<'i, 't>,
  ) -> Result<AtRulePrelude, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &*name,
      "media" => {
        let media = MediaList::parse(input);
        Ok(AtRulePrelude::Media(media))
      },
      "supports" => {
        let cond = SupportsCondition::parse(input)?;
        Ok(AtRulePrelude::Supports(cond))
      },
      "font-face" => {
        Ok(AtRulePrelude::FontFace)
      },
      // "font-feature-values" => {
      //     if !cfg!(feature = "gecko") {
      //         // Support for this rule is not fully implemented in Servo yet.
      //         return Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
      //     }
      //     let family_names = parse_family_name_list(self.context, input)?;
      //     Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::FontFeatureValues(family_names)))
      // },
      "counter-style" => {
        let name = CustomIdent::parse(input)?;
        Ok(AtRulePrelude::CounterStyle(name))
      },
      // "viewport" => {
      //     if viewport_rule::enabled() {
      //         Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::Viewport))
      //     } else {
      //         Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
      //     }
      // },
      "keyframes" | "-webkit-keyframes" | "-moz-keyframes" | "-o-keyframes" | "-ms-keyframes" => {
        let prefix = if starts_with_ignore_ascii_case(&*name, "-webkit-") {
          VendorPrefix::WebKit
        } else if starts_with_ignore_ascii_case(&*name, "-moz-") {
          VendorPrefix::Moz
        } else if starts_with_ignore_ascii_case(&*name, "-o-") {
          VendorPrefix::O
        } else if starts_with_ignore_ascii_case(&*name, "-ms-") {
          VendorPrefix::Ms
        } else {
          VendorPrefix::None
        };

        let location = input.current_source_location();
        let name = match *input.next()? {
          Token::Ident(ref s) => s.as_ref(),
          Token::QuotedString(ref s) => s.as_ref(),
          ref t => return Err(location.new_unexpected_token_error(t.clone())),
        };

        Ok(AtRulePrelude::Keyframes(name.into(), prefix))
      },
      "page" => {
        let selectors = input.try_parse(|input| input.parse_comma_separated(PageSelector::parse)).unwrap_or_default();
        Ok(AtRulePrelude::Page(selectors))
      },
      "-moz-document" => {
        // Firefox only supports the url-prefix() function with no arguments as a legacy CSS hack.
        // See https://css-tricks.com/snippets/css/css-hacks-targeting-firefox/
        input.expect_function_matching("url-prefix")?;
        input.parse_nested_block(|input| input.expect_exhausted().map_err(|e| e.into()))?;

        Ok(AtRulePrelude::MozDocument)
      },
      _ => Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: AtRulePrelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<CssRule, ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    match prelude {
      AtRulePrelude::FontFace => {
        let mut parser = DeclarationListParser::new(input, FontFaceDeclarationParser);
        let mut properties = vec![];
        while let Some(decl) = parser.next() {
          if let Ok(decl) = decl {
            properties.push(decl);
          }
        }
        Ok(CssRule::FontFace(FontFaceRule {
          properties,
          loc
        }))
      },
      // AtRuleBlockPrelude::FontFeatureValues(family_names) => {
      //     let context = ParserContext::new_with_rule_type(
      //         self.context,
      //         CssRuleType::FontFeatureValues,
      //         self.namespaces,
      //     );

      //     Ok(CssRule::FontFeatureValues(Arc::new(self.shared_lock.wrap(
      //         FontFeatureValuesRule::parse(
      //             &context,
      //             input,
      //             family_names,
      //             start.source_location(),
      //         ),
      //     ))))
      // },
      AtRulePrelude::CounterStyle(name) => {
        Ok(CssRule::CounterStyle(CounterStyleRule {
          name,
          declarations: DeclarationBlock::parse(input)?,
          loc
        }))
      },
      AtRulePrelude::Media(query) => {
        Ok(CssRule::Media(MediaRule {
          query,
          rules: self.parse_nested_rules(input),
          loc
        }))
      },
      AtRulePrelude::Supports(condition) => {
        Ok(CssRule::Supports(SupportsRule {
          condition,
          rules: self.parse_nested_rules(input),
          loc
        }))
      },
      // AtRuleBlockPrelude::Viewport => {
      //     let context = ParserContext::new_with_rule_type(
      //         self.context,
      //         CssRuleType::Viewport,
      //         self.namespaces,
      //     );

      //     Ok(CssRule::Viewport(Arc::new(
      //         self.shared_lock.wrap(ViewportRule::parse(&context, input)?),
      //     )))
      // },
      AtRulePrelude::Keyframes(name, vendor_prefix) => {
        let iter = RuleListParser::new_for_nested_rule(input, KeyframeListParser);
        Ok(CssRule::Keyframes(KeyframesRule {
          name,
          keyframes: iter.filter_map(Result::ok).collect(),
          vendor_prefix,
          loc
        }))
      },
      AtRulePrelude::Page(selectors) => {
        Ok(CssRule::Page(PageRule {
          selectors,
          declarations: DeclarationBlock::parse(input)?,
          loc
        }))
      },
      AtRulePrelude::MozDocument => {
        Ok(CssRule::MozDocument(MozDocumentRule {
          rules: self.parse_nested_rules(input),
          loc
        }))
      },
      // _ => Ok()
      _ => {
        println!("{:?}", prelude);
        unreachable!()
      }
    }
  }
}

impl<'a, 'b, 'i> QualifiedRuleParser<'i> for NestedRuleParser<'a> {
  type Prelude = SelectorList<Selectors>;
  type QualifiedRule = CssRule;
  type Error = ();

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let selector_parser = SelectorParser {
      default_namespace: self.default_namespace,
      namespace_prefixes: self.namespace_prefixes
    };
    match SelectorList::parse(&selector_parser, input, NestingRequirement::None) {
      Ok(x) => Ok(x),
      Err(_) => Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<CssRule, ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    let (declarations, rules) = parse_declaration_list(input, self.default_namespace, self.namespace_prefixes)?;
    Ok(CssRule::Style(StyleRule {
      selectors,
      vendor_prefix: VendorPrefix::empty(),
      declarations,
      rules,
      loc
    }))
  }
}

#[derive(Debug)]
pub enum DeclarationOrRule {
  Declaration(Declaration),
  Rule(CssRule)
}

fn parse_declaration_list<'a, 'i, 't>(
  input: &mut Parser<'i, 't>,
  default_namespace: &'a Option<String>,
  namespace_prefixes: &'a HashMap<String, String>
) -> Result<(DeclarationBlock, CssRuleList), ParseError<'i, ()>> {
  let mut declarations = vec![];
  let mut rules = vec![];
  let parser = PropertyDeclarationParser {
    default_namespace,
    namespace_prefixes
  };

  let mut declaration_parser = DeclarationListParser::new(input, parser);
  let mut last = declaration_parser.input.state();
  while let Some(decl) = declaration_parser.next() {
    match decl {
      Ok(DeclarationOrRule::Declaration(decl)) => {
        if rules.len() > 0 {
          // Declarations cannot come after nested rules.
          return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }

        declarations.push(decl);
      }
      Ok(DeclarationOrRule::Rule(rule)) => rules.push(rule),
      _ => {
        declaration_parser.input.reset(&last);
        break
      }
    }

    last = declaration_parser.input.state();
  }

  let mut iter = RuleListParser::new_for_nested_rule(declaration_parser.input, declaration_parser.parser);
  while let Some(result) = iter.next() {
    match result {
      Ok(DeclarationOrRule::Rule(CssRule::Ignored)) => {},
      Ok(DeclarationOrRule::Rule(rule)) => rules.push(rule),
      Ok(DeclarationOrRule::Declaration(_)) => unreachable!(),
      Err(_) => {
        // TODO
      },
    }
  }

  Ok((DeclarationBlock { declarations }, CssRuleList(rules)))
}

pub struct PropertyDeclarationParser<'a> {
  default_namespace: &'a Option<String>,
  namespace_prefixes: &'a HashMap<String, String>
}

/// Parse a declaration within {} block: `color: blue`
impl<'a, 'i> cssparser::DeclarationParser<'i> for PropertyDeclarationParser<'a> {
  type Declaration = DeclarationOrRule;
  type Error = ();

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    Ok(DeclarationOrRule::Declaration(Declaration::parse(name, input)?))
  }
}

impl<'a, 'i> AtRuleParser<'i> for PropertyDeclarationParser<'a> {
  type Prelude = AtRulePrelude;
  type AtRule = DeclarationOrRule;
  type Error = ();

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<AtRulePrelude, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &*name,
      "media" => {
        let media = MediaList::parse(input);
        Ok(AtRulePrelude::Media(media))
      },
      "nest" => {
        let selector_parser = SelectorParser {
          default_namespace: self.default_namespace,
          namespace_prefixes: self.namespace_prefixes
        };
        let selectors = match SelectorList::parse(&selector_parser, input, NestingRequirement::Contained) {
          Ok(x) => x,
          Err(_) => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        };
        Ok(AtRulePrelude::Nest(selectors))
      },
      _ => Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: AtRulePrelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<DeclarationOrRule, ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    match prelude {
      AtRulePrelude::Media(query) => {
        let (declarations, mut rules) = parse_declaration_list(input, self.default_namespace, self.namespace_prefixes)?;

        if declarations.declarations.len() > 0 {
          rules.0.insert(0, CssRule::Style(StyleRule {
            selectors: SelectorList(smallvec::smallvec![parcel_selectors::parser::Selector::from_vec2(vec![parcel_selectors::parser::Component::Nesting])]),
            declarations,
            vendor_prefix: VendorPrefix::empty(),
            rules: CssRuleList(vec![]),
            loc: loc.clone()
          }))
        }
        
        Ok(DeclarationOrRule::Rule(CssRule::Media(MediaRule {
          query,
          rules,
          loc
        })))
      },
      AtRulePrelude::Nest(selectors) => {
        let (declarations, rules) = parse_declaration_list(input, self.default_namespace, self.namespace_prefixes)?;
        Ok(DeclarationOrRule::Rule(CssRule::Nesting(NestingRule {
          style: StyleRule {
            selectors,
            declarations,
            vendor_prefix: VendorPrefix::empty(),
            rules,
            loc
          },
          loc
        })))
      },
      _ => {
        println!("{:?}", prelude);
        unreachable!()
      }
    }
  }
}

impl<'a, 'b, 'i> QualifiedRuleParser<'i> for PropertyDeclarationParser<'a> {
  type Prelude = SelectorList<Selectors>;
  type QualifiedRule = DeclarationOrRule;
  type Error = ();

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let selector_parser = SelectorParser {
      default_namespace: self.default_namespace,
      namespace_prefixes: self.namespace_prefixes
    };
    match SelectorList::parse(&selector_parser, input, NestingRequirement::Prefixed) {
      Ok(x) => Ok(x),
      Err(_) => Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<DeclarationOrRule, ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    let (declarations, rules) = parse_declaration_list(input, self.default_namespace, self.namespace_prefixes)?;
    Ok(DeclarationOrRule::Rule(CssRule::Style(StyleRule {
      selectors,
      vendor_prefix: VendorPrefix::empty(),
      declarations,
      rules,
      loc
    })))
  }
}

fn starts_with_ignore_ascii_case(string: &str, prefix: &str) -> bool {
  string.len() >= prefix.len() && string.as_bytes()[0..prefix.len()].eq_ignore_ascii_case(prefix.as_bytes())
}
