use cssparser::*;
use selectors::SelectorList;
use std::fmt;
use std::cell::RefCell;
use crate::media_query::*;
use crate::traits::Parse;
use std::fmt::Write;
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
  style::StyleRule
};
use crate::values::ident::CustomIdent;
use crate::declaration::{Declaration, DeclarationBlock};
use crate::properties::VendorPrefix;

#[derive(Eq, PartialEq, Clone)]
pub struct CssString(RefCell<String>);

impl CssString {
  // pub fn replace(&self, x: String) {
  //   self.0.replace(x);
  // }

  pub fn write_identifier<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
    serialize_identifier(self.0.borrow().as_ref(), dest)
  }

  pub fn write_string<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
    let b = self.0.borrow();
    let s: &str = b.as_ref();
    write!(CssStringWriter::new(dest), "{}", s)
  }
}

impl cssparser::ToCss for CssString {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result where W: fmt::Write {
    dest.write_str(self.0.borrow().as_ref())
  }
}

impl<'a> std::convert::From<&'a str> for CssString {
  fn from(s: &str) -> CssString {
    CssString(RefCell::new(s.into()))
  }
}

impl std::borrow::Borrow<String> for CssString {
  fn borrow(&self) -> &String {
    unsafe { self.0.try_borrow_unguarded() }.unwrap()
  }
}

impl std::default::Default for CssString {
  fn default() -> CssString {
    CssString(RefCell::new(String::default()))
  }
}

impl std::cmp::PartialEq<str> for CssString {
  fn eq(&self, rhs: &str) -> bool {
    self.0.borrow().eq(rhs)
  }
}

/// The parser for the top-level rules in a stylesheet.
pub struct TopLevelRuleParser {}

impl<'b> TopLevelRuleParser {
  fn nested<'a: 'b>(&'a self) -> NestedRuleParser {
      NestedRuleParser {}
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
  /// A @document rule, with its conditional.
  Document,//(DocumentCondition),
  /// A @import rule prelude.
  Import(String, MediaList, Option<SupportsCondition>),
  /// A @namespace rule prelude.
  Namespace(Option<String>, String),
}

impl<'a, 'i> AtRuleParser<'i> for TopLevelRuleParser {
  type PreludeNoBlock = AtRulePrelude;
  type PreludeBlock = AtRulePrelude;
  type AtRule = (SourcePosition, CssRule);
  type Error = ();

  fn parse_prelude<'t>(
      &mut self,
      name: CowRcStr<'i>,
      input: &mut Parser<'i, 't>,
  ) -> Result<AtRuleType<AtRulePrelude, AtRulePrelude>, ParseError<'i, Self::Error>> {
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
          return Ok(AtRuleType::WithoutBlock(AtRulePrelude::Import(url_string, media, supports)));
        },
        "namespace" => {
          let prefix = input.try_parse(|input| input.expect_ident_cloned()).map(|v| v.as_ref().to_owned()).ok();
          let namespace = input.expect_url_or_string()?.as_ref().to_owned();
          let prelude = AtRulePrelude::Namespace(prefix, namespace);
          return Ok(AtRuleType::WithoutBlock(prelude));
        },
        // @charset is removed by rust-cssparser if it’s the first rule in the stylesheet
        // anything left is invalid.
        "charset" => {
          return Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
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
  ) -> Self::AtRule {
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
          CssRule::Namespace(NamespaceRule {
            prefix,
            url,
            loc
          })
        },
        _ => unreachable!()
      };

      (start.position(), rule)
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
struct NestedRuleParser {}

impl<'a, 'b> NestedRuleParser {
  fn parse_nested_rules(&mut self, input: &mut Parser) -> CssRuleList {
    let nested_parser = NestedRuleParser {};

    let mut iter = RuleListParser::new_for_nested_rule(input, nested_parser);
    let mut rules = Vec::new();
    while let Some(result) = iter.next() {
      match result {
        Ok(rule) => rules.push(rule),
        Err(_) => {
          // TODO
        },
      }
    }

    CssRuleList(rules)
  }
}

impl<'a, 'b, 'i> AtRuleParser<'i> for NestedRuleParser {
  type PreludeNoBlock = AtRulePrelude;
  type PreludeBlock = AtRulePrelude;
  type AtRule = CssRule;
  type Error = ();

  fn parse_prelude<'t>(
      &mut self,
      name: CowRcStr<'i>,
      input: &mut Parser<'i, 't>,
  ) -> Result<AtRuleType<AtRulePrelude, AtRulePrelude>, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &*name,
      "media" => {
        let media = MediaList::parse(input);
        Ok(AtRuleType::WithBlock(AtRulePrelude::Media(media)))
      },
      "supports" => {
        let cond = SupportsCondition::parse(input)?;
        Ok(AtRuleType::WithBlock(AtRulePrelude::Supports(cond)))
      },
      "font-face" => {
        Ok(AtRuleType::WithBlock(AtRulePrelude::FontFace))
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
        Ok(AtRuleType::WithBlock(AtRulePrelude::CounterStyle(name)))
      },
      // "viewport" => {
      //     if viewport_rule::enabled() {
      //         Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::Viewport))
      //     } else {
      //         Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
      //     }
      // },
      "keyframes" | "-webkit-keyframes" | "-moz-keyframes" | "-o-keyframes" => {
        let prefix = if starts_with_ignore_ascii_case(&*name, "-webkit-") {
          VendorPrefix::WebKit
        } else if starts_with_ignore_ascii_case(&*name, "-moz-") {
          VendorPrefix::Moz
        } else if starts_with_ignore_ascii_case(&*name, "-o-") {
          VendorPrefix::O
        } else {
          VendorPrefix::None
        };

        let location = input.current_source_location();
        let name = match *input.next()? {
          Token::Ident(ref s) => s.as_ref(),
          Token::QuotedString(ref s) => s.as_ref(),
          ref t => return Err(location.new_unexpected_token_error(t.clone())),
        };

        Ok(AtRuleType::WithBlock(AtRulePrelude::Keyframes(name.into(), prefix)))
      },
      "page" => {
        let selectors = input.try_parse(|input| input.parse_comma_separated(PageSelector::parse)).unwrap_or_default();
        Ok(AtRuleType::WithBlock(AtRulePrelude::Page(selectors)))
      },
      // "-moz-document" => {
      //     if !cfg!(feature = "gecko") {
      //         return Err(input.new_custom_error(
      //             StyleParseErrorKind::UnsupportedAtRule(name.clone())
      //         ))
      //     }

      //     let cond = DocumentCondition::parse(self.context, input)?;
      //     Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::Document(cond)))
      // },
      // _ => Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
      _ => {
        print!("UNKNOWN AT RULE {}", name);
        Ok(AtRuleType::WithBlock(AtRulePrelude::FontFeatureValues))
      }
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
        let mut parser = DeclarationListParser::new(input, PropertyDeclarationParser);
        let mut declarations = vec![];
        while let Some(decl) = parser.next() {
          if let Ok(decl) = decl {
            declarations.push(decl);
          }
        }

        Ok(CssRule::CounterStyle(CounterStyleRule {
          name,
          declarations: DeclarationBlock {
            declarations
          },
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
        let mut parser = DeclarationListParser::new(input, PropertyDeclarationParser);
        let mut declarations = vec![];
        while let Some(decl) = parser.next() {
          if let Ok(decl) = decl {
            declarations.push(decl);
          }
        }
        Ok(CssRule::Page(PageRule {
          selectors,
          declarations: DeclarationBlock {
            declarations
          },
          loc
        }))
      },
      // AtRuleBlockPrelude::Document(condition) => {
      //     if !cfg!(feature = "gecko") {
      //         unreachable!()
      //     }
      //     Ok(CssRule::Document(Arc::new(self.shared_lock.wrap(
      //         DocumentRule {
      //             condition,
      //             rules: self.parse_nested_rules(input, CssRuleType::Document),
      //             source_location: start.source_location(),
      //         },
      //     ))))
      // },
      // _ => Ok()
      _ => {
        println!("{:?}", prelude);
        unreachable!()
      }
    }
  }
}

impl<'a, 'b, 'i> QualifiedRuleParser<'i> for NestedRuleParser {
  type Prelude = SelectorList<Selectors>;
  type QualifiedRule = CssRule;
  type Error = ();

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let selector_parser = SelectorParser {};
    match SelectorList::parse(&selector_parser, input) {
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
    let mut parser = DeclarationListParser::new(input, PropertyDeclarationParser);
    let mut declarations = vec![];
    while let Some(decl) = parser.next() {
      if let Ok(decl) = decl {
        declarations.push(decl);
      }
    }

    Ok(CssRule::Style(StyleRule {
      selectors,
      vendor_prefix: VendorPrefix::empty(),
      declarations: DeclarationBlock {
        declarations
      },
      loc
    }))
  }
}

pub struct PropertyDeclarationParser;

/// Parse a declaration within {} block: `color: blue`
impl<'i> cssparser::DeclarationParser<'i> for PropertyDeclarationParser {
  type Declaration = Declaration;
  type Error = ();

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    Declaration::parse(name, input)
  }
}

/// Default methods reject all at rules.
impl<'i> AtRuleParser<'i> for PropertyDeclarationParser {
  type PreludeNoBlock = ();
  type PreludeBlock = ();
  type AtRule = Declaration;
  type Error = ();
}

fn starts_with_ignore_ascii_case(string: &str, prefix: &str) -> bool {
  string.len() >= prefix.len() && string.as_bytes()[0..prefix.len()].eq_ignore_ascii_case(prefix.as_bytes())
}
