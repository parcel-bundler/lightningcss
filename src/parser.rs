use cssparser::*;
use selectors::SelectorList;
use std::fmt;
use std::cell::RefCell;
use crate::media_query::*;
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use std::fmt::Write;
use crate::selector::{Selectors, SelectorParser};
use crate::rules::keyframes::{KeyframeListParser, KeyframesRule};
use crate::rules::font_face::{FontFaceRule, FontFaceDeclarationParser};
use crate::rules::page::{PageSelector, PageRule};
use crate::declaration::{Declaration, DeclarationHandler};
use crate::properties::prefixes::Browsers;

#[derive(Eq, PartialEq, Clone)]
pub struct CssString(RefCell<String>);

impl CssString {
  pub fn replace(&self, x: String) {
    self.0.replace(x);
  }

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
pub struct TopLevelRuleParser {
}

impl<'b> TopLevelRuleParser {
  fn nested<'a: 'b>(&'a self) -> NestedRuleParser {
      NestedRuleParser {}
  }
}

#[derive(Clone, Debug)]
/// Vendor prefix.
enum VendorPrefix {
  /// -moz prefix.
  Moz,
  /// -webkit prefix.
  WebKit,
}

/// A rule prelude for at-rule with block.
#[derive(Debug)]
pub enum AtRulePrelude {
  /// A @font-face rule prelude.
  FontFace,
  /// A @font-feature-values rule prelude, with its FamilyName list.
  FontFeatureValues,//(Vec<FamilyName>),
  /// A @counter-style rule prelude, with its counter style name.
  CounterStyle,//(CustomIdent),
  /// A @media rule prelude, with its media queries.
  Media(MediaList),//(Arc<Locked<MediaList>>),
  /// An @supports rule, with its conditional
  Supports,//(SupportsCondition),
  /// A @viewport rule prelude.
  Viewport,
  /// A @keyframes rule, with its animation name and vendor prefix if exists.
  Keyframes(String),//(KeyframesName, Option<VendorPrefix>),
  /// A @page rule prelude.
  Page(Vec<PageSelector>),
  /// A @document rule, with its conditional.
  Document,//(DocumentCondition),
  /// A @import rule prelude.
  Import(String, MediaList),//(CssUrl, Arc<Locked<MediaList>>),
  /// A @namespace rule prelude.
  Namespace,//(Option<Prefix>, Namespace),
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
              // if !self.check_state(State::Imports) {
              //     return Err(input.new_custom_error(StyleParseErrorKind::UnexpectedImportRule))
              // }

              // if let AllowImportRules::No = self.allow_import_rules {
              //     return Err(input.new_custom_error(StyleParseErrorKind::DisallowedImportRule))
              // }

              // // FIXME(emilio): We should always be able to have a loader
              // // around! See bug 1533783.
              // if self.loader.is_none() {
              //     error!("Saw @import rule, but no way to trigger the load");
              //     return Err(input.new_custom_error(StyleParseErrorKind::UnexpectedImportRule))
              // }

              let url_string = input.expect_url_or_string()?.as_ref().to_owned();
              // let media = exhaust(input);
              // let url = CssUrl::parse_from_string(url_string, &self.context, CorsMode::None);

              let media = MediaList::parse(input);
              // let media = Arc::new(self.shared_lock.wrap(media));

              // let prelude = AtRuleNonBlockPrelude::Import(url, media);

              return Ok(AtRuleType::WithoutBlock(AtRulePrelude::Import(url_string, media)));
          },
          // "namespace" => {
          //     if !self.check_state(State::Namespaces) {
          //         return Err(input.new_custom_error(StyleParseErrorKind::UnexpectedNamespaceRule))
          //     }

          //     let prefix = input.try_parse(|i| i.expect_ident_cloned())
          //                       .map(|s| Prefix::from(s.as_ref())).ok();
          //     let maybe_namespace = match input.expect_url_or_string() {
          //         Ok(url_or_string) => url_or_string,
          //         Err(BasicParseError { kind: BasicParseErrorKind::UnexpectedToken(t), location }) => {
          //             return Err(location.new_custom_error(StyleParseErrorKind::UnexpectedTokenWithinNamespace(t)))
          //         }
          //         Err(e) => return Err(e.into()),
          //     };
          //     let url = Namespace::from(maybe_namespace.as_ref());
          //     let prelude = AtRuleNonBlockPrelude::Namespace(prefix, url);
          //     return Ok(AtRuleType::WithoutBlock(prelude));
          // },
          // // @charset is removed by rust-cssparser if it’s the first rule in the stylesheet
          // // anything left is invalid.
          // "charset" => {
          //     self.dom_error = Some(RulesMutateError::HierarchyRequest);
          //     return Err(input.new_custom_error(StyleParseErrorKind::UnexpectedCharsetRule))
          // },
          _ => {}
      }

      // if !self.check_state(State::Body) {
      //     return Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError));
      // }

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
      // self.state = State::Body;
      Ok((start.position(), rule))
  }

  #[inline]
  fn rule_without_block(
      &mut self,
      prelude: AtRulePrelude,
      start: &ParserState,
  ) -> Self::AtRule {
      let rule = match prelude {
          AtRulePrelude::Import(url, media) => {
              // let loader = self
              //     .loader
              //     .expect("Expected a stylesheet loader for @import");

              // let import_rule = loader.request_stylesheet(
              //     url,
              //     start.source_location(),
              //     &self.context,
              //     &self.shared_lock,
              //     media,
              // );

              // self.state = State::Imports;
              CssRule::Import(ImportRule {
                url,
                media
              })
          },
          // AtRuleNonBlockPrelude::Namespace(prefix, url) => {
          //     let prefix = if let Some(prefix) = prefix {
          //         self.namespaces.prefixes.insert(prefix.clone(), url.clone());
          //         Some(prefix)
          //     } else {
          //         self.namespaces.default = Some(url.clone());
          //         None
          //     };

          //     self.state = State::Namespaces;
          //     CssRule::Namespace(Arc::new(self.shared_lock.wrap(NamespaceRule {
          //         prefix,
          //         url,
          //         source_location: start.source_location(),
          //     })))
          // },
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
      // if !self.check_state(State::Body) {
      //     return Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError));
      // }

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
      // self.state = State::Body;
      // Ok((start.position(), rule))
      Ok((start.position(), rule))
  }
}

#[derive(Clone)] // shallow, relatively cheap .clone
struct NestedRuleParser {
}

#[derive(Debug, PartialEq)]
pub struct MediaRule {
  query: MediaList,
  rules: Vec<CssRule>
}

impl ToCss for MediaRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    dest.write_str("@media ")?;
    self.query.to_css(dest)?;
    // serialize_string(&self.query, dest)?;
    // dest.write_str(";")
    // dest.write_str(" {")?;
    dest.whitespace()?;
    dest.write_char('{')?;
    for rule in self.rules.iter() {
      if !dest.minify {
        dest.write_str("\n  ")?;
      }
      rule.to_css(dest)?;
    }
    dest.newline()?;
    dest.write_char('}')
  }
}

#[derive(Debug, PartialEq)]
pub struct ImportRule {
  pub url: String,
  pub media: MediaList
}

impl ToCss for ImportRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    dest.write_str("@import ")?;
    serialize_string(&self.url, dest)?;
    // dest.write_str(&self.media)?;
    dest.write_str(";")
  }
}

#[derive(Debug, PartialEq)]
pub struct StyleRule {
  pub selectors: SelectorList<Selectors>,
  pub declarations: DeclarationBlock
}

impl ToCss for StyleRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    self.selectors.to_css(dest)?;
    self.declarations.to_css(dest)
  }
}

#[derive(Debug, PartialEq)]
pub struct DeclarationBlock {
  pub declarations: Vec<Declaration>
}

impl ToCss for DeclarationBlock {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    dest.whitespace()?;
    dest.write_char('{')?;
    let len = self.declarations.len();
    for (i, decl) in self.declarations.iter().enumerate() {
      if !dest.minify {
        dest.write_str("\n  ")?;
      }
      decl.to_css(dest)?;
      if i != len - 1 || !dest.minify {
        dest.write_char(';')?;
      }
    }
    dest.newline()?;
    dest.write_char('}')
  }
}

impl DeclarationBlock {
  pub fn minify(&mut self, targets: Option<Browsers>) {
    let mut handler = DeclarationHandler::new(false, targets);
    let mut important_handler = DeclarationHandler::new(true, targets);

    let mut decls: Vec<Declaration> = vec![];
    for decl in self.declarations.iter() {
      let handled = 
        (decl.important && important_handler.handle_property(decl)) ||
        (!decl.important && handler.handle_property(decl));

      if !handled {
        decls.push(decl.clone());
      }
    }

    decls.extend(handler.finalize());
    decls.extend(important_handler.finalize());
    self.declarations = decls;
  }
}

#[derive(Debug, PartialEq)]
pub enum CssRule {
  Media(MediaRule),
  Import(ImportRule),
  Style(StyleRule),
  Keyframes(KeyframesRule),
  FontFace(FontFaceRule),
  Page(PageRule)
}

impl ToCss for CssRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    match self {
      CssRule::Media(media) => media.to_css(dest),
      CssRule::Import(import) => import.to_css(dest),
      CssRule::Style(style) => style.to_css(dest),
      CssRule::Keyframes(keyframes) => keyframes.to_css(dest),
      CssRule::FontFace(font_face) => font_face.to_css(dest),
      CssRule::Page(font_face) => font_face.to_css(dest),
    }
  }
}

impl<'a, 'b> NestedRuleParser {
  fn parse_nested_rules(
      &mut self,
      input: &mut Parser,
      // rule_type: CssRuleType,
  ) -> Vec<CssRule> {
      // let context = ParserContext::new_with_rule_type(self.context, rule_type, self.namespaces);

      let nested_parser = NestedRuleParser {};

      let mut iter = RuleListParser::new_for_nested_rule(input, nested_parser);
      let mut rules = Vec::new();
      while let Some(result) = iter.next() {
          match result {
              Ok(rule) => rules.push(rule),
              Err((error, slice)) => {
                  let location = error.location;
                  // let error = ContextualParseError::InvalidRule(slice, error);
                  // self.context.log_css_error(location, error);
              },
          }
      }
      // CssRules::new(rules, self.shared_lock)
      rules
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
              // let media = exhaust(input);
              let media = MediaList::parse(input);
              // let media_queries = MediaList::parse(self.context, input);
              // let arc = Arc::new(self.shared_lock.wrap(media_queries));
              // Ok(AtRuleType::WithBlock(AtRulePrelude::Media(arc)))
              Ok(AtRuleType::WithBlock(AtRulePrelude::Media(media)))
          },
          // "supports" => {
          //     let cond = SupportsCondition::parse(input)?;
          //     Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::Supports(cond)))
          // },
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
          // "counter-style" => {
          //     if !cfg!(feature = "gecko") {
          //         // Support for this rule is not fully implemented in Servo yet.
          //         return Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
          //     }
          //     let name = parse_counter_style_name_definition(input)?;
          //     Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::CounterStyle(name)))
          // },
          // "viewport" => {
          //     if viewport_rule::enabled() {
          //         Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::Viewport))
          //     } else {
          //         Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
          //     }
          // },
          "keyframes" | "-webkit-keyframes" | "-moz-keyframes" => {
              // let prefix = if starts_with_ignore_ascii_case(&*name, "-webkit-") {
              //     Some(VendorPrefix::WebKit)
              // } else if starts_with_ignore_ascii_case(&*name, "-moz-") {
              //     Some(VendorPrefix::Moz)
              // } else {
              //     None
              // };
              // let name = KeyframesName::parse(self.context, input)?;
              let location = input.current_source_location();
              let name = match *input.next()? {
                Token::Ident(ref s) => s.as_ref(),
                Token::QuotedString(ref s) => s.as_ref(),
                ref t => return Err(location.new_unexpected_token_error(t.clone())),
              };

              Ok(AtRuleType::WithBlock(AtRulePrelude::Keyframes(name.into())))
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
      match prelude {
          AtRulePrelude::FontFace => {
            let parser = DeclarationListParser::new(input, FontFaceDeclarationParser);
            let properties: Vec<_> = parser.flatten().collect();
            Ok(CssRule::FontFace(FontFaceRule {
              properties
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
          // AtRuleBlockPrelude::CounterStyle(name) => {
          //     let context = ParserContext::new_with_rule_type(
          //         self.context,
          //         CssRuleType::CounterStyle,
          //         self.namespaces,
          //     );

          //     Ok(CssRule::CounterStyle(Arc::new(
          //         self.shared_lock.wrap(
          //             parse_counter_style_body(name, &context, input, start.source_location())?
          //                 .into(),
          //         ),
          //     )))
          // },
          AtRulePrelude::Media(query) => {
              // Ok(CssRule::Media(Arc::new(self.shared_lock.wrap(MediaRule {
              //     media_queries,
              //     rules: self.parse_nested_rules(input, CssRuleType::Media),
              //     source_location: start.source_location(),
              // }))))
              Ok(CssRule::Media(MediaRule {
                query,
                rules: self.parse_nested_rules(input)
              }))
          },
          // AtRuleBlockPrelude::Supports(condition) => {
          //     let eval_context = ParserContext::new_with_rule_type(
          //         self.context,
          //         CssRuleType::Style,
          //         self.namespaces,
          //     );

          //     let enabled = condition.eval(&eval_context, self.namespaces);
          //     Ok(CssRule::Supports(Arc::new(self.shared_lock.wrap(
          //         SupportsRule {
          //             condition,
          //             rules: self.parse_nested_rules(input, CssRuleType::Supports),
          //             enabled,
          //             source_location: start.source_location(),
          //         },
          //     ))))
          // },
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
          AtRulePrelude::Keyframes(name) => {
              // let context = ParserContext::new_with_rule_type(
              //     self.context,
              //     CssRuleType::Keyframes,
              //     self.namespaces,
              // );

              let iter = RuleListParser::new_for_nested_rule(input, KeyframeListParser);

              Ok(CssRule::Keyframes(KeyframesRule {
                name,
                keyframes: iter.filter_map(Result::ok).collect()
                // keyframes: parse_keyframe_list(&context, input, self.shared_lock),
                // vendor_prefix,
                // source_location: start.source_location(),
              }))
          },
          AtRulePrelude::Page(selectors) => {
            let parser = DeclarationListParser::new(input, PropertyDeclarationParser);
            let declarations: Vec<_> = parser.flatten().collect();
            Ok(CssRule::Page(PageRule {
              selectors,
              declarations: DeclarationBlock {
                declarations
              }
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
      let selector_parser = SelectorParser {
      //     stylesheet_origin: self.context.stylesheet_origin,
      //     namespaces: self.namespaces,
      //     url_data: Some(self.context.url_data),
      };
      match SelectorList::parse(&selector_parser, input) {
        Ok(x) => Ok(x),
        Err(_) => Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
      }
      // Ok(exhaust(input))
  }

  fn parse_block<'t>(
      &mut self,
      selectors: Self::Prelude,
      start: &ParserState,
      input: &mut Parser<'i, 't>,
  ) -> Result<CssRule, ParseError<'i, Self::Error>> {
      // let declarations = parse_property_declaration_list(&context, input, Some(&selectors));
      // let block = Arc::new(self.shared_lock.wrap(declarations));
      // Ok(CssRule::Style(Arc::new(self.shared_lock.wrap(StyleRule {
      //     selectors,
      //     block,
      //     source_location: start.source_location(),
      // }))))
      let parser = DeclarationListParser::new(input, PropertyDeclarationParser);
      let declarations: Vec<_> = parser.flatten().collect();
      // Ok((prelude, declarations))
      Ok(CssRule::Style(StyleRule {
        selectors,
        declarations: DeclarationBlock {
          declarations
        }
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
