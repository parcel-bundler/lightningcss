use crate::declaration::{parse_declaration, DeclarationBlock, DeclarationList};
use crate::error::{Error, ParserError};
use crate::media_query::*;
use crate::properties::custom::TokenList;
use crate::rules::container::{ContainerName, ContainerRule};
use crate::rules::font_palette_values::FontPaletteValuesRule;
use crate::rules::layer::{LayerBlockRule, LayerStatementRule};
use crate::rules::property::PropertyRule;
use crate::rules::viewport::ViewportRule;
use crate::rules::{
  counter_style::CounterStyleRule,
  custom_media::CustomMediaRule,
  document::MozDocumentRule,
  font_face::{FontFaceDeclarationParser, FontFaceRule},
  import::ImportRule,
  keyframes::{KeyframeListParser, KeyframesRule},
  layer::LayerName,
  media::MediaRule,
  namespace::NamespaceRule,
  nesting::NestingRule,
  page::{PageRule, PageSelector},
  style::StyleRule,
  supports::{SupportsCondition, SupportsRule},
  unknown::UnknownAtRule,
  CssRule, CssRuleList, Location,
};
use crate::selector::{SelectorParser, Selectors};
use crate::traits::Parse;
use crate::values::ident::{CustomIdent, DashedIdent};
use crate::values::string::CowArcStr;
use crate::vendor_prefix::VendorPrefix;
use cssparser::*;
use parcel_selectors::{parser::NestingRequirement, SelectorList};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// CSS parsing options.
#[derive(Default, Clone, Debug)]
pub struct ParserOptions<'o, 'i> {
  /// Filename to use in error messages.
  pub filename: String,
  /// Whether the enable the [CSS nesting](https://www.w3.org/TR/css-nesting-1/) draft syntax.
  pub nesting: bool,
  /// Whether to enable the [custom media](https://drafts.csswg.org/mediaqueries-5/#custom-mq) draft syntax.
  pub custom_media: bool,
  /// Whether the enable [CSS modules](https://github.com/css-modules/css-modules).
  pub css_modules: Option<crate::css_modules::Config<'o>>,
  /// The source index to assign to all parsed rules. Impacts the source map when
  /// the style sheet is serialized.
  pub source_index: u32,
  /// Whether to ignore invalid rules and declarations rather than erroring.
  pub error_recovery: bool,
  /// A list that will be appended to when a warning occurs.
  pub warnings: Option<Arc<RwLock<Vec<Error<ParserError<'i>>>>>>,
}

impl<'o, 'i> ParserOptions<'o, 'i> {
  #[inline]
  pub(crate) fn warn(&self, warning: ParseError<'i, ParserError<'i>>) {
    if let Some(warnings) = &self.warnings {
      if let Ok(mut warnings) = warnings.write() {
        warnings.push(Error::from(warning, self.filename.clone()));
      }
    }
  }
}

#[derive(PartialEq, PartialOrd)]
enum State {
  Start = 1,
  Layers = 2,
  Imports = 3,
  Namespaces = 4,
  Body = 5,
}

/// The parser for the top-level rules in a stylesheet.
pub struct TopLevelRuleParser<'a, 'o, 'i> {
  default_namespace: Option<CowArcStr<'i>>,
  namespace_prefixes: HashMap<CowArcStr<'i>, CowArcStr<'i>>,
  options: &'a ParserOptions<'o, 'i>,
  state: State,
}

impl<'a, 'o, 'b, 'i> TopLevelRuleParser<'a, 'o, 'i> {
  pub fn new(options: &'a ParserOptions<'o, 'i>) -> Self {
    TopLevelRuleParser {
      default_namespace: None,
      namespace_prefixes: HashMap::new(),
      options,
      state: State::Start,
    }
  }

  fn nested<'x: 'b>(&'x mut self) -> NestedRuleParser<'_, 'o, 'i> {
    NestedRuleParser {
      default_namespace: &mut self.default_namespace,
      namespace_prefixes: &mut self.namespace_prefixes,
      options: &self.options,
    }
  }
}

/// A rule prelude for at-rule with block.
#[derive(Debug)]
#[allow(dead_code)]
pub enum AtRulePrelude<'i> {
  /// A @font-face rule prelude.
  FontFace,
  /// A @font-feature-values rule prelude, with its FamilyName list.
  FontFeatureValues, //(Vec<FamilyName>),
  /// A @font-palette-values rule prelude, with its name.
  FontPaletteValues(DashedIdent<'i>),
  /// A @counter-style rule prelude, with its counter style name.
  CounterStyle(CustomIdent<'i>),
  /// A @media rule prelude, with its media queries.
  Media(MediaList<'i>),
  /// A @custom-media rule prelude.
  CustomMedia(DashedIdent<'i>, MediaList<'i>),
  /// An @supports rule, with its conditional
  Supports(SupportsCondition<'i>),
  /// A @viewport rule prelude.
  Viewport(VendorPrefix),
  /// A @keyframes rule, with its animation name and vendor prefix if exists.
  Keyframes(CustomIdent<'i>, VendorPrefix),
  /// A @page rule prelude.
  Page(Vec<PageSelector<'i>>),
  /// A @-moz-document rule.
  MozDocument,
  /// A @import rule prelude.
  Import(
    CowRcStr<'i>,
    MediaList<'i>,
    Option<SupportsCondition<'i>>,
    Option<Option<LayerName<'i>>>,
  ),
  /// A @namespace rule prelude.
  Namespace(Option<CowRcStr<'i>>, CowRcStr<'i>),
  /// A @charset rule prelude.
  Charset,
  /// A @nest prelude.
  Nest(SelectorList<'i, Selectors>),
  /// An @layer prelude.
  Layer(Vec<LayerName<'i>>),
  /// An @property prelude.
  Property(DashedIdent<'i>),
  /// A @container prelude.
  Container(Option<ContainerName<'i>>, MediaCondition<'i>),
  /// An unknown prelude.
  Unknown(CowArcStr<'i>, TokenList<'i>),
}

impl<'a, 'o, 'i> AtRuleParser<'i> for TopLevelRuleParser<'a, 'o, 'i> {
  type Prelude = AtRulePrelude<'i>;
  type AtRule = (SourcePosition, CssRule<'i>);
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &*name,
      "import" => {
        if self.state > State::Imports {
          return Err(input.new_custom_error(ParserError::UnexpectedImportRule))
        }

        let url_string = input.expect_url_or_string()?.clone();

        let layer = if input.try_parse(|input| input.expect_ident_matching("layer")).is_ok() {
          Some(None)
        } else if input.try_parse(|input| input.expect_function_matching("layer")).is_ok() {
          let name = input.parse_nested_block(LayerName::parse).map(|name| Some(name))?;
          Some(name)
        } else {
          None
        };

        let supports = if input.try_parse(|input| input.expect_function_matching("supports")).is_ok() {
          Some(input.parse_nested_block(|input| {
            input.try_parse(SupportsCondition::parse).or_else(|_| SupportsCondition::parse_declaration(input))
          })?)
        } else {
          None
        };
        let media = MediaList::parse(input)?;
        return Ok(AtRulePrelude::Import(url_string, media, supports, layer));
      },
      "namespace" => {
        if self.state > State::Namespaces {
          return Err(input.new_custom_error(ParserError::UnexpectedNamespaceRule))
        }

        let prefix = input.try_parse(|input| input.expect_ident_cloned()).ok();
        let namespace = input.expect_url_or_string()?;
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
      "custom-media" if self.options.custom_media => {
        let name = DashedIdent::parse(input)?;
        let media = MediaList::parse(input)?;
        return Ok(AtRulePrelude::CustomMedia(name, media))
      },
      "property" => {
        let name = DashedIdent::parse(input)?;
        return Ok(AtRulePrelude::Property(name))
      },
      _ => {}
    }

    AtRuleParser::parse_prelude(&mut self.nested(), name, input)
  }

  #[inline]
  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
    self.state = State::Body;
    let rule = AtRuleParser::parse_block(&mut self.nested(), prelude, start, input)?;
    Ok((start.position(), rule))
  }

  #[inline]
  fn rule_without_block(&mut self, prelude: AtRulePrelude<'i>, start: &ParserState) -> Result<Self::AtRule, ()> {
    let loc = start.source_location();
    let loc = Location {
      source_index: self.options.source_index,
      line: loc.line,
      column: loc.column,
    };

    let rule = match prelude {
      AtRulePrelude::Import(url, media, supports, layer) => {
        self.state = State::Imports;
        CssRule::Import(ImportRule {
          url: url.into(),
          layer,
          supports,
          media,
          loc,
        })
      }
      AtRulePrelude::Namespace(prefix, url) => {
        self.state = State::Namespaces;

        if let Some(prefix) = &prefix {
          self.namespace_prefixes.insert(prefix.into(), url.clone().into());
        } else {
          self.default_namespace = Some(url.clone().into());
        }

        CssRule::Namespace(NamespaceRule {
          prefix: prefix.map(|x| x.into()),
          url: url.into(),
          loc,
        })
      }
      AtRulePrelude::CustomMedia(name, query) => {
        self.state = State::Body;
        CssRule::CustomMedia(CustomMediaRule { name, query, loc })
      }
      AtRulePrelude::Layer(_) => {
        // @layer statements are allowed before @import rules, but cannot be interleaved.
        if self.state <= State::Layers {
          self.state = State::Layers;
        } else {
          self.state = State::Body;
        }
        AtRuleParser::rule_without_block(&mut self.nested(), prelude, start)?
      }
      AtRulePrelude::Charset => CssRule::Ignored,
      AtRulePrelude::Unknown(name, prelude) => CssRule::Unknown(UnknownAtRule {
        name,
        prelude,
        block: None,
        loc,
      }),
      _ => return Err(()),
    };

    Ok((start.position(), rule))
  }
}

impl<'a, 'o, 'i> QualifiedRuleParser<'i> for TopLevelRuleParser<'a, 'o, 'i> {
  type Prelude = SelectorList<'i, Selectors>;
  type QualifiedRule = (SourcePosition, CssRule<'i>);
  type Error = ParserError<'i>;

  #[inline]
  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    self.state = State::Body;
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
struct NestedRuleParser<'a, 'o, 'i> {
  default_namespace: &'a Option<CowArcStr<'i>>,
  namespace_prefixes: &'a HashMap<CowArcStr<'i>, CowArcStr<'i>>,
  options: &'a ParserOptions<'o, 'i>,
}

impl<'a, 'o, 'b, 'i> NestedRuleParser<'a, 'o, 'i> {
  fn parse_nested_rules<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<CssRuleList<'i>, ParseError<'i, ParserError<'i>>> {
    let nested_parser = NestedRuleParser {
      default_namespace: self.default_namespace,
      namespace_prefixes: self.namespace_prefixes,
      options: self.options,
    };

    let mut iter = RuleListParser::new_for_nested_rule(input, nested_parser);
    let mut rules = Vec::new();
    while let Some(result) = iter.next() {
      match result {
        Ok(CssRule::Ignored) => {}
        Ok(rule) => rules.push(rule),
        Err((e, _)) => {
          if self.options.error_recovery {
            self.options.warn(e);
            continue;
          }
          return Err(e);
        }
      }
    }

    Ok(CssRuleList(rules))
  }

  fn loc(&self, start: &ParserState) -> Location {
    let loc = start.source_location();
    Location {
      source_index: self.options.source_index,
      line: loc.line,
      column: loc.column,
    }
  }
}

impl<'a, 'o, 'b, 'i> AtRuleParser<'i> for NestedRuleParser<'a, 'o, 'i> {
  type Prelude = AtRulePrelude<'i>;
  type AtRule = CssRule<'i>;
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &*name,
      "media" => {
        let media = MediaList::parse(input)?;
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
      "font-palette-values" => {
        let name = DashedIdent::parse(input)?;
        return Ok(AtRulePrelude::FontPaletteValues(name))
      },
      "counter-style" => {
        let name = CustomIdent::parse(input)?;
        Ok(AtRulePrelude::CounterStyle(name))
      },
      "viewport" | "-ms-viewport" => {
        let prefix = if starts_with_ignore_ascii_case(&*name, "-ms") {
          VendorPrefix::Ms
        } else {
          VendorPrefix::None
        };
        Ok(AtRulePrelude::Viewport(prefix))
      },
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
          Token::Ident(ref s) => s.into(),
          Token::QuotedString(ref s) => s.into(),
          ref t => return Err(location.new_unexpected_token_error(t.clone())),
        };

        Ok(AtRulePrelude::Keyframes(CustomIdent(name), prefix))
      },
      "page" => {
        let selectors = input.try_parse(|input| input.parse_comma_separated(PageSelector::parse)).unwrap_or_default();
        Ok(AtRulePrelude::Page(selectors))
      },
      "-moz-document" => {
        // Firefox only supports the url-prefix() function with no arguments as a legacy CSS hack.
        // See https://css-tricks.com/snippets/css/css-hacks-targeting-firefox/
        input.expect_function_matching("url-prefix")?;
        input.parse_nested_block(|input| {
          // Firefox also allows an empty string as an argument...
          // https://github.com/mozilla/gecko-dev/blob/0077f2248712a1b45bf02f0f866449f663538164/servo/components/style/stylesheets/document_rule.rs#L303
          let _ = input.try_parse(|input| -> Result<(), ParseError<'i, Self::Error>> {
            let s = input.expect_string()?;
            if !s.is_empty() {
              return Err(input.new_custom_error(ParserError::InvalidValue))
            }
            Ok(())
          });
          input.expect_exhausted()?;
          Ok(())
        })?;

        Ok(AtRulePrelude::MozDocument)
      },
      "layer" => {
        let names = match Vec::<LayerName>::parse(input) {
          Ok(names) => names,
          Err(ParseError { kind: ParseErrorKind::Basic(BasicParseErrorKind::EndOfInput), .. }) => Vec::new(),
          Err(e) => return Err(e)
        };
        Ok(AtRulePrelude::Layer(names))
      },
      "container" => {
        let name = input.try_parse(ContainerName::parse).ok();
        let condition = MediaCondition::parse(input, true)?;
        Ok(AtRulePrelude::Container(name, condition))
      },
      _ => {
        self.options.warn(input.new_error(BasicParseErrorKind::AtRuleInvalid(name.clone())));
        input.skip_whitespace();
        let tokens = TokenList::parse(input, &self.options, 0)?;
        Ok(AtRulePrelude::Unknown(name.into(), tokens))
      }
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<CssRule<'i>, ParseError<'i, Self::Error>> {
    let loc = self.loc(start);
    match prelude {
      AtRulePrelude::FontFace => {
        let mut parser = DeclarationListParser::new(input, FontFaceDeclarationParser);
        let mut properties = vec![];
        while let Some(decl) = parser.next() {
          if let Ok(decl) = decl {
            properties.push(decl);
          }
        }
        Ok(CssRule::FontFace(FontFaceRule { properties, loc }))
      }
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
      AtRulePrelude::FontPaletteValues(name) => {
        let rule = FontPaletteValuesRule::parse(name, input, loc)?;
        Ok(CssRule::FontPaletteValues(rule))
      }
      AtRulePrelude::CounterStyle(name) => Ok(CssRule::CounterStyle(CounterStyleRule {
        name,
        declarations: DeclarationBlock::parse(input, self.options)?,
        loc,
      })),
      AtRulePrelude::Media(query) => Ok(CssRule::Media(MediaRule {
        query,
        rules: self.parse_nested_rules(input)?,
        loc,
      })),
      AtRulePrelude::Supports(condition) => Ok(CssRule::Supports(SupportsRule {
        condition,
        rules: self.parse_nested_rules(input)?,
        loc,
      })),
      AtRulePrelude::Container(name, condition) => Ok(CssRule::Container(ContainerRule {
        name,
        condition,
        rules: self.parse_nested_rules(input)?,
        loc,
      })),
      AtRulePrelude::Viewport(vendor_prefix) => {
        Ok(CssRule::Viewport(ViewportRule {
          vendor_prefix,
          // TODO: parse viewport descriptors rather than properties
          // https://drafts.csswg.org/css-device-adapt/#viewport-desc
          declarations: DeclarationBlock::parse(input, self.options)?,
          loc,
        }))
      }
      AtRulePrelude::Keyframes(name, vendor_prefix) => {
        let iter = RuleListParser::new_for_nested_rule(input, KeyframeListParser);
        Ok(CssRule::Keyframes(KeyframesRule {
          name,
          keyframes: iter.filter_map(Result::ok).collect(),
          vendor_prefix,
          loc,
        }))
      }
      AtRulePrelude::Page(selectors) => Ok(CssRule::Page(PageRule {
        selectors,
        declarations: DeclarationBlock::parse(input, self.options)?,
        loc,
      })),
      AtRulePrelude::MozDocument => Ok(CssRule::MozDocument(MozDocumentRule {
        rules: self.parse_nested_rules(input)?,
        loc,
      })),
      AtRulePrelude::Layer(names) => {
        let name = if names.is_empty() {
          None
        } else if names.len() == 1 {
          names.into_iter().next()
        } else {
          return Err(input.new_error(BasicParseErrorKind::AtRuleBodyInvalid));
        };

        Ok(CssRule::LayerBlock(LayerBlockRule {
          name,
          rules: self.parse_nested_rules(input)?,
          loc,
        }))
      }
      AtRulePrelude::Property(name) => Ok(CssRule::Property(PropertyRule::parse(name, input, loc)?)),
      AtRulePrelude::Import(..)
      | AtRulePrelude::Namespace(..)
      | AtRulePrelude::CustomMedia(..)
      | AtRulePrelude::Charset => {
        // These rules don't have blocks.
        Err(input.new_unexpected_token_error(Token::CurlyBracketBlock))
      }
      AtRulePrelude::FontFeatureValues | AtRulePrelude::Nest(..) => unreachable!(),
      AtRulePrelude::Unknown(name, prelude) => Ok(CssRule::Unknown(UnknownAtRule {
        name,
        prelude,
        block: Some(TokenList::parse(input, &self.options, 0)?),
        loc,
      })),
    }
  }

  #[inline]
  fn rule_without_block(&mut self, prelude: AtRulePrelude<'i>, start: &ParserState) -> Result<Self::AtRule, ()> {
    let loc = self.loc(start);
    match prelude {
      AtRulePrelude::Layer(names) => {
        if names.is_empty() {
          return Err(());
        }

        Ok(CssRule::LayerStatement(LayerStatementRule { names, loc }))
      }
      AtRulePrelude::Unknown(name, prelude) => Ok(CssRule::Unknown(UnknownAtRule {
        name,
        prelude,
        block: None,
        loc,
      })),
      _ => Err(()),
    }
  }
}

impl<'a, 'o, 'b, 'i> QualifiedRuleParser<'i> for NestedRuleParser<'a, 'o, 'i> {
  type Prelude = SelectorList<'i, Selectors>;
  type QualifiedRule = CssRule<'i>;
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let selector_parser = SelectorParser {
      default_namespace: self.default_namespace,
      namespace_prefixes: self.namespace_prefixes,
      is_nesting_allowed: false,
      options: &self.options,
    };
    SelectorList::parse(&selector_parser, input, NestingRequirement::None)
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<CssRule<'i>, ParseError<'i, Self::Error>> {
    let loc = self.loc(start);
    let (declarations, rules) = if self.options.nesting {
      parse_declarations_and_nested_rules(input, self.default_namespace, self.namespace_prefixes, self.options)?
    } else {
      (DeclarationBlock::parse(input, self.options)?, CssRuleList(vec![]))
    };
    Ok(CssRule::Style(StyleRule {
      selectors,
      vendor_prefix: VendorPrefix::empty(),
      declarations,
      rules,
      loc,
    }))
  }
}

fn parse_declarations_and_nested_rules<'a, 'o, 'i, 't>(
  input: &mut Parser<'i, 't>,
  default_namespace: &'a Option<CowArcStr<'i>>,
  namespace_prefixes: &'a HashMap<CowArcStr<'i>, CowArcStr<'i>>,
  options: &'a ParserOptions<'o, 'i>,
) -> Result<(DeclarationBlock<'i>, CssRuleList<'i>), ParseError<'i, ParserError<'i>>> {
  let mut important_declarations = DeclarationList::new();
  let mut declarations = DeclarationList::new();
  let mut rules = CssRuleList(vec![]);
  let parser = StyleRuleParser {
    default_namespace,
    namespace_prefixes,
    options,
    declarations: &mut declarations,
    important_declarations: &mut important_declarations,
    rules: &mut rules,
  };

  let mut declaration_parser = DeclarationListParser::new(input, parser);
  let mut last = declaration_parser.input.state();
  while let Some(decl) = declaration_parser.next() {
    match decl {
      Ok(_) => {}
      _ => {
        declaration_parser.input.reset(&last);
        break;
      }
    }

    last = declaration_parser.input.state();
  }

  let mut iter = RuleListParser::new_for_nested_rule(declaration_parser.input, declaration_parser.parser);
  while let Some(result) = iter.next() {
    if let Err((err, _)) = result {
      if options.error_recovery {
        options.warn(err);
        continue;
      }
      return Err(err);
    }
  }

  Ok((
    DeclarationBlock {
      declarations,
      important_declarations,
    },
    rules,
  ))
}

pub struct StyleRuleParser<'a, 'o, 'i> {
  default_namespace: &'a Option<CowArcStr<'i>>,
  namespace_prefixes: &'a HashMap<CowArcStr<'i>, CowArcStr<'i>>,
  options: &'a ParserOptions<'o, 'i>,
  declarations: &'a mut DeclarationList<'i>,
  important_declarations: &'a mut DeclarationList<'i>,
  rules: &'a mut CssRuleList<'i>,
}

/// Parse a declaration within {} block: `color: blue`
impl<'a, 'o, 'i> cssparser::DeclarationParser<'i> for StyleRuleParser<'a, 'o, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    if !self.rules.0.is_empty() {
      // Declarations cannot come after nested rules.
      return Err(input.new_custom_error(ParserError::InvalidNesting));
    }
    parse_declaration(
      name,
      input,
      &mut self.declarations,
      &mut self.important_declarations,
      &self.options,
    )
  }
}

impl<'a, 'o, 'i> AtRuleParser<'i> for StyleRuleParser<'a, 'o, 'i> {
  type Prelude = AtRulePrelude<'i>;
  type AtRule = ();
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &*name,
      "media" => {
        let media = MediaList::parse(input)?;
        Ok(AtRulePrelude::Media(media))
      },
      "supports" => {
        let cond = SupportsCondition::parse(input)?;
        Ok(AtRulePrelude::Supports(cond))
      },
      "nest" => {
        let selector_parser = SelectorParser {
          default_namespace: self.default_namespace,
          namespace_prefixes: self.namespace_prefixes,
          is_nesting_allowed: true,
          options: &self.options,
        };
        let selectors = SelectorList::parse(&selector_parser, input, NestingRequirement::Contained)?;
        Ok(AtRulePrelude::Nest(selectors))
      },
      _ => Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: AtRulePrelude<'i>,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<(), ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    let loc = Location {
      source_index: self.options.source_index,
      line: loc.line,
      column: loc.column,
    };
    match prelude {
      AtRulePrelude::Media(query) => {
        self.rules.0.push(CssRule::Media(MediaRule {
          query,
          rules: parse_nested_at_rule(
            input,
            self.options.source_index,
            self.default_namespace,
            self.namespace_prefixes,
            self.options,
          )?,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Supports(condition) => {
        self.rules.0.push(CssRule::Supports(SupportsRule {
          condition,
          rules: parse_nested_at_rule(
            input,
            self.options.source_index,
            self.default_namespace,
            self.namespace_prefixes,
            self.options,
          )?,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Nest(selectors) => {
        let (declarations, rules) = parse_declarations_and_nested_rules(
          input,
          self.default_namespace,
          self.namespace_prefixes,
          self.options,
        )?;
        self.rules.0.push(CssRule::Nesting(NestingRule {
          style: StyleRule {
            selectors,
            declarations,
            vendor_prefix: VendorPrefix::empty(),
            rules,
            loc,
          },
          loc,
        }));
        Ok(())
      }
      _ => {
        unreachable!()
      }
    }
  }
}

#[inline]
fn parse_nested_at_rule<'a, 'o, 'i, 't>(
  input: &mut Parser<'i, 't>,
  source_index: u32,
  default_namespace: &'a Option<CowArcStr<'i>>,
  namespace_prefixes: &'a HashMap<CowArcStr<'i>, CowArcStr<'i>>,
  options: &'a ParserOptions<'o, 'i>,
) -> Result<CssRuleList<'i>, ParseError<'i, ParserError<'i>>> {
  let loc = input.current_source_location();
  let loc = Location {
    source_index,
    line: loc.line,
    column: loc.column,
  };

  // Declarations can be immediately within @media and @supports blocks that are nested within a parent style rule.
  // These act the same way as if they were nested within a `& { ... }` block.
  let (declarations, mut rules) =
    parse_declarations_and_nested_rules(input, default_namespace, namespace_prefixes, options)?;

  if declarations.declarations.len() > 0 {
    rules.0.insert(
      0,
      CssRule::Style(StyleRule {
        selectors: SelectorList(smallvec::smallvec![parcel_selectors::parser::Selector::from_vec2(
          vec![parcel_selectors::parser::Component::Nesting]
        )]),
        declarations,
        vendor_prefix: VendorPrefix::empty(),
        rules: CssRuleList(vec![]),
        loc,
      }),
    )
  }

  Ok(rules)
}

impl<'a, 'o, 'b, 'i> QualifiedRuleParser<'i> for StyleRuleParser<'a, 'o, 'i> {
  type Prelude = SelectorList<'i, Selectors>;
  type QualifiedRule = ();
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let selector_parser = SelectorParser {
      default_namespace: self.default_namespace,
      namespace_prefixes: self.namespace_prefixes,
      is_nesting_allowed: true,
      options: &self.options,
    };
    SelectorList::parse(&selector_parser, input, NestingRequirement::Prefixed)
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<(), ParseError<'i, Self::Error>> {
    let loc = start.source_location();
    let (declarations, rules) =
      parse_declarations_and_nested_rules(input, self.default_namespace, self.namespace_prefixes, self.options)?;
    self.rules.0.push(CssRule::Style(StyleRule {
      selectors,
      vendor_prefix: VendorPrefix::empty(),
      declarations,
      rules,
      loc: Location {
        source_index: self.options.source_index,
        line: loc.line,
        column: loc.column,
      },
    }));
    Ok(())
  }
}

#[inline]
pub fn starts_with_ignore_ascii_case(string: &str, prefix: &str) -> bool {
  string.len() >= prefix.len() && string.as_bytes()[0..prefix.len()].eq_ignore_ascii_case(prefix.as_bytes())
}
