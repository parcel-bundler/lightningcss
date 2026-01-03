use crate::declaration::{parse_declaration, DeclarationBlock, DeclarationList};
use crate::error::{Error, ParserError, PrinterError};
use crate::media_query::*;
use crate::printer::Printer;
use crate::properties::custom::TokenList;
use crate::rules::container::{ContainerCondition, ContainerName, ContainerRule};
use crate::rules::font_feature_values::FontFeatureValuesRule;
use crate::rules::font_palette_values::FontPaletteValuesRule;
use crate::rules::layer::{LayerBlockRule, LayerStatementRule};
use crate::rules::nesting::NestedDeclarationsRule;
use crate::rules::property::PropertyRule;
use crate::rules::scope::ScopeRule;
use crate::rules::starting_style::StartingStyleRule;
use crate::rules::view_transition::ViewTransitionRule;
use crate::rules::viewport::ViewportRule;

use crate::properties::font::FamilyName;
use crate::rules::{
  counter_style::CounterStyleRule,
  custom_media::CustomMediaRule,
  document::MozDocumentRule,
  font_face::{FontFaceDeclarationParser, FontFaceRule},
  import::ImportRule,
  keyframes::{KeyframeListParser, KeyframesName, KeyframesRule},
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
use crate::selector::{SelectorList, SelectorParser};
use crate::traits::{Parse, ParseWithOptions};
use crate::values::ident::{CustomIdent, DashedIdent};
use crate::values::string::CowArcStr;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::{Visit, VisitTypes, Visitor};
use bitflags::bitflags;
use cssparser::*;
use parcel_selectors::parser::{NestingRequirement, ParseErrorRecovery};
use std::sync::{Arc, RwLock};

bitflags! {
  /// Parser feature flags to enable.
  #[derive(Clone, Debug, Default)]
  pub struct ParserFlags: u8 {
    /// Whether the enable the [CSS nesting](https://www.w3.org/TR/css-nesting-1/) draft syntax.
    const NESTING = 1 << 0;
    /// Whether to enable the [custom media](https://drafts.csswg.org/mediaqueries-5/#custom-mq) draft syntax.
    const CUSTOM_MEDIA = 1 << 1;
    /// Whether to enable the non-standard >>> and /deep/ selector combinators used by Vue and Angular.
    const DEEP_SELECTOR_COMBINATOR = 1 << 2;
  }
}

/// CSS parsing options.
#[derive(Clone, Debug, Default)]
pub struct ParserOptions<'o, 'i> {
  /// Filename to use in error messages.
  pub filename: String,
  /// Whether the enable [CSS modules](https://github.com/css-modules/css-modules).
  pub css_modules: Option<crate::css_modules::Config<'o>>,
  /// The source index to assign to all parsed rules. Impacts the source map when
  /// the style sheet is serialized.
  pub source_index: u32,
  /// Whether to ignore invalid rules and declarations rather than erroring.
  pub error_recovery: bool,
  /// A list that will be appended to when a warning occurs.
  pub warnings: Option<Arc<RwLock<Vec<Error<ParserError<'i>>>>>>,
  /// Feature flags to enable.
  pub flags: ParserFlags,
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

#[derive(Clone, Default)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct DefaultAtRuleParser;
impl<'i> crate::traits::AtRuleParser<'i> for DefaultAtRuleParser {
  type AtRule = DefaultAtRule;
  type Error = ();
  type Prelude = ();
}

#[derive(PartialEq, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct DefaultAtRule;
impl crate::traits::ToCss for DefaultAtRule {
  fn to_css<W: std::fmt::Write>(&self, _: &mut Printer<W>) -> Result<(), PrinterError> {
    Err(PrinterError {
      kind: crate::error::PrinterErrorKind::FmtError,
      loc: None,
    })
  }
}

#[cfg(feature = "into_owned")]
impl<'any> static_self::IntoOwned<'any> for DefaultAtRule {
  type Owned = Self;
  fn into_owned(self) -> Self {
    self
  }
}

#[cfg(feature = "visitor")]
#[cfg_attr(docsrs, doc(cfg(feature = "visitor")))]
impl<'i, V: Visitor<'i, DefaultAtRule>> Visit<'i, DefaultAtRule, V> for DefaultAtRule {
  const CHILD_TYPES: VisitTypes = VisitTypes::empty();
  fn visit_children(&mut self, _: &mut V) -> Result<(), V::Error> {
    Ok(())
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
pub struct TopLevelRuleParser<'a, 'o, 'i, T: crate::traits::AtRuleParser<'i>> {
  pub options: &'a ParserOptions<'o, 'i>,
  state: State,
  at_rule_parser: &'a mut T,
  rules: &'a mut CssRuleList<'i, T::AtRule>,
}

impl<'a, 'o, 'b, 'i, T: crate::traits::AtRuleParser<'i>> TopLevelRuleParser<'a, 'o, 'i, T> {
  pub fn new(
    options: &'a ParserOptions<'o, 'i>,
    at_rule_parser: &'a mut T,
    rules: &'a mut CssRuleList<'i, T::AtRule>,
  ) -> Self {
    TopLevelRuleParser {
      options,
      state: State::Start,
      at_rule_parser,
      rules,
    }
  }

  pub fn nested<'x: 'b>(&'x mut self) -> NestedRuleParser<'x, 'o, 'i, T> {
    NestedRuleParser {
      options: &self.options,
      at_rule_parser: self.at_rule_parser,
      declarations: DeclarationList::new(),
      important_declarations: DeclarationList::new(),
      rules: &mut self.rules,
      is_in_style_rule: false,
      allow_declarations: false,
    }
  }
}

/// A rule prelude for at-rule with block.
#[derive(Debug)]
#[allow(dead_code)]
pub enum AtRulePrelude<'i, T> {
  /// A @font-face rule prelude.
  FontFace,
  /// A @font-feature-values rule prelude, with its FamilyName list.
  FontFeatureValues(Vec<FamilyName<'i>>),
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
  Keyframes(KeyframesName<'i>, VendorPrefix),
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
  Nest(SelectorList<'i>),
  /// An @layer prelude.
  Layer(Vec<LayerName<'i>>),
  /// An @property prelude.
  Property(DashedIdent<'i>),
  /// A @container prelude.
  Container(Option<ContainerName<'i>>, ContainerCondition<'i>),
  /// A @starting-style prelude.
  StartingStyle,
  /// A @scope rule prelude.
  Scope(Option<SelectorList<'i>>, Option<SelectorList<'i>>),
  /// A @view-transition rule prelude.
  ViewTransition,
  /// An unknown prelude.
  Unknown(CowArcStr<'i>, TokenList<'i>),
  /// A custom prelude.
  Custom(T),
}

impl<'i, T> AtRulePrelude<'i, T> {
  // https://drafts.csswg.org/css-nesting/#conditionals
  //     In addition to nested style rules, this specification allows nested group rules inside
  //     of style rules: any at-rule whose body contains style rules can be nested inside of a
  //     style rule as well.
  fn allowed_in_style_rule(&self) -> bool {
    match *self {
      Self::Media(..)
      | Self::Supports(..)
      | Self::Container(..)
      | Self::MozDocument
      | Self::Layer(..)
      | Self::StartingStyle
      | Self::Scope(..)
      | Self::Nest(..)
      | Self::Unknown(..)
      | Self::Custom(..) => true,

      Self::Namespace(..)
      | Self::FontFace
      | Self::FontFeatureValues(..)
      | Self::FontPaletteValues(..)
      | Self::CounterStyle(..)
      | Self::Keyframes(..)
      | Self::Page(..)
      | Self::Property(..)
      | Self::Import(..)
      | Self::CustomMedia(..)
      | Self::Viewport(..)
      | Self::Charset
      | Self::ViewTransition => false,
    }
  }
}

impl<'a, 'o, 'i, T: crate::traits::AtRuleParser<'i>> AtRuleParser<'i> for TopLevelRuleParser<'a, 'o, 'i, T> {
  type Prelude = AtRulePrelude<'i, T::Prelude>;
  type AtRule = ();
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
        let media = MediaList::parse(input, &self.options)?;
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
      "custom-media" if self.options.flags.contains(ParserFlags::CUSTOM_MEDIA) => {
        let name = DashedIdent::parse(input)?;
        let media = MediaList::parse(input, &self.options)?;
        return Ok(AtRulePrelude::CustomMedia(name, media))
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
    AtRuleParser::parse_block(&mut self.nested(), prelude, start, input)
  }

  #[inline]
  fn rule_without_block(
    &mut self,
    prelude: AtRulePrelude<'i, T::Prelude>,
    start: &ParserState,
  ) -> Result<Self::AtRule, ()> {
    let loc = start.source_location();
    let loc = Location {
      source_index: self.options.source_index,
      line: loc.line,
      column: loc.column,
    };

    match prelude {
      AtRulePrelude::Import(url, media, supports, layer) => {
        self.state = State::Imports;
        self.rules.0.push(CssRule::Import(ImportRule {
          url: url.into(),
          layer,
          supports,
          media,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Namespace(prefix, url) => {
        self.state = State::Namespaces;

        self.rules.0.push(CssRule::Namespace(NamespaceRule {
          prefix: prefix.map(|x| x.into()),
          url: url.into(),
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::CustomMedia(name, query) => {
        self.state = State::Body;
        self.rules.0.push(CssRule::CustomMedia(CustomMediaRule { name, query, loc }));
        Ok(())
      }
      AtRulePrelude::Layer(_) => {
        // @layer statements are allowed before @import rules, but cannot be interleaved.
        if self.state <= State::Layers {
          self.state = State::Layers;
        } else {
          self.state = State::Body;
        }
        AtRuleParser::rule_without_block(&mut self.nested(), prelude, start)
      }
      AtRulePrelude::Charset => Ok(()),
      AtRulePrelude::Unknown(name, prelude) => {
        self.rules.0.push(CssRule::Unknown(UnknownAtRule {
          name,
          prelude,
          block: None,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Custom(_) => {
        self.state = State::Body;
        AtRuleParser::rule_without_block(&mut self.nested(), prelude, start)
      }
      _ => Err(()),
    }
  }
}

impl<'a, 'o, 'i, T: crate::traits::AtRuleParser<'i>> QualifiedRuleParser<'i>
  for TopLevelRuleParser<'a, 'o, 'i, T>
{
  type Prelude = SelectorList<'i>;
  type QualifiedRule = ();
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
    QualifiedRuleParser::parse_block(&mut self.nested(), prelude, start, input)
  }
}

pub struct NestedRuleParser<'a, 'o, 'i, T: crate::traits::AtRuleParser<'i>> {
  pub options: &'a ParserOptions<'o, 'i>,
  pub at_rule_parser: &'a mut T,
  declarations: DeclarationList<'i>,
  important_declarations: DeclarationList<'i>,
  rules: &'a mut CssRuleList<'i, T::AtRule>,
  is_in_style_rule: bool,
  allow_declarations: bool,
}

impl<'a, 'o, 'b, 'i, T: crate::traits::AtRuleParser<'i>> NestedRuleParser<'a, 'o, 'i, T> {
  pub fn parse_nested<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
    is_style_rule: bool,
  ) -> Result<(DeclarationBlock<'i>, CssRuleList<'i, T::AtRule>), ParseError<'i, ParserError<'i>>> {
    let mut rules = CssRuleList(vec![]);
    let mut nested_parser = NestedRuleParser {
      options: self.options,
      at_rule_parser: self.at_rule_parser,
      declarations: DeclarationList::new(),
      important_declarations: DeclarationList::new(),
      rules: &mut rules,
      is_in_style_rule: self.is_in_style_rule || is_style_rule,
      allow_declarations: self.allow_declarations || self.is_in_style_rule || is_style_rule,
    };

    let parse_declarations = nested_parser.parse_declarations();
    let mut errors = Vec::new();
    let mut iter = RuleBodyParser::new(input, &mut nested_parser);
    while let Some(result) = iter.next() {
      match result {
        Ok(()) => {}
        Err((e, _)) => {
          if parse_declarations {
            iter.parser.declarations.clear();
            iter.parser.important_declarations.clear();
            errors.push(e);
          } else {
            if iter.parser.options.error_recovery {
              iter.parser.options.warn(e);
              continue;
            }
            return Err(e);
          }
        }
      }
    }

    if parse_declarations {
      if !errors.is_empty() {
        if self.options.error_recovery {
          for err in errors {
            self.options.warn(err);
          }
        } else {
          return Err(errors.remove(0));
        }
      }
    }

    Ok((
      DeclarationBlock {
        declarations: nested_parser.declarations,
        important_declarations: nested_parser.important_declarations,
      },
      rules,
    ))
  }

  fn parse_style_block<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<CssRuleList<'i, T::AtRule>, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let loc = Location {
      source_index: self.options.source_index,
      line: loc.line,
      column: loc.column,
    };

    // Declarations can be immediately within @media and @supports blocks that are nested within a parent style rule.
    // These are wrapped in an (invisible) NestedDeclarationsRule.
    let (declarations, mut rules) = self.parse_nested(input, false)?;

    if declarations.len() > 0 {
      rules.0.insert(
        0,
        CssRule::NestedDeclarations(NestedDeclarationsRule { declarations, loc }),
      )
    }

    Ok(rules)
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

impl<'a, 'o, 'b, 'i, T: crate::traits::AtRuleParser<'i>> AtRuleParser<'i> for NestedRuleParser<'a, 'o, 'i, T> {
  type Prelude = AtRulePrelude<'i, T::Prelude>;
  type AtRule = ();
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let result = match_ignore_ascii_case! { &*name,
      "media" => {
        let media = MediaList::parse(input, &self.options)?;
        AtRulePrelude::Media(media)
      },
      "supports" => {
        let cond = SupportsCondition::parse(input, )?;
        AtRulePrelude::Supports(cond)
      },
      "font-face" => {
        AtRulePrelude::FontFace
      },
      // "font-feature-values" => {
      //     if !cfg!(feature = "gecko") {
      //         // Support for this rule is not fully implemented in Servo yet.
      //         return Err(input.new_custom_error(StyleParseErrorKind::UnsupportedAtRule(name.clone())))
      //     }
      //     let family_names = parse_family_name_list(self.context, input)?;
      //     Ok(AtRuleType::WithBlock(AtRuleBlockPrelude::FontFeatureValues(family_names)))
      // },
      "font-feature-values" => {
        let names = match Vec::<FamilyName>::parse(input) {
          Ok(names) => names,
          Err(e) => return Err(e)
        };

        AtRulePrelude::FontFeatureValues(names)
      },
      "font-palette-values" => {
        let name = DashedIdent::parse(input)?;
        AtRulePrelude::FontPaletteValues(name)
      },
      "counter-style" => {
        let name = CustomIdent::parse(input)?;
        AtRulePrelude::CounterStyle(name)
      },
      "viewport" | "-ms-viewport" => {
        let prefix = if starts_with_ignore_ascii_case(&*name, "-ms") {
          VendorPrefix::Ms
        } else {
          VendorPrefix::None
        };
        AtRulePrelude::Viewport(prefix)
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

        let name = input.try_parse(KeyframesName::parse)?;
        AtRulePrelude::Keyframes(name, prefix)
      },
      "page" => {
        let selectors = input.try_parse(|input| input.parse_comma_separated(PageSelector::parse)).unwrap_or_default();
        AtRulePrelude::Page(selectors)
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

        AtRulePrelude::MozDocument
      },
      "layer" => {
        let names = match Vec::<LayerName>::parse(input) {
          Ok(names) => names,
          Err(ParseError { kind: ParseErrorKind::Basic(BasicParseErrorKind::EndOfInput), .. }) => Vec::new(),
          Err(e) => return Err(e)
        };
        AtRulePrelude::Layer(names)
      },
      "container" => {
        let name = input.try_parse(ContainerName::parse).ok();
        let condition = ContainerCondition::parse_with_options(input, &self.options)?;
        AtRulePrelude::Container(name, condition)
      },
      "starting-style" => {
        AtRulePrelude::StartingStyle
      },
      "scope" => {
        let selector_parser = SelectorParser {
          is_nesting_allowed: true,
          options: &self.options,
        };

        let scope_start = if input.try_parse(|input| input.expect_parenthesis_block()).is_ok() {
          Some(input.parse_nested_block(|input| {
            // https://drafts.csswg.org/css-cascade-6/#scoped-rules
            // TODO: disallow pseudo elements?
            SelectorList::parse_relative(&selector_parser, input, ParseErrorRecovery::IgnoreInvalidSelector, NestingRequirement::None)
          })?)
        } else {
          None
        };

        let scope_end = if input.try_parse(|input| input.expect_ident_matching("to")).is_ok() {
          input.expect_parenthesis_block()?;
          Some(input.parse_nested_block(|input| {
            SelectorList::parse_relative(&selector_parser, input, ParseErrorRecovery::IgnoreInvalidSelector, NestingRequirement::None)
          })?)
        } else {
          None
        };

        AtRulePrelude::Scope(scope_start, scope_end)
      },
      "view-transition" => {
        AtRulePrelude::ViewTransition
      },
      "nest" if self.is_in_style_rule => {
        self.options.warn(input.new_custom_error(ParserError::DeprecatedNestRule));
        let selector_parser = SelectorParser {
          is_nesting_allowed: true,
          options: &self.options,
        };
        let selectors = SelectorList::parse(&selector_parser, input, ParseErrorRecovery::DiscardList, NestingRequirement::Contained)?;
        AtRulePrelude::Nest(selectors)
      },

      "value" if self.options.css_modules.is_some() => {
        return Err(input.new_custom_error(ParserError::DeprecatedCssModulesValueRule));
      },

      "property" => {
        let name = DashedIdent::parse(input)?;
        return Ok(AtRulePrelude::Property(name))
      },

      _ => parse_custom_at_rule_prelude(&name, input, self.options, self.at_rule_parser)?
    };

    if self.is_in_style_rule && !result.allowed_in_style_rule() {
      return Err(input.new_error(BasicParseErrorKind::AtRuleInvalid(name.clone())));
    }

    Ok(result)
  }

  #[inline]
  fn rule_without_block(
    &mut self,
    prelude: AtRulePrelude<'i, T::Prelude>,
    start: &ParserState,
  ) -> Result<Self::AtRule, ()> {
    let loc = self.loc(start);
    match prelude {
      AtRulePrelude::Layer(names) => {
        if self.is_in_style_rule || names.is_empty() {
          return Err(());
        }

        self.rules.0.push(CssRule::LayerStatement(LayerStatementRule { names, loc }));
        Ok(())
      }
      AtRulePrelude::Unknown(name, prelude) => {
        self.rules.0.push(CssRule::Unknown(UnknownAtRule {
          name,
          prelude,
          block: None,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Custom(prelude) => {
        self.rules.0.push(parse_custom_at_rule_without_block(
          prelude,
          start,
          self.options,
          self.at_rule_parser,
          self.is_in_style_rule,
        )?);
        Ok(())
      }
      _ => Err(()),
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<(), ParseError<'i, Self::Error>> {
    let loc = self.loc(start);
    match prelude {
      AtRulePrelude::FontFace => {
        let mut decl_parser = FontFaceDeclarationParser;
        let mut parser = RuleBodyParser::new(input, &mut decl_parser);
        let mut properties = vec![];
        while let Some(decl) = parser.next() {
          if let Ok(decl) = decl {
            properties.push(decl);
          }
        }
        self.rules.0.push(CssRule::FontFace(FontFaceRule { properties, loc }));
        Ok(())
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
        self.rules.0.push(CssRule::FontPaletteValues(rule));
        Ok(())
      }
      AtRulePrelude::CounterStyle(name) => {
        self.rules.0.push(CssRule::CounterStyle(CounterStyleRule {
          name,
          declarations: DeclarationBlock::parse(input, self.options)?,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Media(query) => {
        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::Media(MediaRule { query, rules, loc }));
        Ok(())
      }
      AtRulePrelude::Supports(condition) => {
        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::Supports(SupportsRule { condition, rules, loc }));
        Ok(())
      }
      AtRulePrelude::Container(name, condition) => {
        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::Container(ContainerRule {
          name,
          condition,
          rules,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Scope(scope_start, scope_end) => {
        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::Scope(ScopeRule {
          scope_start,
          scope_end,
          rules,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Viewport(vendor_prefix) => {
        self.rules.0.push(CssRule::Viewport(ViewportRule {
          vendor_prefix,
          // TODO: parse viewport descriptors rather than properties
          // https://drafts.csswg.org/css-device-adapt/#viewport-desc
          declarations: DeclarationBlock::parse(input, self.options)?,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Keyframes(name, vendor_prefix) => {
        let mut parser = KeyframeListParser;
        let iter = RuleBodyParser::new(input, &mut parser);
        self.rules.0.push(CssRule::Keyframes(KeyframesRule {
          name,
          keyframes: iter.filter_map(Result::ok).collect(),
          vendor_prefix,
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Page(selectors) => {
        let rule = PageRule::parse(selectors, input, loc, self.options)?;
        self.rules.0.push(CssRule::Page(rule));
        Ok(())
      }
      AtRulePrelude::MozDocument => {
        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::MozDocument(MozDocumentRule { rules, loc }));
        Ok(())
      }
      AtRulePrelude::Layer(names) => {
        let name = if names.is_empty() {
          None
        } else if names.len() == 1 {
          names.into_iter().next()
        } else {
          return Err(input.new_error(BasicParseErrorKind::AtRuleBodyInvalid));
        };

        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::LayerBlock(LayerBlockRule { name, rules, loc }));
        Ok(())
      }
      AtRulePrelude::Property(name) => {
        self.rules.0.push(CssRule::Property(PropertyRule::parse(name, input, loc)?));
        Ok(())
      }
      AtRulePrelude::Import(..)
      | AtRulePrelude::Namespace(..)
      | AtRulePrelude::CustomMedia(..)
      | AtRulePrelude::Charset => {
        // These rules don't have blocks.
        Err(input.new_unexpected_token_error(Token::CurlyBracketBlock))
      }
      AtRulePrelude::StartingStyle => {
        let rules = self.parse_style_block(input)?;
        self.rules.0.push(CssRule::StartingStyle(StartingStyleRule { rules, loc }));
        Ok(())
      }
      AtRulePrelude::ViewTransition => {
        self
          .rules
          .0
          .push(CssRule::ViewTransition(ViewTransitionRule::parse(input, loc)?));
        Ok(())
      }
      AtRulePrelude::Nest(selectors) => {
        let (declarations, rules) = self.parse_nested(input, true)?;
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
      AtRulePrelude::FontFeatureValues(family_names) => {
        let rule = FontFeatureValuesRule::parse(family_names, input, loc, self.options)?;
        self.rules.0.push(CssRule::FontFeatureValues(rule));
        Ok(())
      }
      AtRulePrelude::Unknown(name, prelude) => {
        self.rules.0.push(CssRule::Unknown(UnknownAtRule {
          name,
          prelude,
          block: Some(TokenList::parse(input, &self.options, 0)?),
          loc,
        }));
        Ok(())
      }
      AtRulePrelude::Custom(prelude) => {
        self.rules.0.push(parse_custom_at_rule_body(
          prelude,
          input,
          start,
          self.options,
          self.at_rule_parser,
          self.is_in_style_rule,
        )?);
        Ok(())
      }
    }
  }
}

impl<'a, 'o, 'b, 'i, T: crate::traits::AtRuleParser<'i>> QualifiedRuleParser<'i>
  for NestedRuleParser<'a, 'o, 'i, T>
{
  type Prelude = SelectorList<'i>;
  type QualifiedRule = ();
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let selector_parser = SelectorParser {
      is_nesting_allowed: true,
      options: &self.options,
    };
    if self.is_in_style_rule {
      SelectorList::parse_relative(
        &selector_parser,
        input,
        ParseErrorRecovery::DiscardList,
        NestingRequirement::Implicit,
      )
    } else {
      SelectorList::parse(
        &selector_parser,
        input,
        ParseErrorRecovery::DiscardList,
        NestingRequirement::None,
      )
    }
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<(), ParseError<'i, Self::Error>> {
    let loc = self.loc(start);
    let (declarations, rules) = self.parse_nested(input, true)?;
    self.rules.0.push(CssRule::Style(StyleRule {
      selectors,
      vendor_prefix: VendorPrefix::empty(),
      declarations,
      rules,
      loc,
    }));
    Ok(())
  }
}

/// Parse a declaration within {} block: `color: blue`
impl<'a, 'o, 'i, T: crate::traits::AtRuleParser<'i>> cssparser::DeclarationParser<'i>
  for NestedRuleParser<'a, 'o, 'i, T>
{
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    if self.rules.0.is_empty() {
      parse_declaration(
        name,
        input,
        &mut self.declarations,
        &mut self.important_declarations,
        &self.options,
      )
    } else if let Some(CssRule::NestedDeclarations(last)) = self.rules.0.last_mut() {
      parse_declaration(
        name,
        input,
        &mut last.declarations.declarations,
        &mut last.declarations.important_declarations,
        &self.options,
      )
    } else {
      let loc = self.loc(&input.state());
      let mut nested = NestedDeclarationsRule {
        declarations: DeclarationBlock::new(),
        loc,
      };

      parse_declaration(
        name,
        input,
        &mut nested.declarations.declarations,
        &mut nested.declarations.important_declarations,
        &self.options,
      )?;

      self.rules.0.push(CssRule::NestedDeclarations(nested));
      Ok(())
    }
  }
}

impl<'a, 'o, 'b, 'i, T: crate::traits::AtRuleParser<'i>> RuleBodyItemParser<'i, (), ParserError<'i>>
  for NestedRuleParser<'a, 'o, 'i, T>
{
  fn parse_qualified(&self) -> bool {
    true
  }

  fn parse_declarations(&self) -> bool {
    self.allow_declarations
  }
}

fn parse_custom_at_rule_prelude<'i, 't, T: crate::traits::AtRuleParser<'i>>(
  name: &CowRcStr<'i>,
  input: &mut Parser<'i, 't>,
  options: &ParserOptions<'_, 'i>,
  at_rule_parser: &mut T,
) -> Result<AtRulePrelude<'i, T::Prelude>, ParseError<'i, ParserError<'i>>> {
  match at_rule_parser.parse_prelude(name.clone(), input, options) {
    Ok(prelude) => return Ok(AtRulePrelude::Custom(prelude)),
    Err(ParseError {
      kind: ParseErrorKind::Basic(BasicParseErrorKind::AtRuleInvalid(..)),
      ..
    }) => {}
    Err(err) => {
      return Err(match &err.kind {
        ParseErrorKind::Basic(kind) => ParseError {
          kind: ParseErrorKind::Basic(kind.clone()),
          location: err.location,
        },
        _ => input.new_custom_error(ParserError::AtRulePreludeInvalid),
      })
    }
  }

  options.warn(input.new_error(BasicParseErrorKind::AtRuleInvalid(name.clone())));
  input.skip_whitespace();
  let tokens = TokenList::parse(input, &options, 0)?;
  Ok(AtRulePrelude::Unknown(name.into(), tokens))
}

fn parse_custom_at_rule_body<'i, 't, T: crate::traits::AtRuleParser<'i>>(
  prelude: T::Prelude,
  input: &mut Parser<'i, 't>,
  start: &ParserState,
  options: &ParserOptions<'_, 'i>,
  at_rule_parser: &mut T,
  is_nested: bool,
) -> Result<CssRule<'i, T::AtRule>, ParseError<'i, ParserError<'i>>> {
  at_rule_parser
    .parse_block(prelude, start, input, options, is_nested)
    .map(|prelude| CssRule::Custom(prelude))
    .map_err(|err| match &err.kind {
      ParseErrorKind::Basic(kind) => ParseError {
        kind: ParseErrorKind::Basic(kind.clone()),
        location: err.location,
      },
      _ => input.new_error(BasicParseErrorKind::AtRuleBodyInvalid),
    })
}

fn parse_custom_at_rule_without_block<'i, 't, T: crate::traits::AtRuleParser<'i>>(
  prelude: T::Prelude,
  start: &ParserState,
  options: &ParserOptions<'_, 'i>,
  at_rule_parser: &mut T,
  is_nested: bool,
) -> Result<CssRule<'i, T::AtRule>, ()> {
  at_rule_parser
    .rule_without_block(prelude, start, options, is_nested)
    .map(|prelude| CssRule::Custom(prelude))
}

pub fn parse_rule_list<'a, 'o, 'i, 't, T: crate::traits::AtRuleParser<'i>>(
  input: &mut Parser<'i, 't>,
  options: &'a ParserOptions<'o, 'i>,
  at_rule_parser: &mut T,
) -> Result<CssRuleList<'i, T::AtRule>, ParseError<'i, ParserError<'i>>> {
  let mut parser = NestedRuleParser {
    options,
    at_rule_parser,
    declarations: DeclarationList::new(),
    important_declarations: DeclarationList::new(),
    rules: &mut CssRuleList(Vec::new()),
    is_in_style_rule: false,
    allow_declarations: false,
  };

  let (_, rules) = parser.parse_nested(input, false)?;
  Ok(rules)
}

pub fn parse_style_block<'a, 'o, 'i, 't, T: crate::traits::AtRuleParser<'i>>(
  input: &mut Parser<'i, 't>,
  options: &'a ParserOptions<'o, 'i>,
  at_rule_parser: &mut T,
  is_nested: bool,
) -> Result<CssRuleList<'i, T::AtRule>, ParseError<'i, ParserError<'i>>> {
  let mut parser = NestedRuleParser {
    options,
    at_rule_parser,
    declarations: DeclarationList::new(),
    important_declarations: DeclarationList::new(),
    rules: &mut CssRuleList(Vec::new()),
    is_in_style_rule: is_nested,
    allow_declarations: true,
  };

  parser.parse_style_block(input)
}

#[inline]
pub fn starts_with_ignore_ascii_case(string: &str, prefix: &str) -> bool {
  string.len() >= prefix.len() && string.as_bytes()[0..prefix.len()].eq_ignore_ascii_case(prefix.as_bytes())
}
