//! CSS style sheets and style attributes.
//!
//! A [StyleSheet](StyleSheet) represents a `.css` file or `<style>` element in HTML.
//! A [StyleAttribute](StyleAttribute) represents an inline `style` attribute in HTML.

use crate::compat::Feature;
use crate::context::{DeclarationContext, PropertyHandlerContext};
use crate::css_modules::{CssModule, CssModuleExports, CssModuleReferences};
use crate::declaration::{DeclarationBlock, DeclarationHandler};
use crate::dependencies::Dependency;
use crate::error::{Error, ErrorLocation, MinifyErrorKind, ParserError, PrinterError, PrinterErrorKind};
use crate::parser::TopLevelRuleParser;
use crate::printer::Printer;
use crate::rules::{CssRule, CssRuleList, MinifyContext};
use crate::targets::Browsers;
use crate::traits::ToCss;
use cssparser::{Parser, ParserInput, RuleListParser};
use parcel_sourcemap::SourceMap;
use std::collections::{HashMap, HashSet};

pub use crate::parser::ParserOptions;
pub use crate::printer::PrinterOptions;
pub use crate::printer::PseudoClasses;

/// A CSS style sheet, representing a `.css` file or inline `<style>` element.
///
/// Style sheets can be parsed from a string, constructed from scratch,
/// or created using a [Bundler](super::bundler::Bundler). Then, they can be
/// minified and transformed for a set of target browsers, and serialied to a string.
///
/// # Example
///
/// ```
/// use parcel_css::stylesheet::{
///   StyleSheet, ParserOptions, MinifyOptions, PrinterOptions
/// };
///
/// // Parse a style sheet from a string.
/// let mut stylesheet = StyleSheet::parse(
///   r#"
///   .foo {
///     color: red;
///   }
///
///   .bar {
///     color: red;
///   }
///   "#,
///   ParserOptions::default()
/// ).unwrap();
///
/// // Minify the stylesheet.
/// stylesheet.minify(MinifyOptions::default()).unwrap();
///
/// // Serialize it to a string.
/// let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
/// assert_eq!(res.code, ".foo, .bar {\n  color: red;\n}\n");
/// ```
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StyleSheet<'i, 'o> {
  /// A list of top-level rules within the style sheet.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub rules: CssRuleList<'i>,
  /// A list of file names for all source files included within the style sheet.
  /// Sources are referenced by index in the `loc` property of each rule.
  pub sources: Vec<String>,
  /// The source map URL extracted from the original style sheet.
  pub source_map_url: Option<String>,
  #[cfg_attr(feature = "serde", serde(skip))]
  /// The options the style sheet was originally parsed with.
  options: ParserOptions<'o, 'i>,
}

/// Options for the `minify` function of a [StyleSheet](StyleSheet)
/// or [StyleAttribute](StyleAttribute).
#[derive(Default)]
pub struct MinifyOptions {
  /// Browser targets to compile the CSS for.
  pub targets: Option<Browsers>,
  /// A list of known unused symbols, including CSS class names,
  /// ids, and `@keyframe` names. The declarations of these will be removed.
  pub unused_symbols: HashSet<String>,
}

/// A result returned from `to_css`, including the serialize CSS
/// and other metadata depending on the input options.
#[derive(Debug)]
pub struct ToCssResult {
  /// Serialized CSS code.
  pub code: String,
  /// A map of CSS module exports, if the `css_modules` option was
  /// enabled during parsing.
  pub exports: Option<CssModuleExports>,
  /// A map of CSS module references, if the `css_modules` config
  /// had `dashed_idents` enabled.
  pub references: Option<CssModuleReferences>,
  /// A list of dependencies (e.g. `@import` or `url()`) found in
  /// the style sheet, if the `analyze_dependencies` option is enabled.
  pub dependencies: Option<Vec<Dependency>>,
}

impl<'i, 'o> StyleSheet<'i, 'o> {
  /// Creates a new style sheet with the given source filenames and rules.
  pub fn new(sources: Vec<String>, rules: CssRuleList<'i>, options: ParserOptions<'o, 'i>) -> StyleSheet<'i, 'o> {
    StyleSheet {
      sources,
      source_map_url: None,
      rules,
      options,
    }
  }

  /// Parse a style sheet from a string.
  pub fn parse(code: &'i str, options: ParserOptions<'o, 'i>) -> Result<Self, Error<ParserError<'i>>> {
    let mut input = ParserInput::new(&code);
    let mut parser = Parser::new(&mut input);
    let rule_list_parser = RuleListParser::new_for_stylesheet(&mut parser, TopLevelRuleParser::new(&options));

    let mut rules = vec![];
    for rule in rule_list_parser {
      let rule = match rule {
        Ok((_, CssRule::Ignored)) => continue,
        Ok((_, rule)) => rule,
        Err((e, _)) => {
          if options.error_recovery {
            options.warn(e);
            continue;
          }

          return Err(Error::from(e, options.filename.clone()));
        }
      };

      rules.push(rule)
    }

    Ok(StyleSheet {
      sources: vec![options.filename.clone()],
      source_map_url: parser.current_source_map_url().map(|s| s.to_owned()),
      rules: CssRuleList(rules),
      options,
    })
  }

  /// Returns the inline source map associated with the style sheet.
  pub fn source_map(&self) -> Option<SourceMap> {
    let source_map_url = self.source_map_url.as_ref()?;
    SourceMap::from_data_url("/", source_map_url).ok()
  }

  /// Minify and transform the style sheet for the provided browser targets.
  pub fn minify(&mut self, options: MinifyOptions) -> Result<(), Error<MinifyErrorKind>> {
    let mut context = PropertyHandlerContext::new(options.targets, &options.unused_symbols);
    let mut handler = DeclarationHandler::new(options.targets);
    let mut important_handler = DeclarationHandler::new(options.targets);

    // @custom-media rules may be defined after they are referenced, but may only be defined at the top level
    // of a stylesheet. Do a pre-scan here and create a lookup table by name.
    let custom_media = if self.options.custom_media
      && options.targets.is_some()
      && !Feature::CustomMediaQueries.is_compatible(options.targets.unwrap())
    {
      let mut custom_media = HashMap::new();
      for rule in &self.rules.0 {
        if let CssRule::CustomMedia(rule) = rule {
          custom_media.insert(rule.name.0.clone(), rule.clone());
        }
      }
      Some(custom_media)
    } else {
      None
    };

    let mut ctx = MinifyContext {
      targets: &options.targets,
      handler: &mut handler,
      important_handler: &mut important_handler,
      handler_context: &mut context,
      unused_symbols: &options.unused_symbols,
      custom_media,
    };

    self.rules.minify(&mut ctx, false).map_err(|e| Error {
      kind: e.kind,
      loc: Some(ErrorLocation::new(
        e.loc,
        self.sources[e.loc.source_index as usize].clone(),
      )),
    })?;

    Ok(())
  }

  /// Serialize the style sheet to a CSS string.
  pub fn to_css(&self, options: PrinterOptions) -> Result<ToCssResult, Error<PrinterErrorKind>> {
    // Make sure we always have capacity > 0: https://github.com/napi-rs/napi-rs/issues/1124.
    let mut dest = String::with_capacity(1);
    let mut printer = Printer::new(&mut dest, options);

    printer.sources = Some(&self.sources);

    if let Some(config) = &self.options.css_modules {
      let mut exports = HashMap::new();
      let mut references = HashMap::new();
      printer.css_module = Some(CssModule::new(
        config,
        printer.filename(),
        &mut exports,
        &mut references,
      ));

      self.rules.to_css(&mut printer)?;
      printer.newline()?;

      Ok(ToCssResult {
        dependencies: printer.dependencies,
        code: dest,
        exports: Some(exports),
        references: Some(references),
      })
    } else {
      self.rules.to_css(&mut printer)?;
      printer.newline()?;

      if let Some(sm) = printer.source_map {
        if let Some(mut input_sm) = self.source_map() {
          let _ = sm.extends(&mut input_sm);
        }
      }

      Ok(ToCssResult {
        dependencies: printer.dependencies,
        code: dest,
        exports: None,
        references: None,
      })
    }
  }
}

/// An inline style attribute, as in HTML or SVG.
///
/// Style attributes can be parsed from a string, minified and transformed
/// for a set of target browsers, and serialied to a string.
///
/// # Example
///
/// ```
/// use parcel_css::stylesheet::{
///   StyleAttribute, ParserOptions, MinifyOptions, PrinterOptions
/// };
///
/// // Parse a style sheet from a string.
/// let mut style = StyleAttribute::parse(
///   "color: yellow; font-family: 'Helvetica';",
///   ParserOptions::default()
/// ).unwrap();
///
/// // Minify the stylesheet.
/// style.minify(MinifyOptions::default());
///
/// // Serialize it to a string.
/// let res = style.to_css(PrinterOptions::default()).unwrap();
/// assert_eq!(res.code, "color: #ff0; font-family: Helvetica");
/// ```
pub struct StyleAttribute<'i> {
  /// The declarations in the style attribute.
  pub declarations: DeclarationBlock<'i>,
}

impl<'i> StyleAttribute<'i> {
  /// Parses a style attribute from a string.
  pub fn parse(
    code: &'i str,
    options: ParserOptions<'_, 'i>,
  ) -> Result<StyleAttribute<'i>, Error<ParserError<'i>>> {
    let mut input = ParserInput::new(&code);
    let mut parser = Parser::new(&mut input);
    Ok(StyleAttribute {
      declarations: DeclarationBlock::parse(&mut parser, &options).map_err(|e| Error::from(e, "".into()))?,
    })
  }

  /// Minify and transform the style attribute for the provided browser targets.
  pub fn minify(&mut self, options: MinifyOptions) {
    let mut context = PropertyHandlerContext::new(options.targets, &options.unused_symbols);
    let mut handler = DeclarationHandler::new(options.targets);
    let mut important_handler = DeclarationHandler::new(options.targets);
    context.context = DeclarationContext::StyleAttribute;
    self.declarations.minify(&mut handler, &mut important_handler, &mut context);
  }

  /// Serializes the style attribute to a CSS string.
  pub fn to_css(&self, options: PrinterOptions) -> Result<ToCssResult, PrinterError> {
    assert!(
      options.source_map.is_none(),
      "Source maps are not supported for style attributes"
    );

    // Make sure we always have capacity > 0: https://github.com/napi-rs/napi-rs/issues/1124.
    let mut dest = String::with_capacity(1);
    let mut printer = Printer::new(&mut dest, options);

    self.declarations.to_css(&mut printer)?;

    Ok(ToCssResult {
      dependencies: printer.dependencies,
      code: dest,
      exports: None,
      references: None,
    })
  }
}
