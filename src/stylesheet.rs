use cssparser::{Parser, ParserInput, RuleListParser, ParseError};
use parcel_sourcemap::SourceMap;
use crate::rules::{CssRule, CssRuleList};
use crate::parser::TopLevelRuleParser;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::targets::Browsers;
use crate::declaration::{DeclarationHandler, DeclarationBlock};
use crate::css_modules::{hash, CssModule, CssModuleExports};
use std::collections::HashMap;
use crate::dependencies::Dependency;

pub use crate::parser::ParserOptions;

pub struct StyleSheet {
  pub filename: String,
  pub rules: CssRuleList,
  options: ParserOptions
}

#[derive(Default)]
pub struct PrinterOptions {
  pub minify: bool,
  pub source_map: bool,
  pub targets: Option<Browsers>,
  pub analyze_dependencies: bool
}

pub struct ToCssResult {
  pub code: String,
  pub source_map: Option<SourceMap>,
  pub exports: Option<CssModuleExports>,
  pub dependencies: Option<Vec<Dependency>>
}

impl StyleSheet {
  pub fn parse<'i>(filename: String, code: &'i str, options: ParserOptions) -> Result<StyleSheet, ParseError<'i, ()>> {
    let mut input = ParserInput::new(&code);
    let mut parser = Parser::new(&mut input);
    let rule_list_parser = RuleListParser::new_for_stylesheet(&mut parser, TopLevelRuleParser::new(&options));

    let mut rules = vec![];
    for rule in rule_list_parser {
      let rule = match rule {
        Ok((_, CssRule::Ignored)) => continue,
        Ok((_, rule)) => rule,
        Err((e, _)) => return Err(e)
      };

      rules.push(rule)
    }

    Ok(StyleSheet {
      filename,
      rules: CssRuleList(rules),
      options
    })
  }

  pub fn minify(&mut self, targets: Option<Browsers>) {
    let mut handler = DeclarationHandler::new(false, targets);
    let mut important_handler = DeclarationHandler::new(true, targets);
    self.rules.minify(targets, &mut handler, &mut important_handler);
  }

  pub fn to_css(&self, options: PrinterOptions) -> Result<ToCssResult, std::fmt::Error> {
    let mut dest = String::new();
    let mut source_map = if options.source_map {
      let mut sm = SourceMap::new("/");
      sm.add_source(&self.filename);
      Some(sm)
    } else {
      None
    };

    let mut printer = Printer::new(&self.filename, &mut dest, source_map.as_mut(), options.minify, options.targets);

   let mut dependencies = if options.analyze_dependencies {
      Some(Vec::new())
    } else {
      None
    };

    printer.dependencies = dependencies.as_mut();

    if self.options.css_modules {
      let h = hash(&self.filename);
      let mut exports = HashMap::new();
      printer.css_module = Some(CssModule {
        hash: &h,
        exports: &mut exports
      });

      self.rules.to_css(&mut printer)?;
      printer.newline()?;

      Ok(ToCssResult {
        code: dest,
        source_map,
        exports: Some(exports),
        dependencies
      })
    } else {
      self.rules.to_css(&mut printer)?;
      printer.newline()?;
      Ok(ToCssResult {
        code: dest,
        source_map,
        exports: None,
        dependencies
      })
    }
  }
}

pub struct StyleAttribute {
  pub declarations: DeclarationBlock
}

impl StyleAttribute {
  pub fn parse<'i>(code: &'i str) -> Result<StyleAttribute, ParseError<'i, ()>> {
    let mut input = ParserInput::new(&code);
    let mut parser = Parser::new(&mut input);
    let options = ParserOptions::default();
    Ok(StyleAttribute {
      declarations: DeclarationBlock::parse(&mut parser, &options)?
    })
  }

  pub fn minify(&mut self, targets: Option<Browsers>) {
    let mut handler = DeclarationHandler::new(false, targets);
    let mut important_handler = DeclarationHandler::new(true, targets);
    self.declarations.minify(&mut handler, &mut important_handler);
  }

  pub fn to_css(&self, minify: bool, targets: Option<Browsers>) -> Result<String, std::fmt::Error> {
    let mut dest = String::new();
    let mut printer = Printer::new("", &mut dest, None, minify, targets);

    let declarations = &self.declarations.declarations;
    let len = declarations.len();
    for (i, decl) in declarations.iter().enumerate() {
      decl.to_css(&mut printer)?;
      if i != len - 1 {
        printer.write_char(';')?;
        printer.whitespace()?;
      }
    }

    Ok(dest)
  }
}
