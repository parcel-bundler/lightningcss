use cssparser::{Parser, ParserInput, RuleListParser, ParseError};
use parcel_sourcemap::SourceMap;
use crate::rules::{CssRule, CssRuleList};
use crate::parser::TopLevelRuleParser;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::targets::Browsers;
use crate::declaration::{DeclarationHandler, DeclarationBlock};
use crate::traits::Parse;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use data_encoding::Specification;

macro_rules! hash {
  ($str:expr) => {{
    let mut hasher = DefaultHasher::new();
    $str.hash(&mut hasher);
    hasher.finish()
  }};
}

pub use crate::parser::ParserOptions;

pub struct StyleSheet {
  pub filename: String,
  pub rules: CssRuleList
}

impl StyleSheet {
  pub fn parse<'i>(filename: String, code: &'i str, options: ParserOptions) -> Result<StyleSheet, ParseError<'i, ()>> {
    let mut input = ParserInput::new(&code);
    let mut parser = Parser::new(&mut input);
    let rule_list_parser = RuleListParser::new_for_stylesheet(&mut parser, TopLevelRuleParser::new(options));

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
      rules: CssRuleList(rules)
    })
  }

  pub fn minify(&mut self, targets: Option<Browsers>) {
    let mut handler = DeclarationHandler::new(false, targets);
    let mut important_handler = DeclarationHandler::new(true, targets);
    self.rules.minify(targets, &mut handler, &mut important_handler);
  }

  pub fn to_css(&self, minify: bool, source_map: bool, targets: Option<Browsers>) -> Result<(String, Option<SourceMap>), std::fmt::Error> {
    let mut dest = String::new();
    let mut source_map = if source_map {
      let mut sm = SourceMap::new("/");
      sm.add_source(&self.filename);
      Some(sm)
    } else {
      None
    };

    let mut printer = Printer::new(&mut dest, source_map.as_mut(), minify, targets);
    self.rules.to_css(&mut printer)?;
    printer.newline()?;

    Ok((dest, source_map))
  }

  pub fn to_css_module(&self, minify: bool, source_map: bool, targets: Option<Browsers>) -> Result<(String, Option<SourceMap>, HashMap<String, String>), std::fmt::Error> {
    let mut dest = String::new();
    let mut source_map = if source_map {
      let mut sm = SourceMap::new("/");
      sm.add_source(&self.filename);
      Some(sm)
    } else {
      None
    };

    let encoder = {
      let mut spec = Specification::new();
      spec.symbols.push_str("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_-");
      spec.encoding().unwrap()
    };

    let hash = encoder.encode(&(hash!(self.filename) as u32).to_le_bytes());

    let mut exports = HashMap::new();
    let mut printer = Printer::new(&mut dest, source_map.as_mut(), minify, targets);
    printer.css_module = Some(CssModuleData {
      hash: &hash,
      exports: &mut exports
    });
    self.rules.to_css(&mut printer)?;
    printer.newline()?;

    Ok((dest, source_map, exports))
  }
}

pub struct StyleAttribute {
  pub declarations: DeclarationBlock
}

impl StyleAttribute {
  pub fn parse<'i>(code: &'i str) -> Result<StyleAttribute, ParseError<'i, ()>> {
    let mut input = ParserInput::new(&code);
    let mut parser = Parser::new(&mut input);
    Ok(StyleAttribute {
      declarations: DeclarationBlock::parse(&mut parser)?
    })
  }

  pub fn minify(&mut self, targets: Option<Browsers>) {
    let mut handler = DeclarationHandler::new(false, targets);
    let mut important_handler = DeclarationHandler::new(true, targets);
    self.declarations.minify(&mut handler, &mut important_handler);
  }

  pub fn to_css(&self, minify: bool, targets: Option<Browsers>) -> Result<String, std::fmt::Error> {
    let mut dest = String::new();
    let mut printer = Printer::new(&mut dest, None, minify, targets);

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
