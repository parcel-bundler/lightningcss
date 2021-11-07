use cssparser::*;
use crate::values::percentage::Percentage;
use crate::traits::{Parse, ToCss};
use crate::parser::{PropertyDeclarationParser, DeclarationBlock};
use crate::printer::Printer;
use std::fmt::Write;

// TODO: preserve vendor prefix
#[derive(Debug, PartialEq)]
pub struct KeyframesRule {
  pub name: String,
  pub keyframes: Vec<Keyframe>
}

impl ToCss for KeyframesRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("@keyframes ")?;
    serialize_identifier(&self.name, dest)?;
    dest.delim('{', true)?;
    for keyframe in &self.keyframes {
      dest.newline()?;
      keyframe.to_css(dest)?;
    }
    dest.newline()?;
    dest.write_char('}')
  }
}

/// https://drafts.csswg.org/css-animations/#typedef-keyframe-selector
#[derive(Debug, PartialEq)]
pub enum KeyframeSelector {
  Percentage(Percentage),
  From,
  To
}

impl Parse for KeyframeSelector {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(val) = input.try_parse(Percentage::parse) {
      return Ok(KeyframeSelector::Percentage(val))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "from" => Ok(KeyframeSelector::From),
      "to" => Ok(KeyframeSelector::To),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for KeyframeSelector {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      KeyframeSelector::Percentage(p) => {
        if dest.minify && *p == Percentage(1.0) {
          dest.write_str("to")
        } else {
          p.to_css(dest)
        }
      },
      KeyframeSelector::From => {
        if dest.minify {
          dest.write_str("0%")
        } else {
          dest.write_str("from")
        }
      }
      KeyframeSelector::To => dest.write_str("to")
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct Keyframe {
  pub selectors: Vec<KeyframeSelector>,
  pub declarations: DeclarationBlock
}

impl ToCss for Keyframe {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut first = true;
    for selector in &self.selectors {
      if !first {
        dest.delim(',', false)?;
      }
      first = false;
      selector.to_css(dest)?;
    }
    
    self.declarations.to_css(dest)
  }
}

pub struct KeyframeListParser;

impl<'a, 'i> AtRuleParser<'i> for KeyframeListParser {
  type PreludeNoBlock = ();
  type PreludeBlock = ();
  type AtRule = Keyframe;
  type Error = ();
}

impl<'a, 'i> QualifiedRuleParser<'i> for KeyframeListParser {
  type Prelude = Vec<KeyframeSelector>;
  type QualifiedRule = Keyframe;
  type Error = ();

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, ()>> {
    input.parse_comma_separated(KeyframeSelector::parse)
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    start: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::QualifiedRule, ParseError<'i, ()>> {
    let mut parser = DeclarationListParser::new(input, PropertyDeclarationParser);
    let mut declarations = vec![];
    while let Some(decl) = parser.next() {
      if let Ok(decl) = decl {
        declarations.push(decl);
      }
    }
    Ok(Keyframe {
      selectors,
      declarations: DeclarationBlock {
        declarations
      }
    })
  }
}
