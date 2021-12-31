use cssparser::*;
use crate::values::percentage::Percentage;
use crate::traits::{Parse, ToCss};
use crate::declaration::{DeclarationBlock, DeclarationHandler};
use crate::vendor_prefix::VendorPrefix;
use crate::printer::Printer;
use crate::values::ident::CustomIdent;
use crate::parser::ParserOptions;
use crate::error::ParserError;

#[derive(Debug, PartialEq)]
pub struct KeyframesRule {
  pub name: CustomIdent,
  pub keyframes: Vec<Keyframe>,
  pub vendor_prefix: VendorPrefix,
  pub loc: SourceLocation
}

impl KeyframesRule {
  pub(crate) fn minify(&mut self, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    for keyframe in &mut self.keyframes {
      keyframe.declarations.minify(handler, important_handler)
    }
  }
}

impl ToCss for KeyframesRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    let mut first_rule = true;
    macro_rules! write_prefix {
      ($prefix: ident) => {
        if self.vendor_prefix.contains(VendorPrefix::$prefix) {
          #[allow(unused_assignments)]
          if first_rule {
            first_rule = false;
          } else {
            if !dest.minify {
              dest.write_char('\n')?; // no indent
            }
            dest.newline()?;
          }
          dest.write_char('@')?;
          VendorPrefix::$prefix.to_css(dest)?;
          dest.write_str("keyframes ")?;
          self.name.to_css(dest)?;
          dest.whitespace()?;
          dest.write_char('{')?;
          dest.indent();
          let mut first = true;
          for keyframe in &self.keyframes {
            if first {
              first = false;
            } else if !dest.minify {
              dest.write_char('\n')?; // no indent
            }
            dest.newline()?;
            keyframe.to_css(dest)?;
          }
          dest.dedent();
          dest.newline()?;
          dest.write_char('}')?;
        }
      };
    }

    write_prefix!(WebKit);
    write_prefix!(Moz);
    write_prefix!(O);
    write_prefix!(None);
    Ok(())
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
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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

pub(crate) struct KeyframeListParser;

impl<'a, 'i> AtRuleParser<'i> for KeyframeListParser {
  type Prelude = ();
  type AtRule = Keyframe;
  type Error = ParserError<'i>;
}

impl<'a, 'i> QualifiedRuleParser<'i> for KeyframeListParser {
  type Prelude = Vec<KeyframeSelector>;
  type QualifiedRule = Keyframe;
  type Error = ParserError<'i>;

  fn parse_prelude<'t>(
    &mut self,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::Prelude, ParseError<'i, ParserError<'i>>> {
    input.parse_comma_separated(KeyframeSelector::parse)
  }

  fn parse_block<'t>(
    &mut self,
    selectors: Self::Prelude,
    _: &ParserState,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self::QualifiedRule, ParseError<'i, ParserError<'i>>> {
    // For now there are no options that apply within @keyframes
    let options = ParserOptions::default();
    Ok(Keyframe {
      selectors,
      declarations: DeclarationBlock::parse(input, &options)?
    })
  }
}
