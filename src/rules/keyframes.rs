use cssparser::*;
use super::{Location, CssRuleList, CssRule};
use super::supports::SupportsRule;
use crate::properties::Property;
use crate::properties::custom::CustomProperty;
use crate::targets::Browsers;
use crate::values::color::ColorFallbackKind;
use crate::values::percentage::Percentage;
use crate::traits::{Parse, ToCss};
use crate::declaration::DeclarationBlock;
use crate::vendor_prefix::VendorPrefix;
use crate::printer::Printer;
use crate::values::ident::CustomIdent;
use crate::parser::ParserOptions;
use crate::error::{ParserError, PrinterError};
use super::MinifyContext;

#[derive(Debug, PartialEq, Clone)]
pub struct KeyframesRule<'i> {
  pub name: CustomIdent<'i>,
  pub keyframes: Vec<Keyframe<'i>>,
  pub vendor_prefix: VendorPrefix,
  pub loc: Location
}

impl<'i> KeyframesRule<'i> {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext<'_, 'i>) {
    for keyframe in &mut self.keyframes {
      keyframe.declarations.minify(context.handler, context.important_handler, context.handler_context)
    }
  }

  pub(crate) fn get_fallbacks(&mut self, targets: Browsers) -> Vec<CssRule<'i>> {
    let mut fallbacks = ColorFallbackKind::empty();
    for keyframe in &self.keyframes {
      for property in &keyframe.declarations.declarations {
        match property {
          Property::Custom(custom) => {
            fallbacks |= custom.value.get_necessary_fallbacks(targets);
          }
          _ => {}
        }
      }
    }

    let mut res = Vec::new();
    let lowest_fallback = fallbacks.lowest();
    fallbacks.remove(lowest_fallback);

    if fallbacks.contains(ColorFallbackKind::P3) {
      res.push(self.get_fallback(ColorFallbackKind::P3));
    }

    if fallbacks.contains(ColorFallbackKind::LAB) || (!lowest_fallback.is_empty() && lowest_fallback != ColorFallbackKind::LAB) {
      res.push(self.get_fallback(ColorFallbackKind::LAB));
    }

    if !lowest_fallback.is_empty() {  
      for keyframe in &mut self.keyframes {
        for property in &mut keyframe.declarations.declarations {
          match property {
            Property::Custom(custom) => {
              custom.value = custom.value.get_fallback(lowest_fallback);
            }
            _ => {}
          }
        }
      }
    }

    res
  }

  fn get_fallback(&self, kind: ColorFallbackKind) -> CssRule<'i> {
    let keyframes = self.keyframes
      .iter()
      .map(|keyframe| Keyframe {
        selectors: keyframe.selectors.clone(),
        declarations: DeclarationBlock {
          important_declarations: vec![],
          declarations: keyframe.declarations.declarations
            .iter()
            .map(|property| {
              match property {
                Property::Custom(custom) => {
                  Property::Custom(CustomProperty {
                    name: custom.name.clone(),
                    value: custom.value.get_fallback(kind)
                  })
                },
                _ => property.clone()
              }
            })
            .collect()
        }
      })
      .collect();

    CssRule::Supports(SupportsRule {
      condition: kind.supports_condition(),
      rules: CssRuleList(vec![CssRule::Keyframes(KeyframesRule {
        name: self.name.clone(),
        keyframes,
        vendor_prefix: self.vendor_prefix,
        loc: self.loc.clone()
      })]),
      loc: self.loc.clone()
    })
  }
}

impl<'i> ToCss for KeyframesRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
#[derive(Debug, PartialEq, Clone)]
pub enum KeyframeSelector {
  Percentage(Percentage),
  From,
  To
}

impl<'i> Parse<'i> for KeyframeSelector {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

#[derive(Debug, PartialEq, Clone)]
pub struct Keyframe<'i> {
  pub selectors: Vec<KeyframeSelector>,
  pub declarations: DeclarationBlock<'i>
}

impl<'i> ToCss for Keyframe<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
  type AtRule = Keyframe<'i>;
  type Error = ParserError<'i>;
}

impl<'a, 'i> QualifiedRuleParser<'i> for KeyframeListParser {
  type Prelude = Vec<KeyframeSelector>;
  type QualifiedRule = Keyframe<'i>;
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
