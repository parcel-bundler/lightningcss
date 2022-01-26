use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::declaration::DeclarationBlock;
use crate::printer::Printer;
use crate::macros::enum_property;
use crate::error::{ParserError, PrinterError};

/// https://www.w3.org/TR/css-page-3/#typedef-page-selector
#[derive(Debug, PartialEq, Clone)]
pub struct PageSelector {
  pub name: Option<String>,
  pub pseudo_classes: Vec<PagePseudoClass>
}

enum_property! {
  pub enum PagePseudoClass {
    Left,
    Right,
    First,
    Last,
    Blank,
  }
}

impl Parse for PageSelector {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = input.try_parse(|input| input.expect_ident_cloned()).ok().map(|s| s.as_ref().to_owned());
    let mut pseudo_classes = vec![];
    
    loop {
      // Whitespace is not allowed between pseudo classes
      let state = input.state();
      match input.next_including_whitespace() {
        Ok(Token::Colon) => {
          pseudo_classes.push(PagePseudoClass::parse(input)?);
        }
        _ => {
          input.reset(&state);
          break
        }
      }
    }

    if name.is_none() && pseudo_classes.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidPageSelector))
    }

    Ok(PageSelector {
      name,
      pseudo_classes
    })
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PageRule {
  pub selectors: Vec<PageSelector>,
  pub declarations: DeclarationBlock,
  pub loc: SourceLocation
}

impl ToCss for PageRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@page")?;
    if let Some(first) = self.selectors.first() {
      // Space is only required if the first selector has a name.
      if !dest.minify || first.name.is_some() {
        dest.write_char(' ')?;
      }
      let mut first = true;
      for selector in &self.selectors {
        if first {
          first = false;
        } else {
          dest.delim(',', false)?;
        }
        selector.to_css(dest)?;
      }
    }
    self.declarations.to_css(dest)
  }
}

impl ToCss for PageSelector {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    if let Some(name) = &self.name {
      dest.write_str(&name)?;
    }

    for pseudo in &self.pseudo_classes {
      dest.write_char(':')?;
      pseudo.to_css(dest)?;
    }

    Ok(())
  }
}
