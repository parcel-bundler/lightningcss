//! The `@page` rule.

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
use cssparser::*;

/// A [page selector](https://www.w3.org/TR/css-page-3/#typedef-page-selector)
/// within a `@page` rule.
///
/// Either a name or at least one pseudo class is required.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PageSelector<'i> {
  /// An optional named page type.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: Option<CowArcStr<'i>>,
  /// A list of page pseudo classes.
  pub pseudo_classes: Vec<PagePseudoClass>,
}

enum_property! {
  /// A page pseudo class within an `@page` selector.
  ///
  /// See [PageSelector](PageSelector).
  pub enum PagePseudoClass {
    /// The `:left` pseudo class.
    Left,
    /// The `:right` pseudo class.
    Right,
    /// The `:first` pseudo class.
    First,
    /// The `:last` pseudo class.
    Last,
    /// The `:blank` pseudo class.
    Blank,
  }
}

impl<'i> Parse<'i> for PageSelector<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = input.try_parse(|input| input.expect_ident().map(|x| x.into())).ok();
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
          break;
        }
      }
    }

    if name.is_none() && pseudo_classes.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidPageSelector));
    }

    Ok(PageSelector { name, pseudo_classes })
  }
}

/// A [@page](https://www.w3.org/TR/css-page-3/#at-page-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PageRule<'i> {
  /// A list of page selectors.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub selectors: Vec<PageSelector<'i>>,
  /// The declarations within the `@page` rule.
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> ToCss for PageRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
    self.declarations.to_css_block(dest)
  }
}

impl<'i> ToCss for PageSelector<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
