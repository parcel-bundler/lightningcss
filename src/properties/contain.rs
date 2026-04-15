//! CSS properties related to containment.

#![allow(non_upper_case_globals)]

use cssparser::*;
use smallvec::SmallVec;

#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use crate::{
  context::PropertyHandlerContext,
  declaration::{DeclarationBlock, DeclarationList},
  error::{ParserError, PrinterError},
  macros::{define_shorthand, shorthand_handler},
  printer::Printer,
  properties::{Property, PropertyId},
  rules::container::ContainerName as ContainerIdent,
  targets::Browsers,
  traits::{IsCompatible, Parse, PropertyHandler, Shorthand, ToCss},
};

/// A value for the [container-type](https://drafts.csswg.org/css-contain-3/#container-type) property.
/// Establishes the element as a query container for the purpose of container queries.
#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum ContainerType {
  /// The element is not a query container for any container size queries,
  /// but remains a query container for container style queries.
  #[cfg_attr(feature = "serde", serde(rename = "normal"))]
  Normal,
  /// Establishes a query container for container size queries on the container’s own inline axis.
  #[cfg_attr(feature = "serde", serde(rename = "inline-size"))]
  InlineSize,
  /// Establishes a query container for container size queries on both the inline and block axis.
  #[cfg_attr(feature = "serde", serde(rename = "size"))]
  Size,
  /// Establishes a query container for container scroll-state queries.
  #[cfg_attr(feature = "serde", serde(rename = "scroll-state"))]
  ScrollState,
  /// Establishes a query container for anchor positioning container queries.
  #[cfg_attr(feature = "serde", serde(rename = "anchored"))]
  Anchored,
  /// Establishes inline-size and scroll-state query containers.
  #[cfg_attr(feature = "serde", serde(rename = "inline-size scroll-state"))]
  InlineSizeScrollState,
  /// Establishes size and scroll-state query containers.
  #[cfg_attr(feature = "serde", serde(rename = "size scroll-state"))]
  SizeScrollState,
  /// Establishes inline-size and anchored query containers.
  #[cfg_attr(feature = "serde", serde(rename = "inline-size anchored"))]
  InlineSizeAnchored,
  /// Establishes size and anchored query containers.
  #[cfg_attr(feature = "serde", serde(rename = "size anchored"))]
  SizeAnchored,
  /// Establishes scroll-state and anchored query containers.
  #[cfg_attr(feature = "serde", serde(rename = "scroll-state anchored"))]
  ScrollStateAnchored,
  /// Establishes inline-size, scroll-state, and anchored query containers.
  #[cfg_attr(feature = "serde", serde(rename = "inline-size scroll-state anchored"))]
  InlineSizeScrollStateAnchored,
  /// Establishes size, scroll-state, and anchored query containers.
  #[cfg_attr(feature = "serde", serde(rename = "size scroll-state anchored"))]
  SizeScrollStateAnchored,
}

#[derive(Clone, Copy, PartialEq)]
enum ContainerSizeType {
  InlineSize,
  Size,
}

impl<'i> Parse<'i> for ContainerType {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(ContainerType::Normal);
    }

    let mut size = None;
    let mut scroll_state = false;
    let mut anchored = false;

    while let Ok(ident) = input.try_parse(|input| input.expect_ident_cloned()) {
      let location = input.current_source_location();
      match_ignore_ascii_case! { &ident,
        "inline-size" => {
          if size.is_some() {
            return Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone())));
          }
          size = Some(ContainerSizeType::InlineSize);
        },
        "size" => {
          if size.is_some() {
            return Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone())));
          }
          size = Some(ContainerSizeType::Size);
        },
        "scroll-state" => {
          if scroll_state {
            return Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone())));
          }
          scroll_state = true;
        },
        "anchored" => {
          if anchored {
            return Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone())));
          }
          anchored = true;
        },
        _ => return Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone()))),
      }
    }

    Ok(match (size, scroll_state, anchored) {
      (None, false, false) => return Err(input.new_error_for_next_token()),
      (Some(ContainerSizeType::InlineSize), false, false) => ContainerType::InlineSize,
      (Some(ContainerSizeType::Size), false, false) => ContainerType::Size,
      (None, true, false) => ContainerType::ScrollState,
      (None, false, true) => ContainerType::Anchored,
      (Some(ContainerSizeType::InlineSize), true, false) => ContainerType::InlineSizeScrollState,
      (Some(ContainerSizeType::Size), true, false) => ContainerType::SizeScrollState,
      (Some(ContainerSizeType::InlineSize), false, true) => ContainerType::InlineSizeAnchored,
      (Some(ContainerSizeType::Size), false, true) => ContainerType::SizeAnchored,
      (None, true, true) => ContainerType::ScrollStateAnchored,
      (Some(ContainerSizeType::InlineSize), true, true) => ContainerType::InlineSizeScrollStateAnchored,
      (Some(ContainerSizeType::Size), true, true) => ContainerType::SizeScrollStateAnchored,
    })
  }
}

impl ToCss for ContainerType {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_str(match self {
      ContainerType::Normal => "normal",
      ContainerType::InlineSize => "inline-size",
      ContainerType::Size => "size",
      ContainerType::ScrollState => "scroll-state",
      ContainerType::Anchored => "anchored",
      ContainerType::InlineSizeScrollState => "inline-size scroll-state",
      ContainerType::SizeScrollState => "size scroll-state",
      ContainerType::InlineSizeAnchored => "inline-size anchored",
      ContainerType::SizeAnchored => "size anchored",
      ContainerType::ScrollStateAnchored => "scroll-state anchored",
      ContainerType::InlineSizeScrollStateAnchored => "inline-size scroll-state anchored",
      ContainerType::SizeScrollStateAnchored => "size scroll-state anchored",
    })
  }
}

impl Default for ContainerType {
  fn default() -> Self {
    ContainerType::Normal
  }
}

impl IsCompatible for ContainerType {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

/// A value for the [container-name](https://drafts.csswg.org/css-contain-3/#container-name) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ContainerNameList<'i> {
  /// The `none` keyword.
  None,
  /// A list of container names.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Names(SmallVec<[ContainerIdent<'i>; 1]>),
}

impl<'i> Default for ContainerNameList<'i> {
  fn default() -> Self {
    ContainerNameList::None
  }
}

impl<'i> Parse<'i> for ContainerNameList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(ContainerNameList::None);
    }

    let mut names = SmallVec::new();
    while let Ok(name) = input.try_parse(ContainerIdent::parse) {
      names.push(name);
    }

    if names.is_empty() {
      return Err(input.new_error_for_next_token());
    } else {
      return Ok(ContainerNameList::Names(names));
    }
  }
}

impl<'i> ToCss for ContainerNameList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      ContainerNameList::None => dest.write_str("none"),
      ContainerNameList::Names(names) => {
        let mut first = true;
        for name in names {
          if first {
            first = false;
          } else {
            dest.write_char(' ')?;
          }
          name.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl IsCompatible for ContainerNameList<'_> {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

define_shorthand! {
  /// A value for the [container](https://drafts.csswg.org/css-contain-3/#container-shorthand) shorthand property.
  pub struct Container<'i> {
    /// The container name.
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: ContainerName(ContainerNameList<'i>),
    /// The container type.
    container_type: ContainerType(ContainerType),
  }
}

impl<'i> Parse<'i> for Container<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = ContainerNameList::parse(input)?;
    let container_type = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      ContainerType::parse(input)?
    } else {
      ContainerType::default()
    };
    Ok(Container { name, container_type })
  }
}

impl<'i> ToCss for Container<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.name.to_css(dest)?;
    if self.container_type != ContainerType::default() {
      dest.delim('/', true)?;
      self.container_type.to_css(dest)?;
    }
    Ok(())
  }
}

shorthand_handler!(ContainerHandler -> Container<'i> {
  name: ContainerName(ContainerNameList<'i>),
  container_type: ContainerType(ContainerType),
});
