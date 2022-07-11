//! CSS properties related to containment.

#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use cssparser::*;
use smallvec::SmallVec;

use crate::{
  context::PropertyHandlerContext,
  declaration::{DeclarationBlock, DeclarationList},
  error::{ParserError, PrinterError},
  macros::{define_shorthand, shorthand_handler},
  printer::Printer,
  properties::{Property, PropertyId},
  rules::container::ContainerName as ContainerIdent,
  targets::Browsers,
  traits::{Parse, PropertyHandler, Shorthand, ToCss},
};

bitflags! {
  /// A value for the [container-type](https://drafts.csswg.org/css-contain-3/#container-type) property.
  /// Establishes the element as a query container for the purpose of container queries.
  ///
  /// `normal` is mutually exclusive, but other combinations of flags are allowed.
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
  pub struct ContainerType: u8 {
    /// The element is not a query container for any container size queries,
    /// but remains a query container for container style queries.
    const Normal    = 0b00000001;
    /// Establishes a query container for container size queries on the container’s own inline axis.
    const InlineSize = 0b00000010;
    /// Establishes a query container for container size queries on both the inline and block axis.
    const Size = 0b00000100;
  }
}

impl Default for ContainerType {
  fn default() -> Self {
    ContainerType::Normal
  }
}

impl<'i> Parse<'i> for ContainerType {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut flags = ContainerType::empty();
    while let Ok(ident) = input.try_parse(|input| input.expect_ident_cloned()) {
      let location = input.current_source_location();
      let flag = match_ignore_ascii_case! { &ident,
        "normal" if flags.is_empty() => return Ok(ContainerType::Normal), // mutually exclusive
        "size" => ContainerType::Size,
        "inline-size" => ContainerType::InlineSize,
        _ => return Err(location.new_unexpected_token_error(
          cssparser::Token::Ident(ident.clone())
        ))
      };
      if flags.contains(flag) {
        return Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone())));
      }
      flags |= flag;
    }

    if flags.is_empty() {
      return Err(input.new_error_for_next_token());
    } else {
      return Ok(flags);
    }
  }
}

impl ToCss for ContainerType {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.contains(ContainerType::Normal) {
      dest.write_str("normal")?;
    } else if self.contains(ContainerType::Size) {
      dest.write_str("size")?;
    } else if self.contains(ContainerType::InlineSize) {
      dest.write_str("inline-size")?;
    }

    Ok(())
  }
}

/// A value for the [container-name](https://drafts.csswg.org/css-contain-3/#container-name) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
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
