//! CSS properties related to containment.

#![allow(non_upper_case_globals)]

use bitflags::bitflags;
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

bitflags! {
  /// A value for the [container-type](https://drafts.csswg.org/css-contain-3/#container-type) property.
  /// Establishes the element as a query container for the purpose of container queries.
  ///
  /// An empty value represents `normal`. The `size` and `inline-size` flags are mutually exclusive.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "SerializedContainerType", into = "SerializedContainerType"))]
  #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct ContainerType: u8 {
    /// Establishes a query container for container size queries on the container's own inline axis.
    const InlineSize  = 0b0001;
    /// Establishes a query container for container size queries on both the inline and block axis.
    const Size        = 0b0010;
    /// Establishes a query container for container scroll-state queries.
    const ScrollState = 0b0100;
    /// Establishes a query container for anchor positioning container queries.
    const Anchored    = 0b1000;
  }
}

impl Default for ContainerType {
  fn default() -> ContainerType {
    ContainerType::empty()
  }
}

impl<'i> Parse<'i> for ContainerType {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(ContainerType::empty());
    }

    let mut value = ContainerType::empty();
    let size = ContainerType::InlineSize | ContainerType::Size;

    loop {
      let flag = input.try_parse(|input| -> Result<_, ParseError<'i, ParserError<'i>>> {
        let location = input.current_source_location();
        let ident = input.expect_ident_cloned()?;
        match_ignore_ascii_case! { &ident,
          "inline-size" if !value.intersects(size) => Ok(ContainerType::InlineSize),
          "size" if !value.intersects(size) => Ok(ContainerType::Size),
          "scroll-state" if !value.contains(ContainerType::ScrollState) => Ok(ContainerType::ScrollState),
          "anchored" if !value.contains(ContainerType::Anchored) => Ok(ContainerType::Anchored),
          _ => Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone()))),
        }
      });

      match flag {
        Ok(flag) => value |= flag,
        Err(..) => break,
      }
    }

    if value.is_empty() {
      return Err(input.new_error_for_next_token());
    }

    Ok(value)
  }
}

impl ToCss for ContainerType {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.is_empty() {
      return dest.write_str("normal");
    }

    let mut needs_space = false;
    macro_rules! val {
      ($flag:ident, $name:expr) => {
        #[allow(unused_assignments)]
        if self.contains(ContainerType::$flag) {
          if needs_space {
            dest.write_char(' ')?;
          }
          dest.write_str($name)?;
          needs_space = true;
        }
      };
    }

    val!(InlineSize, "inline-size");
    val!(Size, "size");
    val!(ScrollState, "scroll-state");
    val!(Anchored, "anchored");
    Ok(())
  }
}

impl IsCompatible for ContainerType {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(untagged))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum SerializedContainerType {
  Normal(NormalKeyword),
  Flags(Vec<ContainerTypeFlag>),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "kebab-case"))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum NormalKeyword {
  Normal,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "kebab-case"))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum ContainerTypeFlag {
  InlineSize,
  Size,
  ScrollState,
  Anchored,
}

impl From<ContainerType> for SerializedContainerType {
  fn from(t: ContainerType) -> Self {
    if t.is_empty() {
      return Self::Normal(NormalKeyword::Normal);
    }

    let mut v = Vec::new();
    macro_rules! flag {
      ($t:ident) => {
        if t.contains(ContainerType::$t) {
          v.push(ContainerTypeFlag::$t);
        }
      };
    }
    flag!(InlineSize);
    flag!(Size);
    flag!(ScrollState);
    flag!(Anchored);
    Self::Flags(v)
  }
}

impl From<SerializedContainerType> for ContainerType {
  fn from(t: SerializedContainerType) -> Self {
    match t {
      SerializedContainerType::Normal(_) => ContainerType::empty(),
      SerializedContainerType::Flags(flags) => {
        let mut res = ContainerType::empty();
        for f in flags {
          res |= match f {
            ContainerTypeFlag::InlineSize => ContainerType::InlineSize,
            ContainerTypeFlag::Size => ContainerType::Size,
            ContainerTypeFlag::ScrollState => ContainerType::ScrollState,
            ContainerTypeFlag::Anchored => ContainerType::Anchored,
          }
        }
        res
      }
    }
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for ContainerType {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    SerializedContainerType::json_schema(gen)
  }

  fn schema_name() -> String {
    "ContainerType".into()
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
