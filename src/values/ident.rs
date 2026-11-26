//! CSS identifiers.

use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::properties::css_modules::Specifier;
use crate::traits::{Parse, ParseWithOptions, ToCss};
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;
use std::borrow::Borrow;
use std::ops::Deref;

use super::string::impl_string_type;

/// A CSS [`<custom-ident>`](https://www.w3.org/TR/css-values-4/#custom-idents).
///
/// Custom idents are author defined, and allow any valid identifier except the
/// [CSS-wide keywords](https://www.w3.org/TR/css-values-4/#css-wide-keywords).
/// They may be renamed to include a hash when compiled as part of a CSS module.
#[derive(Debug, Clone, Eq, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "visitor", visit(visit_custom_ident, CUSTOM_IDENTS))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct CustomIdent<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CowArcStr<'i>);

impl<'i> Parse<'i> for CustomIdent<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    let valid = match_ignore_ascii_case! { &ident,
      "initial" | "inherit" | "unset" | "default" | "revert" | "revert-layer" => false,
      _ => true
    };

    if !valid {
      return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())));
    }

    Ok(CustomIdent(ident.into()))
  }
}

impl<'i> ToCss for CustomIdent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.to_css_with_options(dest, true)
  }
}

impl<'i> CustomIdent<'i> {
  /// Write the custom ident to CSS.
  pub(crate) fn to_css_with_options<W>(
    &self,
    dest: &mut Printer<W>,
    enabled_css_modules: bool,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let css_module_custom_idents_enabled = enabled_css_modules
      && dest
        .css_module
        .as_mut()
        .map_or(false, |css_module| css_module.config.custom_idents);
    dest.write_ident(&self.0, css_module_custom_idents_enabled)
  }
}

/// A list of CSS [`<custom-ident>`](https://www.w3.org/TR/css-values-4/#custom-idents) values.
pub type CustomIdentList<'i> = SmallVec<[CustomIdent<'i>; 1]>;

/// The `none` keyword, or a space-separated list of custom idents.
#[derive(Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum NoneOrCustomIdentList<'i> {
  /// None.
  #[default]
  None,
  /// A list of idents.
  #[cfg_attr(feature = "serde", serde(borrow, untagged))]
  Idents(SmallVec<[CustomIdent<'i>; 1]>),
}

impl<'i> Parse<'i> for NoneOrCustomIdentList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut types = SmallVec::new();
    loop {
      if let Ok(ident) = input.try_parse(CustomIdent::parse) {
        if ident == "none" {
          if types.is_empty() {
            return Ok(NoneOrCustomIdentList::None);
          } else {
            return Err(input.new_custom_error(ParserError::InvalidValue));
          }
        }

        types.push(ident);
      } else {
        return Ok(NoneOrCustomIdentList::Idents(types));
      }
    }
  }
}

impl<'i> ToCss for NoneOrCustomIdentList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      NoneOrCustomIdentList::None => dest.write_str("none"),
      NoneOrCustomIdentList::Idents(types) => {
        let mut first = true;
        for ident in types {
          if !first {
            dest.write_char(' ')?;
          } else {
            first = false;
          }
          ident.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

/// A CSS [`<dashed-ident>`](https://www.w3.org/TR/css-values-4/#dashed-idents) declaration.
///
/// Dashed idents are used in cases where an identifier can be either author defined _or_ CSS-defined.
/// Author defined idents must start with two dash characters ("--") or parsing will fail.
#[derive(Debug, Clone, Eq, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "visitor", visit(visit_dashed_ident, DASHED_IDENTS))]
#[cfg_attr(feature = "serde", derive(serde::Serialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct DashedIdent<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CowArcStr<'i>);

impl<'i> Parse<'i> for DashedIdent<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    if !ident.starts_with("--") {
      return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())));
    }

    Ok(DashedIdent(ident.into()))
  }
}

impl<'i> ToCss for DashedIdent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_dashed_ident(&self.0, true)
  }
}

#[cfg(feature = "serde")]
impl<'i, 'de: 'i> serde::Deserialize<'de> for DashedIdent<'i> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let ident = CowArcStr::deserialize(deserializer)?;
    if !ident.starts_with("--") {
      return Err(serde::de::Error::custom("Dashed idents must start with --"));
    }

    Ok(DashedIdent(ident))
  }
}

/// A CSS [`<dashed-ident>`](https://www.w3.org/TR/css-values-4/#dashed-idents) reference.
///
/// Dashed idents are used in cases where an identifier can be either author defined _or_ CSS-defined.
/// Author defined idents must start with two dash characters ("--") or parsing will fail.
///
/// In CSS modules, when the `dashed_idents` option is enabled, the identifier may be followed by the
/// `from` keyword and an argument indicating where the referenced identifier is declared (e.g. a filename).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct DashedIdentReference<'i> {
  /// The referenced identifier.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub ident: DashedIdent<'i>,
  /// CSS modules extension: the filename where the variable is defined.
  /// Only enabled when the CSS modules `dashed_idents` option is turned on.
  pub from: Option<Specifier<'i>>,
}

impl<'i> ParseWithOptions<'i> for DashedIdentReference<'i> {
  fn parse_with_options<'t>(
    input: &mut Parser<'i, 't>,
    options: &crate::stylesheet::ParserOptions,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let ident = DashedIdent::parse(input)?;

    let from = match &options.css_modules {
      Some(config) if config.dashed_idents => {
        if input.try_parse(|input| input.expect_ident_matching("from")).is_ok() {
          Some(Specifier::parse(input)?)
        } else {
          None
        }
      }
      _ => None,
    };

    Ok(DashedIdentReference { ident, from })
  }
}

impl<'i> ToCss for DashedIdentReference<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match &mut dest.css_module {
      Some(css_module) if css_module.config.dashed_idents => {
        if let Some(name) = css_module.reference_dashed(&self.ident.0, &self.from, dest.loc.source_index) {
          dest.write_str("--")?;
          serialize_name(&name, dest)?;
          return Ok(());
        }
      }
      _ => {}
    }

    dest.write_dashed_ident(&self.ident.0, false)
  }
}

/// A CSS [`<ident>`](https://www.w3.org/TR/css-values-4/#css-css-identifier).
#[derive(Debug, Clone, Eq, Hash, Default)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Ident<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CowArcStr<'i>);

impl<'i> Parse<'i> for Ident<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let ident = input.expect_ident()?;
    Ok(Ident(ident.into()))
  }
}

impl<'i> ToCss for Ident<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    serialize_identifier(&self.0, dest)?;
    Ok(())
  }
}

impl<'i> cssparser::ToCss for Ident<'i> {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result
  where
    W: std::fmt::Write,
  {
    serialize_identifier(&self.0, dest)
  }
}

impl_string_type!(Ident);
impl_string_type!(CustomIdent);
impl_string_type!(DashedIdent);
