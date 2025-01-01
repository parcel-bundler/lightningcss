//! The `@view-transition` rule.

use super::Location;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::properties::custom::CustomProperty;
use crate::stylesheet::ParserOptions;
use crate::traits::{Parse, ToCss};
use crate::values::ident::NoneOrCustomIdentList;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A [@view-transition](https://drafts.csswg.org/css-view-transitions-2/#view-transition-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ViewTransitionRule<'i> {
  /// Declarations in the `@view-transition` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub properties: Vec<ViewTransitionProperty<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

/// A property within a `@view-transition` rule.
///
///  See [ViewTransitionRule](ViewTransitionRule).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "property", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ViewTransitionProperty<'i> {
  /// The `navigation` property.
  Navigation(Navigation),
  /// The `types` property.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Types(NoneOrCustomIdentList<'i>),
  /// An unknown or unsupported property.
  Custom(CustomProperty<'i>),
}

/// A value for the [navigation](https://drafts.csswg.org/css-view-transitions-2/#view-transition-navigation-descriptor)
/// property in a `@view-transition` rule.
#[derive(Debug, Clone, PartialEq, Default, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Navigation {
  /// There will be no transition.
  #[default]
  None,
  /// The transition will be enabled if the navigation is same-origin.
  Auto,
}

pub(crate) struct ViewTransitionDeclarationParser;

impl<'i> cssparser::DeclarationParser<'i> for ViewTransitionDeclarationParser {
  type Declaration = ViewTransitionProperty<'i>;
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    let state = input.state();
    match_ignore_ascii_case! { &name,
      "navigation" => {
        // https://drafts.csswg.org/css-view-transitions-2/#view-transition-navigation-descriptor
        if let Ok(navigation) = Navigation::parse(input) {
          return Ok(ViewTransitionProperty::Navigation(navigation));
        }
      },
      "types" => {
        // https://drafts.csswg.org/css-view-transitions-2/#types-cross-doc
        if let Ok(types) = NoneOrCustomIdentList::parse(input) {
          return Ok(ViewTransitionProperty::Types(types));
        }
      },
      _ => return Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }

    input.reset(&state);
    return Ok(ViewTransitionProperty::Custom(CustomProperty::parse(
      name.into(),
      input,
      &ParserOptions::default(),
    )?));
  }
}

/// Default methods reject all at rules.
impl<'i> AtRuleParser<'i> for ViewTransitionDeclarationParser {
  type Prelude = ();
  type AtRule = ViewTransitionProperty<'i>;
  type Error = ParserError<'i>;
}

impl<'i> QualifiedRuleParser<'i> for ViewTransitionDeclarationParser {
  type Prelude = ();
  type QualifiedRule = ViewTransitionProperty<'i>;
  type Error = ParserError<'i>;
}

impl<'i> RuleBodyItemParser<'i, ViewTransitionProperty<'i>, ParserError<'i>> for ViewTransitionDeclarationParser {
  fn parse_qualified(&self) -> bool {
    false
  }

  fn parse_declarations(&self) -> bool {
    true
  }
}

impl<'i> ViewTransitionRule<'i> {
  pub(crate) fn parse<'t>(
    input: &mut Parser<'i, 't>,
    loc: Location,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut decl_parser = ViewTransitionDeclarationParser;
    let mut parser = RuleBodyParser::new(input, &mut decl_parser);
    let mut properties = vec![];
    while let Some(decl) = parser.next() {
      if let Ok(decl) = decl {
        properties.push(decl);
      }
    }

    Ok(ViewTransitionRule { properties, loc })
  }
}

impl<'i> ToCss for ViewTransitionRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@view-transition")?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    let len = self.properties.len();
    for (i, prop) in self.properties.iter().enumerate() {
      dest.newline()?;
      prop.to_css(dest)?;
      if i != len - 1 || !dest.minify {
        dest.write_char(';')?;
      }
    }
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

impl<'i> ToCss for ViewTransitionProperty<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    macro_rules! property {
      ($prop: literal, $value: expr) => {{
        dest.write_str($prop)?;
        dest.delim(':', false)?;
        $value.to_css(dest)
      }};
    }

    match self {
      ViewTransitionProperty::Navigation(f) => property!("navigation", f),
      ViewTransitionProperty::Types(t) => property!("types", t),
      ViewTransitionProperty::Custom(custom) => {
        dest.write_str(custom.name.as_ref())?;
        dest.delim(':', false)?;
        custom.value.to_css(dest, true)
      }
    }
  }
}
