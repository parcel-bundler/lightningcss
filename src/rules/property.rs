//! The `@property` rule.

use super::Location;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use crate::{
  error::{ParserError, PrinterError},
  printer::Printer,
  properties::custom::TokenList,
  traits::{Parse, ToCss},
  values::{
    ident::DashedIdent,
    syntax::{ParsedComponent, SyntaxString},
  },
};
use cssparser::*;

/// A [@property](https://drafts.css-houdini.org/css-properties-values-api/#at-property-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PropertyRule<'i> {
  /// The name of the custom property to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// A syntax string to specify the grammar for the custom property.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub syntax: SyntaxString,
  /// Whether the custom property is inherited.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub inherits: bool,
  /// An optional initial value for the custom property.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub initial_value: Option<ParsedComponent<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> PropertyRule<'i> {
  pub(crate) fn parse<'t>(
    name: DashedIdent<'i>,
    input: &mut Parser<'i, 't>,
    loc: Location,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut parser = PropertyRuleDeclarationParser {
      syntax: None,
      inherits: None,
      initial_value: None,
    };

    let mut decl_parser = RuleBodyParser::new(input, &mut parser);
    while let Some(decl) = decl_parser.next() {
      match decl {
        Ok(()) => {}
        Err((e, _)) => return Err(e),
      }
    }

    // `syntax` and `inherits` are always required.
    let parser = decl_parser.parser;
    let syntax = parser
      .syntax
      .clone()
      .ok_or(decl_parser.input.new_custom_error(ParserError::AtRuleBodyInvalid))?;
    let inherits = parser
      .inherits
      .clone()
      .ok_or(decl_parser.input.new_custom_error(ParserError::AtRuleBodyInvalid))?;

    // `initial-value` is required unless the syntax is a universal definition.
    let initial_value = match syntax {
      SyntaxString::Universal => match parser.initial_value {
        None => None,
        Some(val) => {
          let mut input = ParserInput::new(val);
          let mut parser = Parser::new(&mut input);

          if parser.is_exhausted() {
            Some(ParsedComponent::TokenList(TokenList(vec![])))
          } else {
            Some(syntax.parse_value(&mut parser)?)
          }
        }
      },
      _ => {
        let val = parser
          .initial_value
          .ok_or(input.new_custom_error(ParserError::AtRuleBodyInvalid))?;
        let mut input = ParserInput::new(val);
        let mut parser = Parser::new(&mut input);
        Some(syntax.parse_value(&mut parser)?)
      }
    };

    return Ok(PropertyRule {
      name,
      syntax,
      inherits,
      initial_value,
      loc,
    });
  }
}

impl<'i> ToCss for PropertyRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@property ")?;
    self.name.to_css(dest)?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    dest.newline()?;

    dest.write_str("syntax:")?;
    dest.whitespace()?;
    self.syntax.to_css(dest)?;
    dest.write_char(';')?;
    dest.newline()?;

    dest.write_str("inherits:")?;
    dest.whitespace()?;
    match self.inherits {
      true => dest.write_str("true")?,
      false => dest.write_str("false")?,
    }

    if let Some(initial_value) = &self.initial_value {
      dest.write_char(';')?;
      dest.newline()?;

      dest.write_str("initial-value:")?;
      dest.whitespace()?;
      initial_value.to_css(dest)?;

      if !dest.minify {
        dest.write_char(';')?;
      }
    }

    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

pub(crate) struct PropertyRuleDeclarationParser<'i> {
  syntax: Option<SyntaxString>,
  inherits: Option<bool>,
  initial_value: Option<&'i str>,
}

impl<'i> cssparser::DeclarationParser<'i> for PropertyRuleDeclarationParser<'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    match_ignore_ascii_case! { &name,
      "syntax" => {
        let syntax = SyntaxString::parse(input)?;
        self.syntax = Some(syntax);
      },
      "inherits" => {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        let inherits = match_ignore_ascii_case! {&*ident,
          "true" => true,
          "false" => false,
          _ => return Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        };
        self.inherits = Some(inherits);
      },
      "initial-value" => {
        // Buffer the value into a string. We will parse it later.
        let start = input.position();
        while input.next().is_ok() {}
        let initial_value = input.slice_from(start);
        self.initial_value = Some(initial_value);
      },
      _ => return Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }

    return Ok(());
  }
}

/// Default methods reject all at rules.
impl<'i> AtRuleParser<'i> for PropertyRuleDeclarationParser<'i> {
  type Prelude = ();
  type AtRule = ();
  type Error = ParserError<'i>;
}

impl<'i> QualifiedRuleParser<'i> for PropertyRuleDeclarationParser<'i> {
  type Prelude = ();
  type QualifiedRule = ();
  type Error = ParserError<'i>;
}

impl<'i> RuleBodyItemParser<'i, (), ParserError<'i>> for PropertyRuleDeclarationParser<'i> {
  fn parse_qualified(&self) -> bool {
    false
  }

  fn parse_declarations(&self) -> bool {
    true
  }
}
