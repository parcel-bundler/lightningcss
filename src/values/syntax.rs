//! CSS syntax strings

use super::ident::Ident;
use super::number::{CSSInteger, CSSNumber};
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::properties::custom::TokenList;
use crate::stylesheet::ParserOptions;
use crate::traits::{Parse, ToCss};
use crate::values;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A CSS [syntax string](https://drafts.css-houdini.org/css-properties-values-api/#syntax-strings)
/// used to define the grammar for a registered custom property.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum SyntaxString {
  /// A list of syntax components.
  Components(Vec<SyntaxComponent>),
  /// The universal syntax definition.
  Universal,
}

/// A [syntax component](https://drafts.css-houdini.org/css-properties-values-api/#syntax-component)
/// within a [SyntaxString](SyntaxString).
///
/// A syntax component consists of a component kind an a multiplier, which indicates how the component
/// may repeat during parsing.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct SyntaxComponent {
  /// The kind of component.
  pub kind: SyntaxComponentKind,
  /// A multiplier for the component.
  pub multiplier: Multiplier,
}

/// A [syntax component component name](https://drafts.css-houdini.org/css-properties-values-api/#supported-names).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum SyntaxComponentKind {
  /// A `<length>` component.
  Length,
  /// A `<number>` component.
  Number,
  /// A `<percentage>` component.
  Percentage,
  /// A `<length-percentage>` component.
  LengthPercentage,
  /// A `<string>` component.
  String,
  /// A `<color>` component.
  Color,
  /// An `<image>` component.
  Image,
  /// A `<url>` component.
  Url,
  /// An `<integer>` component.
  Integer,
  /// An `<angle>` component.
  Angle,
  /// A `<time>` component.
  Time,
  /// A `<resolution>` component.
  Resolution,
  /// A `<transform-function>` component.
  TransformFunction,
  /// A `<transform-list>` component.
  TransformList,
  /// A `<custom-ident>` component.
  CustomIdent,
  /// A literal component.
  Literal(String), // TODO: borrow??
}

/// A [multiplier](https://drafts.css-houdini.org/css-properties-values-api/#multipliers) for a
/// [SyntaxComponent](SyntaxComponent). Indicates whether and how the component may be repeated.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum Multiplier {
  /// The component may not be repeated.
  None,
  /// The component may repeat one or more times, separated by spaces.
  Space,
  /// The component may repeat one or more times, separated by commas.
  Comma,
}

/// A parsed value for a [SyntaxComponent](SyntaxComponent).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ParsedComponent<'i> {
  /// A `<length>` value.
  Length(values::length::Length),
  /// A `<number>` value.
  Number(CSSNumber),
  /// A `<percentage>` value.
  Percentage(values::percentage::Percentage),
  /// A `<length-percentage>` value.
  LengthPercentage(values::length::LengthPercentage),
  /// A `<string>` value.
  String(values::string::CSSString<'i>),
  /// A `<color>` value.
  Color(values::color::CssColor),
  /// An `<image>` value.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Image(values::image::Image<'i>),
  /// A `<url>` value.
  Url(values::url::Url<'i>),
  /// An `<integer>` value.
  Integer(CSSInteger),
  /// An `<angle>` value.
  Angle(values::angle::Angle),
  /// A `<time>` value.
  Time(values::time::Time),
  /// A `<resolution>` value.
  Resolution(values::resolution::Resolution),
  /// A `<transform-function>` value.
  TransformFunction(crate::properties::transform::Transform),
  /// A `<transform-list>` value.
  TransformList(crate::properties::transform::TransformList),
  /// A `<custom-ident>` value.
  CustomIdent(values::ident::CustomIdent<'i>),
  /// A literal value.
  Literal(Ident<'i>),
  /// A repeated component value.
  Repeated {
    /// The components to repeat.
    #[cfg_attr(feature = "visitor", skip_type)]
    components: Vec<ParsedComponent<'i>>,
    /// A multiplier describing how the components repeat.
    multiplier: Multiplier,
  },
  /// A raw token stream.
  TokenList(crate::properties::custom::TokenList<'i>),
}

impl<'i> SyntaxString {
  /// Parses a syntax string.
  pub fn parse_string(input: &'i str) -> Result<SyntaxString, ()> {
    // https://drafts.css-houdini.org/css-properties-values-api/#parsing-syntax
    let mut input = input.trim_matches(SPACE_CHARACTERS);
    if input.is_empty() {
      return Err(());
    }

    if input == "*" {
      return Ok(SyntaxString::Universal);
    }

    let mut components = Vec::new();
    loop {
      let component = SyntaxComponent::parse_string(&mut input)?;
      components.push(component);

      input = input.trim_start_matches(SPACE_CHARACTERS);
      if input.is_empty() {
        break;
      }

      if input.starts_with('|') {
        input = &input[1..];
        continue;
      }

      return Err(());
    }

    Ok(SyntaxString::Components(components))
  }

  /// Parses a value according to the syntax grammar.
  pub fn parse_value<'t>(
    &self,
    input: &mut Parser<'i, 't>,
  ) -> Result<ParsedComponent<'i>, ParseError<'i, ParserError<'i>>> {
    match self {
      SyntaxString::Universal => Ok(ParsedComponent::TokenList(TokenList::parse(
        input,
        &ParserOptions::default(),
        0,
      )?)),
      SyntaxString::Components(components) => {
        // Loop through each component, and return the first one that parses successfully.
        for component in components {
          let state = input.state();
          let mut parsed = Vec::new();
          loop {
            let value: Result<ParsedComponent<'i>, ParseError<'i, ParserError<'i>>> = input.try_parse(|input| {
              Ok(match &component.kind {
                SyntaxComponentKind::Length => ParsedComponent::Length(values::length::Length::parse(input)?),
                SyntaxComponentKind::Number => ParsedComponent::Number(CSSNumber::parse(input)?),
                SyntaxComponentKind::Percentage => {
                  ParsedComponent::Percentage(values::percentage::Percentage::parse(input)?)
                }
                SyntaxComponentKind::LengthPercentage => {
                  ParsedComponent::LengthPercentage(values::length::LengthPercentage::parse(input)?)
                }
                SyntaxComponentKind::String => ParsedComponent::String(values::string::CSSString::parse(input)?),
                SyntaxComponentKind::Color => ParsedComponent::Color(values::color::CssColor::parse(input)?),
                SyntaxComponentKind::Image => ParsedComponent::Image(values::image::Image::parse(input)?),
                SyntaxComponentKind::Url => ParsedComponent::Url(values::url::Url::parse(input)?),
                SyntaxComponentKind::Integer => ParsedComponent::Integer(CSSInteger::parse(input)?),
                SyntaxComponentKind::Angle => ParsedComponent::Angle(values::angle::Angle::parse(input)?),
                SyntaxComponentKind::Time => ParsedComponent::Time(values::time::Time::parse(input)?),
                SyntaxComponentKind::Resolution => {
                  ParsedComponent::Resolution(values::resolution::Resolution::parse(input)?)
                }
                SyntaxComponentKind::TransformFunction => {
                  ParsedComponent::TransformFunction(crate::properties::transform::Transform::parse(input)?)
                }
                SyntaxComponentKind::TransformList => {
                  ParsedComponent::TransformList(crate::properties::transform::TransformList::parse(input)?)
                }
                SyntaxComponentKind::CustomIdent => {
                  ParsedComponent::CustomIdent(values::ident::CustomIdent::parse(input)?)
                }
                SyntaxComponentKind::Literal(value) => {
                  let location = input.current_source_location();
                  let ident = input.expect_ident()?;
                  if *ident != &value {
                    return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())));
                  }
                  ParsedComponent::Literal(ident.into())
                }
              })
            });

            if let Ok(value) = value {
              match component.multiplier {
                Multiplier::None => return Ok(value),
                Multiplier::Space => {
                  parsed.push(value);
                  if input.is_exhausted() {
                    return Ok(ParsedComponent::Repeated {
                      components: parsed,
                      multiplier: component.multiplier.clone(),
                    });
                  }
                }
                Multiplier::Comma => {
                  parsed.push(value);
                  match input.next() {
                    Err(_) => {
                      return Ok(ParsedComponent::Repeated {
                        components: parsed,
                        multiplier: component.multiplier.clone(),
                      })
                    }
                    Ok(&Token::Comma) => continue,
                    Ok(_) => break,
                  }
                }
              }
            } else {
              break;
            }
          }

          input.reset(&state);
        }

        Err(input.new_error_for_next_token())
      }
    }
  }

  /// Parses a value from a string according to the syntax grammar.
  pub fn parse_value_from_string<'t>(
    &self,
    input: &'i str,
  ) -> Result<ParsedComponent<'i>, ParseError<'i, ParserError<'i>>> {
    let mut input = ParserInput::new(input);
    let mut parser = Parser::new(&mut input);
    self.parse_value(&mut parser)
  }
}

impl SyntaxComponent {
  fn parse_string(input: &mut &str) -> Result<SyntaxComponent, ()> {
    let kind = SyntaxComponentKind::parse_string(input)?;

    // Pre-multiplied types cannot have multipliers.
    if kind == SyntaxComponentKind::TransformList {
      return Ok(SyntaxComponent {
        kind,
        multiplier: Multiplier::None,
      });
    }

    let multiplier = if input.starts_with('+') {
      *input = &input[1..];
      Multiplier::Space
    } else if input.starts_with('#') {
      *input = &input[1..];
      Multiplier::Comma
    } else {
      Multiplier::None
    };

    Ok(SyntaxComponent { kind, multiplier })
  }
}

// https://drafts.csswg.org/css-syntax-3/#whitespace
static SPACE_CHARACTERS: &'static [char] = &['\u{0020}', '\u{0009}'];

impl SyntaxComponentKind {
  fn parse_string(input: &mut &str) -> Result<SyntaxComponentKind, ()> {
    // https://drafts.css-houdini.org/css-properties-values-api/#consume-syntax-component
    *input = input.trim_start_matches(SPACE_CHARACTERS);
    if input.starts_with('<') {
      // https://drafts.css-houdini.org/css-properties-values-api/#consume-data-type-name
      let end_idx = input.find('>').ok_or(())?;
      let name = &input[1..end_idx];
      let component = match_ignore_ascii_case! {name,
        "length" => SyntaxComponentKind::Length,
        "number" => SyntaxComponentKind::Number,
        "percentage" => SyntaxComponentKind::Percentage,
        "length-percentage" => SyntaxComponentKind::LengthPercentage,
        "string" => SyntaxComponentKind::String,
        "color" => SyntaxComponentKind::Color,
        "image" => SyntaxComponentKind::Image,
        "url" => SyntaxComponentKind::Url,
        "integer" => SyntaxComponentKind::Integer,
        "angle" => SyntaxComponentKind::Angle,
        "time" => SyntaxComponentKind::Time,
        "resolution" => SyntaxComponentKind::Resolution,
        "transform-function" => SyntaxComponentKind::TransformFunction,
        "transform-list" => SyntaxComponentKind::TransformList,
        "custom-ident" => SyntaxComponentKind::CustomIdent,
        _ => return Err(())
      };

      *input = &input[end_idx + 1..];
      Ok(component)
    } else if input.starts_with(is_ident_start) {
      // A literal.
      let end_idx = input.find(|c| !is_name_code_point(c)).unwrap_or_else(|| input.len());
      let name = input[0..end_idx].to_owned();
      *input = &input[end_idx..];
      Ok(SyntaxComponentKind::Literal(name))
    } else {
      return Err(());
    }
  }
}

#[inline]
fn is_ident_start(c: char) -> bool {
  // https://drafts.csswg.org/css-syntax-3/#ident-start-code-point
  c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '\u{80}' || c == '_'
}

#[inline]
fn is_name_code_point(c: char) -> bool {
  // https://drafts.csswg.org/css-syntax-3/#ident-code-point
  is_ident_start(c) || c >= '0' && c <= '9' || c == '-'
}

impl<'i> Parse<'i> for SyntaxString {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let string = input.expect_string_cloned()?;
    SyntaxString::parse_string(string.as_ref()).map_err(|_| input.new_custom_error(ParserError::InvalidValue))
  }
}

impl ToCss for SyntaxString {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_char('"')?;
    match self {
      SyntaxString::Universal => dest.write_char('*')?,
      SyntaxString::Components(components) => {
        let mut first = true;
        for component in components {
          if first {
            first = false;
          } else {
            dest.delim('|', true)?;
          }

          component.to_css(dest)?;
        }
      }
    }

    dest.write_char('"')
  }
}

impl ToCss for SyntaxComponent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.kind.to_css(dest)?;
    match self.multiplier {
      Multiplier::None => Ok(()),
      Multiplier::Comma => dest.write_char('#'),
      Multiplier::Space => dest.write_char('+'),
    }
  }
}

impl ToCss for SyntaxComponentKind {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use SyntaxComponentKind::*;
    let s = match self {
      Length => "<length>",
      Number => "<number>",
      Percentage => "<percentage>",
      LengthPercentage => "<length-percentage>",
      String => "<string>",
      Color => "<color>",
      Image => "<image>",
      Url => "<url>",
      Integer => "<integer>",
      Angle => "<angle>",
      Time => "<time>",
      Resolution => "<resolution>",
      TransformFunction => "<transform-function>",
      TransformList => "<transform-list>",
      CustomIdent => "<custom-ident>",
      Literal(l) => l,
    };
    dest.write_str(s)
  }
}

impl<'i> ToCss for ParsedComponent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use ParsedComponent::*;
    match self {
      Length(v) => v.to_css(dest),
      Number(v) => v.to_css(dest),
      Percentage(v) => v.to_css(dest),
      LengthPercentage(v) => v.to_css(dest),
      String(v) => v.to_css(dest),
      Color(v) => v.to_css(dest),
      Image(v) => v.to_css(dest),
      Url(v) => v.to_css(dest),
      Integer(v) => v.to_css(dest),
      Angle(v) => v.to_css(dest),
      Time(v) => v.to_css(dest),
      Resolution(v) => v.to_css(dest),
      TransformFunction(v) => v.to_css(dest),
      TransformList(v) => v.to_css(dest),
      CustomIdent(v) => v.to_css(dest),
      Literal(v) => v.to_css(dest),
      Repeated { components, multiplier } => {
        let mut first = true;
        for component in components {
          if first {
            first = false;
          } else {
            match multiplier {
              Multiplier::Comma => dest.delim(',', false)?,
              Multiplier::Space => dest.write_char(' ')?,
              Multiplier::None => unreachable!(),
            }
          }

          component.to_css(dest)?;
        }
        Ok(())
      }
      TokenList(t) => t.to_css(dest, false),
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::values::color::RGBA;

  use super::*;

  fn test(source: &str, test: &str, expected: ParsedComponent) {
    let parsed = SyntaxString::parse_string(source).unwrap();

    let mut input = ParserInput::new(test);
    let mut parser = Parser::new(&mut input);
    let value = parsed.parse_value(&mut parser).unwrap();
    assert_eq!(value, expected);
  }

  fn parse_error_test(source: &str) {
    let res = SyntaxString::parse_string(source);
    match res {
      Ok(_) => unreachable!(),
      Err(_) => {}
    }
  }

  fn error_test(source: &str, test: &str) {
    let parsed = SyntaxString::parse_string(source).unwrap();
    let mut input = ParserInput::new(test);
    let mut parser = Parser::new(&mut input);
    let res = parsed.parse_value(&mut parser);
    match res {
      Ok(_) => unreachable!(),
      Err(_) => {}
    }
  }

  #[test]
  fn test_syntax() {
    test(
      "foo | <color>+ | <integer>",
      "foo",
      ParsedComponent::Literal("foo".into()),
    );

    test("foo|<color>+|<integer>", "foo", ParsedComponent::Literal("foo".into()));

    test("foo | <color>+ | <integer>", "2", ParsedComponent::Integer(2));

    test(
      "foo | <color>+ | <integer>",
      "red",
      ParsedComponent::Repeated {
        components: vec![ParsedComponent::Color(values::color::CssColor::RGBA(RGBA {
          red: 255,
          green: 0,
          blue: 0,
          alpha: 255,
        }))],
        multiplier: Multiplier::Space,
      },
    );

    test(
      "foo | <color>+ | <integer>",
      "red blue",
      ParsedComponent::Repeated {
        components: vec![
          ParsedComponent::Color(values::color::CssColor::RGBA(RGBA {
            red: 255,
            green: 0,
            blue: 0,
            alpha: 255,
          })),
          ParsedComponent::Color(values::color::CssColor::RGBA(RGBA {
            red: 0,
            green: 0,
            blue: 255,
            alpha: 255,
          })),
        ],
        multiplier: Multiplier::Space,
      },
    );

    error_test("foo | <color>+ | <integer>", "2.5");

    error_test("foo | <color>+ | <integer>", "25px");

    error_test("foo | <color>+ | <integer>", "red, green");

    test(
      "foo | <color># | <integer>",
      "red, blue",
      ParsedComponent::Repeated {
        components: vec![
          ParsedComponent::Color(values::color::CssColor::RGBA(RGBA {
            red: 255,
            green: 0,
            blue: 0,
            alpha: 255,
          })),
          ParsedComponent::Color(values::color::CssColor::RGBA(RGBA {
            red: 0,
            green: 0,
            blue: 255,
            alpha: 255,
          })),
        ],
        multiplier: Multiplier::Comma,
      },
    );

    error_test("foo | <color># | <integer>", "red green");

    test(
      "<length>",
      "25px",
      ParsedComponent::Length(values::length::Length::Value(values::length::LengthValue::Px(25.0))),
    );

    test(
      "<length>",
      "calc(25px + 25px)",
      ParsedComponent::Length(values::length::Length::Value(values::length::LengthValue::Px(50.0))),
    );

    test(
      "<length> | <percentage>",
      "25px",
      ParsedComponent::Length(values::length::Length::Value(values::length::LengthValue::Px(25.0))),
    );

    test(
      "<length> | <percentage>",
      "25%",
      ParsedComponent::Percentage(values::percentage::Percentage(0.25)),
    );

    error_test("<length> | <percentage>", "calc(100% - 25px)");

    test("foo | bar | baz", "bar", ParsedComponent::Literal("bar".into()));

    test(
      "<string>",
      "'foo'",
      ParsedComponent::String(values::string::CSSString("foo".into())),
    );

    test(
      "<custom-ident>",
      "hi",
      ParsedComponent::CustomIdent(values::ident::CustomIdent("hi".into())),
    );

    parse_error_test("<transform-list>#");
    parse_error_test("<color");
    parse_error_test("color>");
  }
}
