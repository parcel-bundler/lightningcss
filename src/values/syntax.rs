use cssparser::*;
use crate::values;
use crate::error::{ParserError, PrinterError};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::values::number::serialize_integer;
use super::string::CowArcStr;

/// https://drafts.css-houdini.org/css-properties-values-api/#syntax-strings
#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxString {
  Components(Vec<SyntaxComponent>),
  Universal
}

/// https://drafts.css-houdini.org/css-properties-values-api/#syntax-component
#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxComponent {
  kind: SyntaxComponentKind,
  multiplier: Multiplier
}

/// https://drafts.css-houdini.org/css-properties-values-api/#supported-names
#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxComponentKind {
  Length,
  Number,
  Percentage,
  LengthPercentage,
  Color,
  Image,
  Url,
  Integer,
  Angle,
  Time,
  Resolution,
  TransformFunction,
  TransformList,
  CustomIdent,
  Literal(String) // TODO: borrow??
}

/// https://drafts.css-houdini.org/css-properties-values-api/#multipliers
#[derive(Debug, PartialEq, Clone)]
pub enum Multiplier {
  None,
  Space,
  Comma
}

/// A parsed value for a SyntaxComponent.
#[derive(Debug, PartialEq, Clone)]
pub enum ParsedComponent<'i> {
  Length(values::length::Length),
  Number(f32),
  Percentage(values::percentage::Percentage),
  LengthPercentage(values::length::LengthPercentage),
  Color(values::color::CssColor),
  Image(values::image::Image<'i>),
  Url(values::url::Url<'i>),
  Integer(i32),
  Angle(values::angle::Angle),
  Time(values::time::Time),
  Resolution(values::resolution::Resolution),
  TransformFunction(crate::properties::transform::Transform),
  TransformList(crate::properties::transform::TransformList),
  CustomIdent(values::ident::CustomIdent<'i>),
  Literal(CowArcStr<'i>),
  Repeated(Vec<ParsedComponent<'i>>, Multiplier),
  Token(crate::properties::custom::Token<'i>)
}

impl<'i> SyntaxString {
  pub fn parse_string(input: &'i str) -> Result<SyntaxString, ()> {
    // https://drafts.css-houdini.org/css-properties-values-api/#parsing-syntax
    let mut input = input.trim_matches(SPACE_CHARACTERS);
    if input.is_empty() {
      return Err(())
    }

    if input == "*" {
      return Ok(SyntaxString::Universal)
    }

    let mut components = Vec::new();
    loop {
      let component = SyntaxComponent::parse_string(&mut input)?;
      components.push(component);

      input = input.trim_start_matches(SPACE_CHARACTERS);
      if input.is_empty() {
        break
      }
      
      if input.starts_with('|') {
        input = &input[1..];
        continue
      }

      return Err(())
    }

    Ok(SyntaxString::Components(components))
  }

  pub fn parse_value<'t>(&self, input: &mut Parser<'i, 't>) -> Result<ParsedComponent<'i>, ParseError<'i, ParserError<'i>>> {
    match self {
      SyntaxString::Universal => {
        Ok(ParsedComponent::Token(crate::properties::custom::Token::from(input.next()?)))
      }
      SyntaxString::Components(components) => {
        // Loop through each component, and return the first one that parses successfully.
        for component in components {
          let state = input.state();
          let mut parsed = Vec::new();
          loop {
            let value: Result<ParsedComponent<'i>, ParseError<'i, ParserError<'i>>>  =  input.try_parse(|input| {
              Ok(match &component.kind {
                SyntaxComponentKind::Length => ParsedComponent::Length(values::length::Length::parse(input)?),
                SyntaxComponentKind::Number => ParsedComponent::Number(f32::parse(input)?),
                SyntaxComponentKind::Percentage => ParsedComponent::Percentage(values::percentage::Percentage::parse(input)?),
                SyntaxComponentKind::LengthPercentage => ParsedComponent::LengthPercentage(values::length::LengthPercentage::parse(input)?),
                SyntaxComponentKind::Color => ParsedComponent::Color(values::color::CssColor::parse(input)?),
                SyntaxComponentKind::Image => ParsedComponent::Image(values::image::Image::parse(input)?),
                SyntaxComponentKind::Url => ParsedComponent::Url(values::url::Url::parse(input)?),
                SyntaxComponentKind::Integer => ParsedComponent::Integer(input.expect_integer()?),
                SyntaxComponentKind::Angle => ParsedComponent::Angle(values::angle::Angle::parse(input)?),
                SyntaxComponentKind::Time => ParsedComponent::Time(values::time::Time::parse(input)?),
                SyntaxComponentKind::Resolution => ParsedComponent::Resolution(values::resolution::Resolution::parse(input)?),
                SyntaxComponentKind::TransformFunction => ParsedComponent::TransformFunction(crate::properties::transform::Transform::parse(input)?),
                SyntaxComponentKind::TransformList => ParsedComponent::TransformList(crate::properties::transform::TransformList::parse(input)?),
                SyntaxComponentKind::CustomIdent => ParsedComponent::CustomIdent(values::ident::CustomIdent::parse(input)?),
                SyntaxComponentKind::Literal(value) => {
                  let location = input.current_source_location();
                  let ident = input.expect_ident()?;
                  if *ident != &value {
                    return Err(location.new_unexpected_token_error(Token::Ident(ident.clone())))
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
                    return Ok(ParsedComponent::Repeated(parsed, component.multiplier.clone()));
                  }
                },
                Multiplier::Comma => {
                  parsed.push(value);
                  match input.next() {
                    Err(_) => return Ok(ParsedComponent::Repeated(parsed, component.multiplier.clone())),
                    Ok(&Token::Comma) => continue,
                    Ok(_) => break,
                  }
                }
              }
            } else {
              break
            }
          }

          input.reset(&state);
        }

        Err(input.new_error_for_next_token())
      }
    }
  }
}

impl SyntaxComponent {
  fn parse_string(input: &mut &str) -> Result<SyntaxComponent, ()> {
    let kind = SyntaxComponentKind::parse_string(input)?;

    // Pre-multiplied types cannot have multipliers.
    if kind == SyntaxComponentKind::TransformList {
      return Ok(SyntaxComponent {
        kind,
        multiplier: Multiplier::None
      })
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

    Ok(SyntaxComponent {
      kind,
      multiplier
    })
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
      return Err(())
    }
  }
}

#[inline]
fn is_ident_start(c: char) -> bool {
  // https://drafts.csswg.org/css-syntax-3/#ident-start-code-point
  c >= 'A' && c <= 'Z' ||
    c >= 'a' && c <= 'z' ||
    c >= '\u{80}' ||
    c == '_'
}

#[inline]
fn is_name_code_point(c: char) -> bool {
  // https://drafts.csswg.org/css-syntax-3/#ident-code-point
  is_ident_start(c) ||
    c >= '0' && c <= '9' ||
    c == '-'
}

impl<'i> Parse<'i> for SyntaxString {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let string = input.expect_string_cloned()?;
    SyntaxString::parse_string(string.as_ref())
      .map_err(|_| input.new_custom_error(ParserError::InvalidValue))
  }
}

impl ToCss for SyntaxString {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.kind.to_css(dest)?;
    match self.multiplier {
      Multiplier::None => Ok(()),
      Multiplier::Comma => dest.write_char('#'),
      Multiplier::Space => dest.write_char('+')
    }
  }
}

impl ToCss for SyntaxComponentKind {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use SyntaxComponentKind::*;
    let s = match self {
      Length => "<length>",
      Number => "<number>",
      Percentage => "<percentage>",
      LengthPercentage => "<length-percentage>",
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
      Literal(l) => l
    };
    dest.write_str(s)
  }
}

impl<'i> ToCss for ParsedComponent<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use ParsedComponent::*;
    match self {
      Length(v) => v.to_css(dest),
      Number(v) => v.to_css(dest),
      Percentage(v) => v.to_css(dest),
      LengthPercentage(v) => v.to_css(dest),
      Color(v) => v.to_css(dest),
      Image(v) => v.to_css(dest),
      Url(v) => v.to_css(dest),
      Integer(v) => serialize_integer(*v, dest),
      Angle(v) => v.to_css(dest),
      Time(v) => v.to_css(dest),
      Resolution(v) => v.to_css(dest),
      TransformFunction(v) => v.to_css(dest),
      TransformList(v) => v.to_css(dest),
      CustomIdent(v) => v.to_css(dest),
      Literal(v) => {
        serialize_identifier(&v, dest)?;
        Ok(())
      },
      Repeated(components, multiplier) => {
        let mut first = true;
        for component in components {
          if first {
            first = false;
          } else {
            match multiplier {
              Multiplier::Comma => dest.delim(',', false)?,
              Multiplier::Space => dest.write_char(' ')?,
              Multiplier::None => unreachable!()
            }
          }

          component.to_css(dest)?;
        }
        Ok(())
      },
      Token(t) => t.to_css(dest)
    }
  }
}

#[cfg(test)]
mod tests {
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
      ParsedComponent::Literal("foo".into())
    );

    test(
      "foo|<color>+|<integer>",
      "foo",
      ParsedComponent::Literal("foo".into())
    );

    test(
      "foo | <color>+ | <integer>",
      "2",
      ParsedComponent::Integer(2)
    );
    
    test(
      "foo | <color>+ | <integer>",
      "red",
      ParsedComponent::Repeated(vec![
        ParsedComponent::Color(values::color::CssColor(Color::RGBA(RGBA { red: 255, green: 0, blue: 0, alpha: 255 })))
      ], Multiplier::Space)
    );

    test(
      "foo | <color>+ | <integer>",
      "red blue",
      ParsedComponent::Repeated(vec![
        ParsedComponent::Color(values::color::CssColor(Color::RGBA(RGBA { red: 255, green: 0, blue: 0, alpha: 255 }))),
        ParsedComponent::Color(values::color::CssColor(Color::RGBA(RGBA { red: 0, green: 0, blue: 255, alpha: 255 })))
      ], Multiplier::Space)
    );

    error_test(
      "foo | <color>+ | <integer>",
      "2.5"
    );

    error_test(
      "foo | <color>+ | <integer>",
      "25px"
    );

    error_test(
      "foo | <color>+ | <integer>",
      "red, green"
    );

    test(
      "foo | <color># | <integer>",
      "red, blue",
      ParsedComponent::Repeated(vec![
        ParsedComponent::Color(values::color::CssColor(Color::RGBA(RGBA { red: 255, green: 0, blue: 0, alpha: 255 }))),
        ParsedComponent::Color(values::color::CssColor(Color::RGBA(RGBA { red: 0, green: 0, blue: 255, alpha: 255 })))
      ], Multiplier::Comma)
    );

    error_test(
      "foo | <color># | <integer>",
      "red green"
    );

    test(
      "<length>",
      "25px",
      ParsedComponent::Length(values::length::Length::Value(values::length::LengthValue::Px(25.0)))
    );

    test(
      "<length>",
      "calc(25px + 25px)",
      ParsedComponent::Length(values::length::Length::Value(values::length::LengthValue::Px(50.0)))
    );

    test(
      "<length> | <percentage>",
      "25px",
      ParsedComponent::Length(values::length::Length::Value(values::length::LengthValue::Px(25.0)))
    );

    test(
      "<length> | <percentage>",
      "25%",
      ParsedComponent::Percentage(values::percentage::Percentage(0.25))
    );

    error_test(
      "<length> | <percentage>",
      "calc(100% - 25px)"
    );

    test(
      "foo | bar | baz",
      "bar",
      ParsedComponent::Literal("bar".into())
    );

    test(
      "<custom-ident>",
      "hi",
      ParsedComponent::CustomIdent(values::ident::CustomIdent("hi".into()))
    );

    parse_error_test("<transform-list>#");
    parse_error_test("<color");
    parse_error_test("color>");
  }
}
