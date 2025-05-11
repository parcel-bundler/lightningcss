use cssparser::*;
use lightningcss::{
  declaration::DeclarationBlock,
  error::{ParserError, PrinterError},
  printer::Printer,
  stylesheet::{ParserOptions, PrinterOptions, StyleSheet},
  traits::{AtRuleParser, Parse, ToCss},
  values::ident::Ident,
};

fn minify_test(source: &str, expected: &str) {
  let mut stylesheet = StyleSheet::parse_with(&source, ParserOptions::default(), &mut TestAtRuleParser).unwrap();
  stylesheet.minify(Default::default()).unwrap();
  let res = stylesheet
    .to_css(PrinterOptions {
      minify: true,
      ..PrinterOptions::default()
    })
    .unwrap();
  assert_eq!(res.code, expected);
}

#[test]
fn test_block() {
  minify_test(
    r#"
    @block test {
      color: yellow;
    }
  "#,
    "@block test{color:#ff0}",
  )
}

#[test]
fn test_inline() {
  minify_test(
    r#"
    @inline test;
    .foo {
      color: yellow;
    }
  "#,
    "@inline test;.foo{color:#ff0}",
  )
}

enum Prelude<'i> {
  Block(Ident<'i>),
  Inline(Ident<'i>),
}

#[derive(Debug, Clone)]
enum AtRule<'i> {
  Block(BlockRule<'i>),
  Inline(InlineRule<'i>),
}

#[derive(Debug, Clone)]
struct BlockRule<'i> {
  name: Ident<'i>,
  declarations: DeclarationBlock<'i>,
}

#[derive(Debug, Clone)]
struct InlineRule<'i> {
  name: Ident<'i>,
}

#[derive(Default)]
struct TestAtRuleParser;
impl<'i> AtRuleParser<'i> for TestAtRuleParser {
  type Prelude = Prelude<'i>;
  type Error = ParserError<'i>;
  type AtRule = AtRule<'i>;

  fn parse_prelude<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
    _options: &ParserOptions<'_, 'i>,
  ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
    let location = input.current_source_location();
    match_ignore_ascii_case! {&*name,
      "block" => {
        let name = Ident::parse(input)?;
        Ok(Prelude::Block(name))
      },
      "inline" => {
        let name = Ident::parse(input)?;
        Ok(Prelude::Inline(name))
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(name.clone())
      ))
    }
  }

  fn rule_without_block(
    &mut self,
    prelude: Self::Prelude,
    _start: &ParserState,
    _options: &ParserOptions<'_, 'i>,
    _is_nested: bool,
  ) -> Result<Self::AtRule, ()> {
    match prelude {
      Prelude::Inline(name) => Ok(AtRule::Inline(InlineRule { name })),
      _ => unreachable!(),
    }
  }

  fn parse_block<'t>(
    &mut self,
    prelude: Self::Prelude,
    _start: &ParserState,
    input: &mut Parser<'i, 't>,
    _options: &ParserOptions<'_, 'i>,
    _is_nested: bool,
  ) -> Result<Self::AtRule, ParseError<'i, Self::Error>> {
    match prelude {
      Prelude::Block(name) => Ok(AtRule::Block(BlockRule {
        name,
        declarations: DeclarationBlock::parse(input, &ParserOptions::default())?,
      })),
      _ => unreachable!(),
    }
  }
}

impl<'i> ToCss for AtRule<'i> {
  fn to_css<W: std::fmt::Write>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> {
    match self {
      AtRule::Block(rule) => {
        dest.write_str("@block ")?;
        rule.name.to_css(dest)?;
        rule.declarations.to_css_block(dest)
      }
      AtRule::Inline(rule) => {
        dest.write_str("@inline ")?;
        rule.name.to_css(dest)?;
        dest.write_char(';')
      }
    }
  }
}
