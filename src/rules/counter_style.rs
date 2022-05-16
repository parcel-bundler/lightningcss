//! The `@counter-style` rule.
#![allow(missing_docs)]

use cssparser::*;

use super::Location;
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::properties::custom::CustomProperty;
use crate::properties::list::PredefinedCounterStyle;
use crate::traits::{Parse, ToCss};
use crate::values::ident::CustomIdent;
use crate::values::image::Image;
use crate::values::number::CSSInteger;
use crate::values::string::CowArcStr;

/// A [@counter-style](https://drafts.csswg.org/css-counter-styles/#the-counter-style-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CounterStyleRule<'i> {
  /// The name of the counter style to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: CustomIdent<'i>,
  /// Declarations in the `@counter-style` rule.
  pub declarations: Vec<CounterStyleProperty<'i>>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> CounterStyleRule<'i> {
  pub(crate) fn parse<'t>(
    name: CustomIdent<'i>,
    input: &mut Parser<'i, 't>,
    loc: Location,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut parser = DeclarationListParser::new(input, CounterStyleDeclarationParser);
    let mut declarations = vec![];
    while let Some(decl) = parser.next() {
      if let Ok(decl) = decl {
        declarations.push(decl);
      }
    }

    Ok(CounterStyleRule {
      name,
      declarations,
      loc,
    })
  }
}

/// A property within a `@counter-style` rule.
///
///  See [CounterStyleRule](CounterStyleRule).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "property", content = "value", rename_all = "kebab-case")
)]
pub enum CounterStyleProperty<'i> {
  /// The `system` property.
  #[cfg_attr(feature = "serde", serde(borrow))]
  System(System<'i>),
  Negative(Negative<'i>),
  Prefix(Symbol<'i>),
  Suffix(Symbol<'i>),
  Range(CounterStyleRange),
  Pad(Pad<'i>),
  Fallback(CounterStyleName<'i>),
  Symbols(Symbols<'i>),
  AdditiveSymbols(Vec<AdditiveSymbol<'i>>),
  SpeakAs(SpeakAs<'i>),
  Custom(CustomProperty<'i>),
}

pub(crate) struct CounterStyleDeclarationParser;

impl<'i> cssparser::DeclarationParser<'i> for CounterStyleDeclarationParser {
  type Declaration = CounterStyleProperty<'i>;
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    let state = input.state();
    match_ignore_ascii_case! { &name,
      "system" => {
        if let Ok(system) = System::parse(input) {
          return Ok(CounterStyleProperty::System(system))
        }
      },
      "negative" => {
        if let Ok(negative) = Negative::parse(input) {
          return Ok(CounterStyleProperty::Negative(negative))
        }
      },
      "prefix" => {
        if let Ok(prefix) = Symbol::parse(input) {
          return Ok(CounterStyleProperty::Prefix(prefix))
        }
      },
      "suffix" => {
        if let Ok(suffix) = Symbol::parse(input) {
          return Ok(CounterStyleProperty::Suffix(suffix))
        }
      },
      "range" => {
        if let Ok(range) = CounterStyleRange::parse(input) {
          return Ok(CounterStyleProperty::Range(range))
        }
      },
      "pad" => {
        if let Ok(pad) = Pad::parse(input) {
          return Ok(CounterStyleProperty::Pad(pad))
        }
      },
      "fallback" => {
        if let Ok(name) = CounterStyleName::parse(input) {
          return Ok(CounterStyleProperty::Fallback(name))
        }
      },
      "symbols" => {
        if let Ok(symbols) = Symbols::parse(input) {
          return Ok(CounterStyleProperty::Symbols(symbols))
        }
      },
      "additive-symbols" => {
        if let Ok(symbols) = Vec::<AdditiveSymbol>::parse(input) {
          return Ok(CounterStyleProperty::AdditiveSymbols(symbols))
        }
      },
      "speak-as" => {
        if let Ok(speak_as) = SpeakAs::parse(input) {
          return Ok(CounterStyleProperty::SpeakAs(speak_as))
        }
      },
      _ => return Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }

    input.reset(&state);
    return Ok(CounterStyleProperty::Custom(CustomProperty::parse(name.into(), input)?));
  }
}

/// Default methods reject all at rules.
impl<'i> AtRuleParser<'i> for CounterStyleDeclarationParser {
  type Prelude = ();
  type AtRule = CounterStyleProperty<'i>;
  type Error = ParserError<'i>;
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum System<'i> {
  Cyclic,
  Numeric,
  Alphabetic,
  Symbolic,
  Additive,
  Fixed(CSSInteger),
  #[cfg_attr(feature = "serde", serde(borrow))]
  Extends(CounterStyleName<'i>),
}

impl<'i> Parse<'i> for System<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "cyclic" => Ok(System::Cyclic),
      "numeric" => Ok(System::Numeric),
      "alphabetic" => Ok(System::Alphabetic),
      "symbolic" => Ok(System::Symbolic),
      "additive" => Ok(System::Additive),
      "fixed" => {
        // https://drafts.csswg.org/css-counter-styles-3/#fixed-system
        let first_symbol_value = input.try_parse(CSSInteger::parse).unwrap_or(1);
        Ok(System::Fixed(first_symbol_value))
      },
      "extends" => {
        // https://drafts.csswg.org/css-counter-styles-3/#extends-system
        let name = CounterStyleName::parse(input)?;
        Ok(System::Extends(name))
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl<'i> Default for System<'i> {
  fn default() -> Self {
    System::Symbolic
  }
}

impl<'i> ToCss for System<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      System::Cyclic => dest.write_str("cyclic"),
      System::Numeric => dest.write_str("numeric"),
      System::Alphabetic => dest.write_str("alphabetic"),
      System::Symbolic => dest.write_str("symbolic"),
      System::Additive => dest.write_str("additive"),
      System::Fixed(first_symbol_value) => {
        dest.write_str("fixed")?;
        if *first_symbol_value != 1 {
          dest.write_char(' ')?;
          first_symbol_value.to_css(dest)?;
        }
        Ok(())
      }
      System::Extends(name) => {
        dest.write_str("extends ")?;
        name.to_css(dest)
      }
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum CounterStyleName<'i> {
  /// A predefined counter style name.
  Predefined(PredefinedCounterStyle),
  /// A custom counter style name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Name(CustomIdent<'i>),
}

impl<'i> Parse<'i> for CounterStyleName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(predefined) = input.try_parse(PredefinedCounterStyle::parse) {
      return Ok(CounterStyleName::Predefined(predefined));
    }

    let name = CustomIdent::parse(input)?;
    Ok(CounterStyleName::Name(name))
  }
}

impl<'i> ToCss for CounterStyleName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      CounterStyleName::Predefined(predefined) => predefined.to_css(dest),
      CounterStyleName::Name(name) => name.to_css(dest),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Negative<'i> {
  #[cfg_attr(feature = "serde", serde(borrow))]
  prepend: Symbol<'i>,
  append: Option<Symbol<'i>>,
}

impl<'i> Parse<'i> for Negative<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let prepend = Symbol::parse(input)?;
    let append = input.try_parse(Symbol::parse).ok();
    Ok(Negative { prepend, append })
  }
}

impl<'i> Default for Negative<'i> {
  fn default() -> Self {
    Negative {
      prepend: Symbol::String("-".into()),
      append: None,
    }
  }
}

impl<'i> ToCss for Negative<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.prepend.to_css(dest)?;
    if let Some(append) = &self.append {
      dest.write_char(' ')?;
      append.to_css(dest)?;
    }
    Ok(())
  }
}

/// A single [symbol](https://drafts.csswg.org/css-counter-styles-3/#typedef-symbol) as used in the
/// `@counter-style` rule.
///
/// See [CounterStyleRule](CounterStyleRule).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Symbol<'i> {
  /// A string.
  #[cfg_attr(feature = "serde", serde(borrow))]
  String(CowArcStr<'i>),
  /// An image.
  Image(Image<'i>),
  /// An identifier.
  Ident(CustomIdent<'i>),
}

impl<'i> Parse<'i> for Symbol<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(img) = input.try_parse(Image::parse) {
      return Ok(Symbol::Image(img));
    }

    if let Ok(ident) = input.try_parse(CustomIdent::parse) {
      return Ok(Symbol::Ident(ident));
    }

    let s = input.expect_string_cloned()?;
    Ok(Symbol::String(s.into()))
  }
}

impl<'i> ToCss for Symbol<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Symbol::String(s) => {
        serialize_string(&s, dest)?;
        Ok(())
      }
      Symbol::Image(img) => img.to_css(dest),
      Symbol::Ident(ident) => ident.to_css(dest),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Symbols<'i>(#[cfg_attr(feature = "serde", serde(borrow))] Vec<Symbol<'i>>);

impl<'i> Parse<'i> for Symbols<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut symbols = Vec::new();
    while let Ok(s) = input.try_parse(Symbol::parse) {
      symbols.push(s);
    }
    if symbols.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }
    Ok(Symbols(symbols))
  }
}

impl<'i> ToCss for Symbols<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    for symbol in &self.0 {
      if first {
        first = false;
      } else {
        dest.write_char(' ')?;
      }
      symbol.to_css(dest)?;
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum CounterStyleRange {
  Auto,
  Ranges(Vec<CounterRange>),
}

impl<'i> Parse<'i> for CounterStyleRange {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(CounterStyleRange::Auto);
    }

    let ranges = input.parse_comma_separated(CounterRange::parse)?;
    Ok(CounterStyleRange::Ranges(ranges))
  }
}

impl ToCss for CounterStyleRange {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      CounterStyleRange::Auto => dest.write_str("auto"),
      CounterStyleRange::Ranges(ranges) => ranges.to_css(dest),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CounterRange {
  start: CounterRangeBound,
  end: CounterRangeBound,
}

impl<'i> Parse<'i> for CounterRange {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let start = CounterRangeBound::parse(input)?;
    let end = CounterRangeBound::parse(input)?;
    if let (CounterRangeBound::Integer(start), CounterRangeBound::Integer(end)) = (&start, &end) {
      if start > end {
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      }
    }

    Ok(CounterRange { start, end })
  }
}

impl ToCss for CounterRange {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.start.to_css(dest)?;
    dest.write_char(' ')?;
    self.end.to_css(dest)
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum CounterRangeBound {
  Integer(CSSInteger),
  Infinite,
}

impl<'i> Parse<'i> for CounterRangeBound {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(integer) = input.try_parse(CSSInteger::parse) {
      return Ok(CounterRangeBound::Integer(integer));
    }
    input.expect_ident_matching("infinite")?;
    Ok(CounterRangeBound::Infinite)
  }
}

impl ToCss for CounterRangeBound {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      CounterRangeBound::Infinite => dest.write_str("infinite"),
      CounterRangeBound::Integer(v) => v.to_css(dest),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Pad<'i> {
  min_length: CSSInteger,
  #[cfg_attr(feature = "serde", serde(borrow))]
  symbol: Symbol<'i>,
}

impl<'i> Parse<'i> for Pad<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let symbol = input.try_parse(Symbol::parse);
    let min_length = CSSInteger::parse(input)?;
    let symbol = symbol.or_else(|_| Symbol::parse(input))?;
    Ok(Pad { min_length, symbol })
  }
}

impl<'i> Default for Pad<'i> {
  fn default() -> Self {
    Pad {
      min_length: 0,
      symbol: Symbol::String("".into()),
    }
  }
}

impl<'i> ToCss for Pad<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.min_length.to_css(dest)?;
    dest.write_char(' ')?;
    self.symbol.to_css(dest)
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AdditiveSymbol<'i> {
  weight: CSSInteger,
  #[cfg_attr(feature = "serde", serde(borrow))]
  symbol: Symbol<'i>,
}

impl<'i> Parse<'i> for AdditiveSymbol<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let symbol = input.try_parse(Symbol::parse);
    let weight = CSSInteger::parse(input)?;
    let symbol = symbol.or_else(|_| Symbol::parse(input))?;
    Ok(AdditiveSymbol { weight, symbol })
  }
}

impl<'i> ToCss for AdditiveSymbol<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.weight.to_css(dest)?;
    dest.write_char(' ')?;
    self.symbol.to_css(dest)
  }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum SpeakAs<'i> {
  Auto,
  Bullets,
  Numbers,
  Words,
  SpellOut,
  #[cfg_attr(feature = "serde", serde(borrow))]
  Custom(CounterStyleName<'i>),
}

impl<'i> Parse<'i> for SpeakAs<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let res = input.try_parse(|input| -> Result<Self, ParseError<'i, ParserError<'i>>> {
      let location = input.current_source_location();
      let ident = input.expect_ident()?;
      match_ignore_ascii_case! { &*ident,
        "auto" => Ok(SpeakAs::Auto),
        "bullets" => Ok(SpeakAs::Bullets),
        "numbers" => Ok(SpeakAs::Numbers),
        "words" => Ok(SpeakAs::Words),
        "spell-out" => Ok(SpeakAs::SpellOut),
        _ => Err(location.new_unexpected_token_error(
          cssparser::Token::Ident(ident.clone())
        ))
      }
    });

    if let Ok(res) = res {
      return Ok(res);
    }

    let custom = CounterStyleName::parse(input)?;
    Ok(SpeakAs::Custom(custom))
  }
}

impl<'i> Default for SpeakAs<'i> {
  fn default() -> Self {
    SpeakAs::Auto
  }
}

impl<'i> ToCss for SpeakAs<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      SpeakAs::Auto => dest.write_str("auto"),
      SpeakAs::Bullets => dest.write_str("bullets"),
      SpeakAs::Numbers => dest.write_str("numbers"),
      SpeakAs::Words => dest.write_str("words"),
      SpeakAs::SpellOut => dest.write_str("spell-out"),
      SpeakAs::Custom(custom) => custom.to_css(dest),
    }
  }
}

impl<'i> ToCss for CounterStyleRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    dest.write_str("@counter-style ")?;
    self.name.to_css(dest)?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    let len = self.declarations.len();
    for (i, prop) in self.declarations.iter().enumerate() {
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

impl<'i> ToCss for CounterStyleProperty<'i> {
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
      CounterStyleProperty::System(v) => property!("system", v),
      CounterStyleProperty::Negative(v) => property!("negative", v),
      CounterStyleProperty::Prefix(v) => property!("prefix", v),
      CounterStyleProperty::Suffix(v) => property!("suffix", v),
      CounterStyleProperty::Range(v) => property!("range", v),
      CounterStyleProperty::Pad(v) => property!("pad", v),
      CounterStyleProperty::Fallback(v) => property!("fallback", v),
      CounterStyleProperty::Symbols(v) => property!("symbols", v),
      CounterStyleProperty::AdditiveSymbols(v) => property!("additive-symbols", v),
      CounterStyleProperty::SpeakAs(v) => property!("speak-as", v),
      CounterStyleProperty::Custom(custom) => {
        dest.write_str(custom.name.as_ref())?;
        dest.delim(':', false)?;
        custom.value.to_css(dest, true)
      }
    }
  }
}
