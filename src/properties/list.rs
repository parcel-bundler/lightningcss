use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::{enum_property, shorthand_handler, shorthand_property};
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{FallbackValues, Parse, PropertyHandler, ToCss};
use crate::values::string::CowArcStr;
use crate::values::{ident::CustomIdent, image::Image};
use cssparser::*;

/// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#text-markers
#[derive(Debug, Clone, PartialEq)]
pub enum ListStyleType<'i> {
  None,
  CounterStyle(CounterStyle<'i>),
  String(CowArcStr<'i>),
}

impl Default for ListStyleType<'_> {
  fn default() -> Self {
    ListStyleType::CounterStyle(CounterStyle::Predefined(PredefinedCounterStyle::Disc))
  }
}

impl<'i> Parse<'i> for ListStyleType<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(ListStyleType::None);
    }

    if let Ok(val) = input.try_parse(CounterStyle::parse) {
      return Ok(ListStyleType::CounterStyle(val));
    }

    let s = input.expect_string_cloned()?;
    Ok(ListStyleType::String(s.into()))
  }
}

impl ToCss for ListStyleType<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      ListStyleType::None => dest.write_str("none"),
      ListStyleType::CounterStyle(style) => style.to_css(dest),
      ListStyleType::String(s) => {
        serialize_string(&s, dest)?;
        Ok(())
      }
    }
  }
}

/// https://www.w3.org/TR/css-counter-styles-3/#typedef-counter-style
#[derive(Debug, Clone, PartialEq)]
pub enum CounterStyle<'i> {
  Predefined(PredefinedCounterStyle),
  Name(CustomIdent<'i>),
  Symbols(SymbolsType, Vec<Symbol<'i>>),
}

enum_property! {
  /// https://www.w3.org/TR/css-counter-styles-3/#predefined-counters
  pub enum PredefinedCounterStyle {
    // https://www.w3.org/TR/css-counter-styles-3/#simple-numeric
    "decimal": Decimal,
    "decimal-leading-zero": DecimalLeadingZero,
    "arabic-indic": ArabicIndic,
    "armenian": Armenian,
    "upper-armenian": UpperArmenian,
    "lower-armenian": LowerArmenian,
    "bengali": Bengali,
    "cambodian": Cambodian,
    "khmer": Khmer,
    "cjk-decimal": CjkDecimal,
    "devanagari": Devanagari,
    "georgian": Georgian,
    "gujarati": Gujarati,
    "gurmukhi": Gurmukhi,
    "hebrew": Hebrew,
    "kannada": Kannada,
    "lao": Lao,
    "malayalam": Malayalam,
    "mongolian": Mongolian,
    "myanmar": Myanmar,
    "oriya": Oriya,
    "persian": Persian,
    "lower-roman": LowerRoman,
    "upper-roman": UpperRoman,
    "tamil": Tamil,
    "telugu": Telugu,
    "thai": Thai,
    "tibetan": Tibetan,

    // https://www.w3.org/TR/css-counter-styles-3/#simple-alphabetic
    "lower-alpha": LowerAlpha,
    "lower-latin": LowerLatin,
    "upper-alpha": UpperAlpha,
    "upper-latin": UpperLatin,
    "lower-greek": LowerGreek,
    "hiragana": Hiragana,
    "hiragana-iroha": HiraganaIroha,
    "katakana": Katakana,
    "katakana-iroha": KatakanaIroha,

    // https://www.w3.org/TR/css-counter-styles-3/#simple-symbolic
    "disc": Disc,
    "circle": Circle,
    "square": Square,
    "disclosure-open": DisclosureOpen,
    "disclosure-closed": DisclosureClosed,

    // https://www.w3.org/TR/css-counter-styles-3/#simple-fixed
    "cjk-earthly-branch": CjkEarthlyBranch,
    "cjk-heavenly-stem": CjkHeavenlyStem,

    // https://www.w3.org/TR/css-counter-styles-3/#complex-cjk
    "japanese-informal": JapaneseInformal,
    "japanese-formal": JapaneseFormal,
    "korean-hangul-formal": KoreanHangulFormal,
    "korean-hanja-informal": KoreanHanjaInformal,
    "korean-hanja-formal": KoreanHanjaFormal,
    "simp-chinese-informal": SimpChineseInformal,
    "simp-chinese-formal": SimpChineseFormal,
    "trad-chinese-informal": TradChineseInformal,
    "trad-chinese-formal": TradChineseFormal,
    "ethiopic-numeric": EthiopicNumeric,
  }
}

impl<'i> Parse<'i> for CounterStyle<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(predefined) = input.try_parse(PredefinedCounterStyle::parse) {
      return Ok(CounterStyle::Predefined(predefined));
    }

    if input.try_parse(|input| input.expect_function_matching("symbols")).is_ok() {
      return input.parse_nested_block(|input| {
        let t = input.try_parse(SymbolsType::parse).unwrap_or(SymbolsType::Symbolic);

        let mut symbols = Vec::new();
        while let Ok(s) = input.try_parse(Symbol::parse) {
          symbols.push(s);
        }

        Ok(CounterStyle::Symbols(t, symbols))
      });
    }

    let name = CustomIdent::parse(input)?;
    Ok(CounterStyle::Name(name))
  }
}

impl ToCss for CounterStyle<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      CounterStyle::Predefined(style) => style.to_css(dest),
      CounterStyle::Name(name) => {
        if let Some(css_module) = &mut dest.css_module {
          css_module.reference(&name.0)
        }
        name.to_css(dest)
      }
      CounterStyle::Symbols(t, symbols) => {
        dest.write_str("symbols(")?;
        let mut needs_space = false;
        if *t != SymbolsType::Symbolic {
          t.to_css(dest)?;
          needs_space = true;
        }

        for symbol in symbols {
          if needs_space {
            dest.write_char(' ')?;
          }
          symbol.to_css(dest)?;
          needs_space = true;
        }
        dest.write_char(')')
      }
    }
  }
}

enum_property! {
  pub enum SymbolsType {
    Cyclic,
    Numeric,
    Alphabetic,
    Symbolic,
    Fixed,
  }
}

/// https://www.w3.org/TR/css-counter-styles-3/#funcdef-symbols
#[derive(Debug, Clone, PartialEq)]
pub enum Symbol<'i> {
  String(CowArcStr<'i>),
  Image(Image<'i>),
}

impl<'i> Parse<'i> for Symbol<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(img) = input.try_parse(Image::parse) {
      return Ok(Symbol::Image(img));
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
    }
  }
}

enum_property! {
  /// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-position-property
  pub enum ListStylePosition {
    Inside,
    Outside,
  }
}

impl Default for ListStylePosition {
  fn default() -> ListStylePosition {
    ListStylePosition::Outside
  }
}

enum_property! {
  /// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#marker-side
  pub enum MarkerSide {
    "match-self": MatchSelf,
    "match-parent": MatchParent,
  }
}

// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-property
shorthand_property!(ListStyle<'i> {
  list_style_type: ListStyleType<'i>,
  image: Image<'i>,
  position: ListStylePosition,
});

impl<'i> FallbackValues for ListStyle<'i> {
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<Self> {
    self
      .image
      .get_fallbacks(targets)
      .into_iter()
      .map(|image| ListStyle { image, ..self.clone() })
      .collect()
  }
}

shorthand_handler!(ListStyleHandler -> ListStyle<'i> {
  list_style_type: ListStyleType(ListStyleType<'i>),
  image: ListStyleImage(Image<'i>, fallback: true),
  position: ListStylePosition(ListStylePosition),
});
