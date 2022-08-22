//! CSS properties related to lists and counters.

use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property, shorthand_handler, shorthand_property};
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::traits::{FallbackValues, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::string::CowArcStr;
use crate::values::{ident::CustomIdent, image::Image};
use cssparser::*;

/// A value for the [list-style-type](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#text-markers) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum ListStyleType<'i> {
  /// No marker.
  None,
  /// An explicit marker string.
  #[cfg_attr(feature = "serde", serde(borrow))]
  String(CowArcStr<'i>),
  /// A named counter style.
  CounterStyle(CounterStyle<'i>),
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

/// A [counter-style](https://www.w3.org/TR/css-counter-styles-3/#typedef-counter-style) name.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum CounterStyle<'i> {
  /// A predefined counter style name.
  Predefined(PredefinedCounterStyle),
  /// A custom counter style name.
  Name(CustomIdent<'i>),
  /// An inline [`symbols()`](https://www.w3.org/TR/css-counter-styles-3/#symbols-function) definition.
  Symbols(
    SymbolsType,
    #[cfg_attr(feature = "serde", serde(borrow))] Vec<Symbol<'i>>,
  ),
}

enum_property! {
  /// A [predefined counter](https://www.w3.org/TR/css-counter-styles-3/#predefined-counters) style.
  #[allow(missing_docs)]
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
  /// A [`<symbols-type>`](https://www.w3.org/TR/css-counter-styles-3/#typedef-symbols-type) value,
  /// as used in the `symbols()` function.
  ///
  /// See [CounterStyle](CounterStyle).
  #[allow(missing_docs)]
  pub enum SymbolsType {
    Cyclic,
    Numeric,
    Alphabetic,
    Symbolic,
    Fixed,
  }
}

/// A single [symbol](https://www.w3.org/TR/css-counter-styles-3/#funcdef-symbols) as used in the
/// `symbols()` function.
///
/// See [CounterStyle](CounterStyle).
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
  /// A value for the [list-style-position](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-position-property) property.
  pub enum ListStylePosition {
    /// The list marker is placed inside the element.
    Inside,
    /// The list marker is placed outside the element.
    Outside,
  }
}

impl Default for ListStylePosition {
  fn default() -> ListStylePosition {
    ListStylePosition::Outside
  }
}

enum_property! {
  /// A value for the [marker-side](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#marker-side) property.
  #[allow(missing_docs)]
  pub enum MarkerSide {
    "match-self": MatchSelf,
    "match-parent": MatchParent,
  }
}

shorthand_property! {
  /// A value for the [list-style](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-property) shorthand property.
  pub struct ListStyle<'i> {
    /// The list style type.
    #[cfg_attr(feature = "serde", serde(borrow))]
    list_style_type: ListStyleType(ListStyleType<'i>),
    /// The list marker image.
    image: ListStyleImage(Image<'i>),
    /// The position of the list marker.
    position: ListStylePosition(ListStylePosition),
  }
}

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

shorthand_handler!(ListStyleHandler -> ListStyle<'i> fallbacks: true {
  image: ListStyleImage(Image<'i>, fallback: true, image: true),
  list_style_type: ListStyleType(ListStyleType<'i>),
  position: ListStylePosition(ListStylePosition),
});
