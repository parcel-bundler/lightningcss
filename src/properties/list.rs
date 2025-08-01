//! CSS properties related to lists and counters.

use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property, shorthand_handler};
use crate::printer::Printer;
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::string::CSSString;
use crate::values::{ident::CustomIdent, image::Image};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A value for the [list-style-type](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#text-markers) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum ListStyleType<'i> {
  /// No marker.
  None,
  /// An explicit marker string.
  #[cfg_attr(feature = "serde", serde(borrow))]
  String(CSSString<'i>),
  /// A named counter style.
  CounterStyle(CounterStyle<'i>),
}

impl Default for ListStyleType<'_> {
  fn default() -> Self {
    ListStyleType::CounterStyle(CounterStyle::Predefined(PredefinedCounterStyle::Disc))
  }
}

impl IsCompatible for ListStyleType<'_> {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      ListStyleType::CounterStyle(c) => c.is_compatible(browsers),
      ListStyleType::String(..) => crate::compat::Feature::StringListStyleType.is_compatible(browsers),
      ListStyleType::None => true,
    }
  }
}

/// A [counter-style](https://www.w3.org/TR/css-counter-styles-3/#typedef-counter-style) name.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum CounterStyle<'i> {
  /// A predefined counter style name.
  #[cfg_attr(
    feature = "serde",
    serde(with = "crate::serialization::ValueWrapper::<PredefinedCounterStyle>")
  )]
  Predefined(PredefinedCounterStyle),
  /// A custom counter style name.
  #[cfg_attr(
    feature = "serde",
    serde(borrow, with = "crate::serialization::ValueWrapper::<CustomIdent>")
  )]
  Name(CustomIdent<'i>),
  /// An inline [`symbols()`](https://www.w3.org/TR/css-counter-styles-3/#symbols-function) definition.
  Symbols {
    /// The counter system.
    #[cfg_attr(feature = "serde", serde(default))]
    system: SymbolsType,
    /// The symbols.
    symbols: Vec<Symbol<'i>>,
  },
}

macro_rules! counter_styles {
  (
    $(#[$outer:meta])*
    $vis:vis enum $name:ident {
      $(
        $(#[$meta: meta])*
        $id: ident,
      )+
    }
  ) => {
    enum_property! {
      /// A [predefined counter](https://www.w3.org/TR/css-counter-styles-3/#predefined-counters) style.
      #[allow(missing_docs)]
      pub enum PredefinedCounterStyle {
        $(
           $(#[$meta])*
           $id,
        )+
      }
    }

    impl IsCompatible for PredefinedCounterStyle {
      fn is_compatible(&self, browsers: Browsers) -> bool {
        match self {
          $(
            PredefinedCounterStyle::$id => pastey::paste! {
              crate::compat::Feature::[<$id ListStyleType>].is_compatible(browsers)
            },
          )+
        }
      }
    }
  };
}

counter_styles! {
  /// A [predefined counter](https://www.w3.org/TR/css-counter-styles-3/#predefined-counters) style.
  #[allow(missing_docs)]
  pub enum PredefinedCounterStyle {
    // https://www.w3.org/TR/css-counter-styles-3/#simple-numeric
    Decimal,
    DecimalLeadingZero,
    ArabicIndic,
    Armenian,
    UpperArmenian,
    LowerArmenian,
    Bengali,
    Cambodian,
    Khmer,
    CjkDecimal,
    Devanagari,
    Georgian,
    Gujarati,
    Gurmukhi,
    Hebrew,
    Kannada,
    Lao,
    Malayalam,
    Mongolian,
    Myanmar,
    Oriya,
    Persian,
    LowerRoman,
    UpperRoman,
    Tamil,
    Telugu,
    Thai,
    Tibetan,

    // https://www.w3.org/TR/css-counter-styles-3/#simple-alphabetic
    LowerAlpha,
    LowerLatin,
    UpperAlpha,
    UpperLatin,
    LowerGreek,
    Hiragana,
    HiraganaIroha,
    Katakana,
    KatakanaIroha,

    // https://www.w3.org/TR/css-counter-styles-3/#simple-symbolic
    Disc,
    Circle,
    Square,
    DisclosureOpen,
    DisclosureClosed,

    // https://www.w3.org/TR/css-counter-styles-3/#simple-fixed
    CjkEarthlyBranch,
    CjkHeavenlyStem,

    // https://www.w3.org/TR/css-counter-styles-3/#complex-cjk
    JapaneseInformal,
    JapaneseFormal,
    KoreanHangulFormal,
    KoreanHanjaInformal,
    KoreanHanjaFormal,
    SimpChineseInformal,
    SimpChineseFormal,
    TradChineseInformal,
    TradChineseFormal,
    EthiopicNumeric,
  }
}

impl<'i> Parse<'i> for CounterStyle<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(predefined) = input.try_parse(PredefinedCounterStyle::parse) {
      return Ok(CounterStyle::Predefined(predefined));
    }

    if input.try_parse(|input| input.expect_function_matching("symbols")).is_ok() {
      return input.parse_nested_block(|input| {
        let t = input.try_parse(SymbolsType::parse).unwrap_or_default();

        let mut symbols = Vec::new();
        while let Ok(s) = input.try_parse(Symbol::parse) {
          symbols.push(s);
        }

        Ok(CounterStyle::Symbols { system: t, symbols })
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
          css_module.reference(&name.0, dest.loc.source_index)
        }
        name.to_css(dest)
      }
      CounterStyle::Symbols { system: t, symbols } => {
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

impl IsCompatible for CounterStyle<'_> {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      CounterStyle::Name(..) => true,
      CounterStyle::Predefined(p) => p.is_compatible(browsers),
      CounterStyle::Symbols { .. } => crate::compat::Feature::SymbolsListStyleType.is_compatible(browsers),
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

impl Default for SymbolsType {
  fn default() -> Self {
    SymbolsType::Symbolic
  }
}

/// A single [symbol](https://www.w3.org/TR/css-counter-styles-3/#funcdef-symbols) as used in the
/// `symbols()` function.
///
/// See [CounterStyle](CounterStyle).
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Symbol<'i> {
  /// A string.
  #[cfg_attr(feature = "serde", serde(borrow))]
  String(CSSString<'i>),
  /// An image.
  Image(Image<'i>),
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

impl IsCompatible for ListStylePosition {
  fn is_compatible(&self, _browsers: Browsers) -> bool {
    true
  }
}

enum_property! {
  /// A value for the [marker-side](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#marker-side) property.
  #[allow(missing_docs)]
  pub enum MarkerSide {
    MatchSelf,
    MatchParent,
  }
}

define_shorthand! {
  /// A value for the [list-style](https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-property) shorthand property.
  pub struct ListStyle<'i> {
    /// The position of the list marker.
    position: ListStylePosition(ListStylePosition),
    /// The list marker image.
    #[cfg_attr(feature = "serde", serde(borrow))]
    image: ListStyleImage(Image<'i>),
    /// The list style type.
    list_style_type: ListStyleType(ListStyleType<'i>),
  }
}

impl<'i> Parse<'i> for ListStyle<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut position = None;
    let mut image = None;
    let mut list_style_type = None;
    let mut nones = 0;

    loop {
      // `none` is ambiguous - both list-style-image and list-style-type support it.
      if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
        nones += 1;
        if nones > 2 {
          return Err(input.new_custom_error(ParserError::InvalidValue));
        }
        continue;
      }

      if image.is_none() {
        if let Ok(val) = input.try_parse(Image::parse) {
          image = Some(val);
          continue;
        }
      }

      if position.is_none() {
        if let Ok(val) = input.try_parse(ListStylePosition::parse) {
          position = Some(val);
          continue;
        }
      }

      if list_style_type.is_none() {
        if let Ok(val) = input.try_parse(ListStyleType::parse) {
          list_style_type = Some(val);
          continue;
        }
      }

      break;
    }

    // Assign the `none` to the opposite property from the one we have a value for,
    // or both in case neither list-style-image or list-style-type have a value.
    match (nones, image, list_style_type) {
      (2, None, None) | (1, None, None) => Ok(ListStyle {
        position: position.unwrap_or_default(),
        image: Image::None,
        list_style_type: ListStyleType::None,
      }),
      (1, Some(image), None) => Ok(ListStyle {
        position: position.unwrap_or_default(),
        image,
        list_style_type: ListStyleType::None,
      }),
      (1, None, Some(list_style_type)) => Ok(ListStyle {
        position: position.unwrap_or_default(),
        image: Image::None,
        list_style_type,
      }),
      (0, image, list_style_type) => Ok(ListStyle {
        position: position.unwrap_or_default(),
        image: image.unwrap_or_default(),
        list_style_type: list_style_type.unwrap_or_default(),
      }),
      _ => Err(input.new_custom_error(ParserError::InvalidValue)),
    }
  }
}

impl<'i> ToCss for ListStyle<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut needs_space = false;
    if self.position != ListStylePosition::default() {
      self.position.to_css(dest)?;
      needs_space = true;
    }

    if self.image != Image::default() {
      if needs_space {
        dest.write_char(' ')?;
      }
      self.image.to_css(dest)?;
      needs_space = true;
    }

    if self.list_style_type != ListStyleType::default() {
      if needs_space {
        dest.write_char(' ')?;
      }
      self.list_style_type.to_css(dest)?;
      needs_space = true;
    }

    if !needs_space {
      self.position.to_css(dest)?;
    }

    Ok(())
  }
}

impl<'i> FallbackValues for ListStyle<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
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
