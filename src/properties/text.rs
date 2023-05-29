//! CSS properties related to text.

#![allow(non_upper_case_globals)]

use super::{Property, PropertyId};
use crate::compat;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property};
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::targets::{should_compile, Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, PropertyHandler, Shorthand, ToCss, Zero};
use crate::values::calc::{Calc, MathFunction};
use crate::values::color::{ColorFallbackKind, CssColor};
use crate::values::length::{Length, LengthPercentage, LengthValue};
use crate::values::percentage::Percentage;
use crate::values::string::CSSString;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use bitflags::bitflags;
use cssparser::*;
use smallvec::SmallVec;

enum_property! {
  /// Defines how text case should be transformed in the
  /// [text-transform](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-transform-property) property.
  pub enum TextTransformCase {
    /// Text should not be transformed.
    None,
    /// Text should be uppercased.
    Uppercase,
    /// Text should be lowercased.
    Lowercase,
    /// Each word should be capitalized.
    Capitalize,
  }
}

impl Default for TextTransformCase {
  fn default() -> TextTransformCase {
    TextTransformCase::None
  }
}

bitflags! {
  /// Defines how ideographic characters should be transformed in the
  /// [text-transform](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-transform-property) property.
  ///
  /// All combinations of flags is supported.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "SerializedTextTransformOther", into = "SerializedTextTransformOther"))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct TextTransformOther: u8 {
    /// Puts all typographic character units in full-width form.
    const FullWidth    = 0b00000001;
    /// Converts all small Kana characters to the equivalent full-size Kana.
    const FullSizeKana = 0b00000010;
  }
}

impl<'i> Parse<'i> for TextTransformOther {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &ident,
      "full-width" => Ok(TextTransformOther::FullWidth),
      "full-size-kana" => Ok(TextTransformOther::FullSizeKana),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for TextTransformOther {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut needs_space = false;
    if self.contains(TextTransformOther::FullWidth) {
      dest.write_str("full-width")?;
      needs_space = true;
    }

    if self.contains(TextTransformOther::FullSizeKana) {
      if needs_space {
        dest.write_char(' ')?;
      }
      dest.write_str("full-size-kana")?;
    }

    Ok(())
  }
}

#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
struct SerializedTextTransformOther {
  /// Puts all typographic character units in full-width form.
  full_width: bool,
  /// Converts all small Kana characters to the equivalent full-size Kana.
  full_size_kana: bool,
}

impl From<TextTransformOther> for SerializedTextTransformOther {
  fn from(t: TextTransformOther) -> Self {
    Self {
      full_width: t.contains(TextTransformOther::FullWidth),
      full_size_kana: t.contains(TextTransformOther::FullSizeKana),
    }
  }
}

impl From<SerializedTextTransformOther> for TextTransformOther {
  fn from(t: SerializedTextTransformOther) -> Self {
    let mut res = TextTransformOther::empty();
    if t.full_width {
      res |= TextTransformOther::FullWidth;
    }
    if t.full_size_kana {
      res |= TextTransformOther::FullSizeKana;
    }
    res
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for TextTransformOther {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    SerializedTextTransformOther::json_schema(gen)
  }

  fn schema_name() -> String {
    "TextTransformOther".into()
  }
}

/// A value for the [text-transform](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-transform-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TextTransform {
  /// How case should be transformed.
  pub case: TextTransformCase,
  /// How ideographic characters should be transformed.
  #[cfg_attr(feature = "serde", serde(flatten))]
  pub other: TextTransformOther,
}

impl<'i> Parse<'i> for TextTransform {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut case = None;
    let mut other = TextTransformOther::empty();

    loop {
      if case.is_none() {
        if let Ok(c) = input.try_parse(TextTransformCase::parse) {
          case = Some(c);
          if c == TextTransformCase::None {
            other = TextTransformOther::empty();
            break;
          }
          continue;
        }
      }

      if let Ok(o) = input.try_parse(TextTransformOther::parse) {
        other |= o;
        continue;
      }

      break;
    }

    Ok(TextTransform {
      case: case.unwrap_or_default(),
      other,
    })
  }
}

impl ToCss for TextTransform {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut needs_space = false;
    if self.case != TextTransformCase::None || self.other.is_empty() {
      self.case.to_css(dest)?;
      needs_space = true;
    }

    if !self.other.is_empty() {
      if needs_space {
        dest.write_char(' ')?;
      }
      self.other.to_css(dest)?;
    }
    Ok(())
  }
}

enum_property! {
  /// A value for the [white-space](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#white-space-property) property.
  pub enum WhiteSpace {
    /// Sequences of white space are collapsed into a single character.
    "normal": Normal,
    /// White space is not collapsed.
    "pre": Pre,
    /// White space is collapsed, but no line wrapping occurs.
    "nowrap": NoWrap,
    /// White space is preserved, but line wrapping occurs.
    "pre-wrap": PreWrap,
    /// Like pre-wrap, but with different line breaking rules.
    "break-spaces": BreakSpaces,
    /// White space is collapsed, but with different line breaking rules.
    "pre-line": PreLine,
  }
}

enum_property! {
  /// A value for the [word-break](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#word-break-property) property.
  pub enum WordBreak {
    /// Words break according to their customary rules.
    "normal": Normal,
    /// Breaking is forbidden within “words”.
    "keep-all": KeepAll,
    /// Breaking is allowed within “words”.
    "break-all": BreakAll,
    /// Breaking is allowed if there is no otherwise acceptable break points in a line.
    "break-word": BreakWord,
  }
}

enum_property! {
  /// A value for the [line-break](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#line-break-property) property.
  pub enum LineBreak {
    /// The UA determines the set of line-breaking restrictions to use.
    Auto,
    /// Breaks text using the least restrictive set of line-breaking rules.
    Loose,
    /// Breaks text using the most common set of line-breaking rules.
    Normal,
    /// Breaks text using the most stringent set of line-breaking rules.
    Strict,
    /// There is a soft wrap opportunity around every typographic character unit.
    Anywhere,
  }
}
enum_property! {
  /// A value for the [hyphens](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#hyphenation) property.
  pub enum Hyphens {
    /// Words are not hyphenated.
    None,
    /// Words are only hyphenated where there are characters inside the word that explicitly suggest hyphenation opportunities.
    Manual,
    /// Words may be broken at hyphenation opportunities determined automatically by the UA.
    Auto,
  }
}

enum_property! {
  /// A value for the [overflow-wrap](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#overflow-wrap-property) property.
  pub enum OverflowWrap {
    /// Lines may break only at allowed break points.
    "normal": Normal,
    /// Breaking is allowed if there is no otherwise acceptable break points in a line.
    "anywhere": Anywhere,
    /// As for anywhere except that soft wrap opportunities introduced by break-word are
    /// not considered when calculating min-content intrinsic sizes.
    "break-word": BreakWord,
  }
}

enum_property! {
  /// A value for the [text-align](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-align-property) property.
  pub enum TextAlign {
    /// Inline-level content is aligned to the start edge of the line box.
    "start": Start,
    /// Inline-level content is aligned to the end edge of the line box.
    "end": End,
    /// Inline-level content is aligned to the line-left edge of the line box.
    "left": Left,
    /// Inline-level content is aligned to the line-right edge of the line box.
    "right": Right,
    /// Inline-level content is centered within the line box.
    "center": Center,
    /// Text is justified according to the method specified by the text-justify property.
    "justify": Justify,
    /// Matches the parent element.
    "match-parent": MatchParent,
    /// Same as justify, but also justifies the last line.
    "justify-all": JustifyAll,
  }
}

enum_property! {
  /// A value for the [text-align-last](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-align-last-property) property.
  pub enum TextAlignLast {
    /// Content on the affected line is aligned per `text-align-all` unless set to `justify`, in which case it is start-aligned.
    "auto": Auto,
    /// Inline-level content is aligned to the start edge of the line box.
    "start": Start,
    /// Inline-level content is aligned to the end edge of the line box.
    "end": End,
    /// Inline-level content is aligned to the line-left edge of the line box.
    "left": Left,
    /// Inline-level content is aligned to the line-right edge of the line box.
    "right": Right,
    /// Inline-level content is centered within the line box.
    "center": Center,
    /// Text is justified according to the method specified by the text-justify property.
    "justify": Justify,
    /// Matches the parent element.
    "match-parent": MatchParent,
  }
}

enum_property! {
  /// A value for the [text-justify](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-justify-property) property.
  pub enum TextJustify {
    /// The UA determines the justification algorithm to follow.
    "auto": Auto,
    /// Justification is disabled.
    "none": None,
    /// Justification adjusts spacing at word separators only.
    "inter-word": InterWord,
    /// Justification adjusts spacing between each character.
    "inter-character": InterCharacter,
  }
}

/// A value for the [word-spacing](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#word-spacing-property)
/// and [letter-spacing](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#letter-spacing-property) properties.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Spacing {
  /// No additional spacing is applied.
  Normal,
  /// Additional spacing between each word or letter.
  Length(Length),
}

impl<'i> Parse<'i> for Spacing {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(Spacing::Normal);
    }

    let length = Length::parse(input)?;
    Ok(Spacing::Length(length))
  }
}

impl ToCss for Spacing {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Spacing::Normal => dest.write_str("normal"),
      Spacing::Length(len) => len.to_css(dest),
    }
  }
}

/// A value for the [text-indent](https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-indent-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TextIndent {
  /// The amount to indent.
  pub value: LengthPercentage,
  /// Inverts which lines are affected.
  pub hanging: bool,
  /// Affects the first line after each hard break.
  pub each_line: bool,
}

impl<'i> Parse<'i> for TextIndent {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut value = None;
    let mut hanging = false;
    let mut each_line = false;

    loop {
      if value.is_none() {
        if let Ok(val) = input.try_parse(LengthPercentage::parse) {
          value = Some(val);
          continue;
        }
      }

      if !hanging {
        if input.try_parse(|input| input.expect_ident_matching("hanging")).is_ok() {
          hanging = true;
          continue;
        }
      }

      if !each_line {
        if input.try_parse(|input| input.expect_ident_matching("each-line")).is_ok() {
          each_line = true;
          continue;
        }
      }

      break;
    }

    if let Some(value) = value {
      Ok(TextIndent {
        value,
        hanging,
        each_line,
      })
    } else {
      Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }
  }
}

impl ToCss for TextIndent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.value.to_css(dest)?;
    if self.hanging {
      dest.write_str(" hanging")?;
    }
    if self.each_line {
      dest.write_str(" each-line")?;
    }
    Ok(())
  }
}

/// A value for the [text-size-adjust](https://w3c.github.io/csswg-drafts/css-size-adjust/#adjustment-control) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum TextSizeAdjust {
  /// Use the default size adjustment when displaying on a small device.
  Auto,
  /// No size adjustment when displaying on a small device.
  None,
  /// When displaying on a small device, the font size is multiplied by this percentage.
  Percentage(Percentage),
}

impl<'i> Parse<'i> for TextSizeAdjust {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(p) = input.try_parse(Percentage::parse) {
      return Ok(TextSizeAdjust::Percentage(p));
    }

    let ident = input.expect_ident_cloned()?;
    match_ignore_ascii_case! {&*ident,
      "auto" => Ok(TextSizeAdjust::Auto),
      "none" => Ok(TextSizeAdjust::None),
      _ => Err(input.new_unexpected_token_error(Token::Ident(ident.clone())))
    }
  }
}

impl ToCss for TextSizeAdjust {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      TextSizeAdjust::Auto => dest.write_str("auto"),
      TextSizeAdjust::None => dest.write_str("none"),
      TextSizeAdjust::Percentage(p) => p.to_css(dest),
    }
  }
}

bitflags! {
  /// A value for the [text-decoration-line](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-line-property) property.
  ///
  /// Multiple lines may be specified by combining the flags.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "SerializedTextDecorationLine", into = "SerializedTextDecorationLine"))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct TextDecorationLine: u8 {
    /// Each line of text is underlined.
    const Underline     = 0b00000001;
    /// Each line of text has a line over it.
    const Overline      = 0b00000010;
    /// Each line of text has a line through the middle.
    const LineThrough   = 0b00000100;
    /// The text blinks.
    const Blink         = 0b00001000;
    /// The text is decorated as a spelling error.
    const SpellingError = 0b00010000;
    /// The text is decorated as a grammar error.
    const GrammarError  = 0b00100000;
  }
}

impl Default for TextDecorationLine {
  fn default() -> TextDecorationLine {
    TextDecorationLine::empty()
  }
}

impl<'i> Parse<'i> for TextDecorationLine {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut value = TextDecorationLine::empty();
    let mut any = false;

    loop {
      let flag: Result<_, ParseError<'i, ParserError<'i>>> = input.try_parse(|input| {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        Ok(match_ignore_ascii_case! { &ident,
          "none" if value.is_empty() => TextDecorationLine::empty(),
          "underline" => TextDecorationLine::Underline,
          "overline" => TextDecorationLine::Overline,
          "line-through" => TextDecorationLine::LineThrough,
          "blink" =>TextDecorationLine::Blink,
          "spelling-error" if value.is_empty() => TextDecorationLine::SpellingError,
          "grammar-error" if value.is_empty() => TextDecorationLine::GrammarError,
          _ => return Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        })
      });

      if let Ok(flag) = flag {
        value |= flag;
        any = true;
      } else {
        break;
      }
    }

    if !any {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }

    Ok(value)
  }
}

impl ToCss for TextDecorationLine {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.is_empty() {
      return dest.write_str("none");
    }

    if self.contains(TextDecorationLine::SpellingError) {
      return dest.write_str("spelling-error");
    }

    if self.contains(TextDecorationLine::GrammarError) {
      return dest.write_str("grammar-error");
    }

    let mut needs_space = false;
    macro_rules! val {
      ($val: ident, $str: expr) => {
        #[allow(unused_assignments)]
        if self.contains(TextDecorationLine::$val) {
          if needs_space {
            dest.write_char(' ')?;
          }
          dest.write_str($str)?;
          needs_space = true;
        }
      };
    }

    val!(Underline, "underline");
    val!(Overline, "overline");
    val!(LineThrough, "line-through");
    val!(Blink, "blink");
    Ok(())
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(untagged))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum SerializedTextDecorationLine {
  Exclusive(ExclusiveTextDecorationLine),
  Other(Vec<OtherTextDecorationLine>),
}

#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum ExclusiveTextDecorationLine {
  None,
  SpellingError,
  GrammarError,
}

#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
enum OtherTextDecorationLine {
  Underline,
  Overline,
  LineThrough,
  Blink,
}

impl From<TextDecorationLine> for SerializedTextDecorationLine {
  fn from(l: TextDecorationLine) -> Self {
    if l.is_empty() {
      return Self::Exclusive(ExclusiveTextDecorationLine::None);
    }

    macro_rules! exclusive {
      ($t: ident) => {
        if l.contains(TextDecorationLine::$t) {
          return Self::Exclusive(ExclusiveTextDecorationLine::$t);
        }
      };
    }

    exclusive!(SpellingError);
    exclusive!(GrammarError);

    let mut v = Vec::new();
    macro_rules! other {
      ($t: ident) => {
        if l.contains(TextDecorationLine::$t) {
          v.push(OtherTextDecorationLine::$t)
        }
      };
    }

    other!(Underline);
    other!(Overline);
    other!(LineThrough);
    other!(Blink);
    Self::Other(v)
  }
}

impl From<SerializedTextDecorationLine> for TextDecorationLine {
  fn from(l: SerializedTextDecorationLine) -> Self {
    match l {
      SerializedTextDecorationLine::Exclusive(v) => match v {
        ExclusiveTextDecorationLine::None => TextDecorationLine::empty(),
        ExclusiveTextDecorationLine::SpellingError => TextDecorationLine::SpellingError,
        ExclusiveTextDecorationLine::GrammarError => TextDecorationLine::GrammarError,
      },
      SerializedTextDecorationLine::Other(v) => {
        let mut res = TextDecorationLine::empty();
        for val in v {
          res |= match val {
            OtherTextDecorationLine::Underline => TextDecorationLine::Underline,
            OtherTextDecorationLine::Overline => TextDecorationLine::Overline,
            OtherTextDecorationLine::LineThrough => TextDecorationLine::LineThrough,
            OtherTextDecorationLine::Blink => TextDecorationLine::Blink,
          }
        }
        res
      }
    }
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for TextDecorationLine {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    SerializedTextDecorationLine::json_schema(gen)
  }

  fn schema_name() -> String {
    "TextDecorationLine".into()
  }
}

enum_property! {
  /// A value for the [text-decoration-style](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-style-property) property.
  pub enum TextDecorationStyle {
    /// A single line segment.
    Solid,
    /// Two parallel solid lines with some space between them.
    Double,
    /// A series of round dots.
    Dotted,
    /// A series of square-ended dashes.
    Dashed,
    /// A wavy line.
    Wavy,
  }
}

impl Default for TextDecorationStyle {
  fn default() -> TextDecorationStyle {
    TextDecorationStyle::Solid
  }
}

/// A value for the [text-decoration-thickness](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-width-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum TextDecorationThickness {
  /// The UA chooses an appropriate thickness for text decoration lines.
  Auto,
  /// Use the thickness defined in the current font.
  FromFont,
  /// An explicit length.
  LengthPercentage(LengthPercentage),
}

impl Default for TextDecorationThickness {
  fn default() -> TextDecorationThickness {
    TextDecorationThickness::Auto
  }
}

impl<'i> Parse<'i> for TextDecorationThickness {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(TextDecorationThickness::Auto);
    }

    if input.try_parse(|input| input.expect_ident_matching("from-font")).is_ok() {
      return Ok(TextDecorationThickness::FromFont);
    }

    let lp = LengthPercentage::parse(input)?;
    Ok(TextDecorationThickness::LengthPercentage(lp))
  }
}

impl ToCss for TextDecorationThickness {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      TextDecorationThickness::Auto => dest.write_str("auto"),
      TextDecorationThickness::FromFont => dest.write_str("from-font"),
      TextDecorationThickness::LengthPercentage(lp) => lp.to_css(dest),
    }
  }
}

define_shorthand! {
  /// A value for the [text-decoration](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-property) shorthand property.
  pub struct TextDecoration(VendorPrefix) {
    /// The lines to display.
    line: TextDecorationLine(TextDecorationLine, VendorPrefix),
    /// The thickness of the lines.
    thickness: TextDecorationThickness(TextDecorationThickness),
    /// The style of the lines.
    style: TextDecorationStyle(TextDecorationStyle, VendorPrefix),
    /// The color of the lines.
    color: TextDecorationColor(CssColor, VendorPrefix),
  }
}

impl<'i> Parse<'i> for TextDecoration {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut line = None;
    let mut thickness = None;
    let mut style = None;
    let mut color = None;

    loop {
      macro_rules! prop {
        ($key: ident, $type: ident) => {
          if $key.is_none() {
            if let Ok(val) = input.try_parse($type::parse) {
              $key = Some(val);
              continue;
            }
          }
        };
      }

      prop!(line, TextDecorationLine);
      prop!(thickness, TextDecorationThickness);
      prop!(style, TextDecorationStyle);
      prop!(color, CssColor);
      break;
    }

    Ok(TextDecoration {
      line: line.unwrap_or_default(),
      thickness: thickness.unwrap_or_default(),
      style: style.unwrap_or_default(),
      color: color.unwrap_or(CssColor::current_color()),
    })
  }
}

impl ToCss for TextDecoration {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.line.to_css(dest)?;
    if self.line.is_empty() {
      return Ok(());
    }

    let mut needs_space = true;
    if self.thickness != TextDecorationThickness::default() {
      dest.write_char(' ')?;
      self.thickness.to_css(dest)?;
      needs_space = true;
    }

    if self.style != TextDecorationStyle::default() {
      if needs_space {
        dest.write_char(' ')?;
      }
      self.style.to_css(dest)?;
      needs_space = true;
    }

    if self.color != CssColor::current_color() {
      if needs_space {
        dest.write_char(' ')?;
      }
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}

impl FallbackValues for TextDecoration {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    self
      .color
      .get_fallbacks(targets)
      .into_iter()
      .map(|color| TextDecoration { color, ..self.clone() })
      .collect()
  }
}

enum_property! {
  /// A value for the [text-decoration-skip-ink](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-skip-ink-property) property.
  pub enum TextDecorationSkipInk {
    /// UAs may interrupt underlines and overlines.
    Auto,
    /// UAs must interrupt underlines and overlines.
    None,
    /// UA must draw continuous underlines and overlines.
    All,
  }
}

enum_property! {
  /// A keyword for the [text-emphasis-style](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-style-property) property.
  ///
  /// See [TextEmphasisStyle](TextEmphasisStyle).
  pub enum TextEmphasisFillMode {
    /// The shape is filled with solid color.
    Filled,
    /// The shape is hollow.
    Open,
  }
}

enum_property! {
  /// A text emphasis shape for the [text-emphasis-style](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-style-property) property.
  ///
  /// See [TextEmphasisStyle](TextEmphasisStyle).
  pub enum TextEmphasisShape {
    /// Display small circles as marks.
    "dot": Dot,
    /// Display large circles as marks.
    "circle": Circle,
    /// Display double circles as marks.
    "double-circle": DoubleCircle,
    /// Display triangles as marks.
    "triangle": Triangle,
    /// Display sesames as marks.
    "sesame": Sesame,
  }
}

/// A value for the [text-emphasis-style](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-style-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum TextEmphasisStyle<'i> {
  /// No emphasis.
  None,
  /// Defines the fill and shape of the marks.
  Keyword {
    /// The fill mode for the marks.
    fill: TextEmphasisFillMode,
    /// The shape of the marks.
    shape: Option<TextEmphasisShape>,
  },
  /// Display the given string as marks.
  #[cfg_attr(
    feature = "serde",
    serde(borrow, with = "crate::serialization::ValueWrapper::<CSSString>")
  )]
  String(CSSString<'i>),
}

impl<'i> Default for TextEmphasisStyle<'i> {
  fn default() -> TextEmphasisStyle<'i> {
    TextEmphasisStyle::None
  }
}

impl<'i> Parse<'i> for TextEmphasisStyle<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(TextEmphasisStyle::None);
    }

    if let Ok(s) = input.try_parse(CSSString::parse) {
      return Ok(TextEmphasisStyle::String(s));
    }

    let mut shape = input.try_parse(TextEmphasisShape::parse).ok();
    let fill = input.try_parse(TextEmphasisFillMode::parse).ok();
    if shape.is_none() {
      shape = input.try_parse(TextEmphasisShape::parse).ok();
    }

    if shape.is_none() && fill.is_none() {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }

    let fill = fill.unwrap_or(TextEmphasisFillMode::Filled);
    Ok(TextEmphasisStyle::Keyword { fill, shape })
  }
}

impl<'i> ToCss for TextEmphasisStyle<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      TextEmphasisStyle::None => dest.write_str("none"),
      TextEmphasisStyle::String(s) => s.to_css(dest),
      TextEmphasisStyle::Keyword { fill, shape } => {
        let mut needs_space = false;
        if *fill != TextEmphasisFillMode::Filled || shape.is_none() {
          fill.to_css(dest)?;
          needs_space = true;
        }

        if let Some(shape) = shape {
          if needs_space {
            dest.write_char(' ')?;
          }
          shape.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

define_shorthand! {
  /// A value for the [text-emphasis](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-property) shorthand property.
  #[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
  pub struct TextEmphasis<'i>(VendorPrefix) {
    /// The text emphasis style.
    #[cfg_attr(feature = "serde", serde(borrow))]
    style: TextEmphasisStyle(TextEmphasisStyle<'i>, VendorPrefix),
    /// The text emphasis color.
    color: TextEmphasisColor(CssColor, VendorPrefix),
  }
}

impl<'i> Parse<'i> for TextEmphasis<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut style = None;
    let mut color = None;

    loop {
      if style.is_none() {
        if let Ok(s) = input.try_parse(TextEmphasisStyle::parse) {
          style = Some(s);
          continue;
        }
      }

      if color.is_none() {
        if let Ok(c) = input.try_parse(CssColor::parse) {
          color = Some(c);
          continue;
        }
      }

      break;
    }

    Ok(TextEmphasis {
      style: style.unwrap_or_default(),
      color: color.unwrap_or(CssColor::current_color()),
    })
  }
}

impl<'i> ToCss for TextEmphasis<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.style.to_css(dest)?;

    if self.style != TextEmphasisStyle::None && self.color != CssColor::current_color() {
      dest.write_char(' ')?;
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}

impl<'i> FallbackValues for TextEmphasis<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    self
      .color
      .get_fallbacks(targets)
      .into_iter()
      .map(|color| TextEmphasis { color, ..self.clone() })
      .collect()
  }
}

enum_property! {
  /// A vertical position keyword for the [text-emphasis-position](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-position-property) property.
  ///
  /// See [TextEmphasisPosition](TextEmphasisPosition).
  pub enum TextEmphasisPositionVertical {
    /// Draw marks over the text in horizontal typographic modes.
    Over,
    /// Draw marks under the text in horizontal typographic modes.
    Under,
  }
}

enum_property! {
  /// A horizontal position keyword for the [text-emphasis-position](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-position-property) property.
  ///
  /// See [TextEmphasisPosition](TextEmphasisPosition).
  pub enum TextEmphasisPositionHorizontal {
    /// Draw marks to the right of the text in vertical typographic modes.
    Left,
    /// Draw marks to the left of the text in vertical typographic modes.
    Right,
  }
}

/// A value for the [text-emphasis-position](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-position-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TextEmphasisPosition {
  /// The vertical position.
  pub vertical: TextEmphasisPositionVertical,
  /// The horizontal position.
  pub horizontal: TextEmphasisPositionHorizontal,
}

impl<'i> Parse<'i> for TextEmphasisPosition {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(horizontal) = input.try_parse(TextEmphasisPositionHorizontal::parse) {
      let vertical = TextEmphasisPositionVertical::parse(input)?;
      Ok(TextEmphasisPosition { horizontal, vertical })
    } else {
      let vertical = TextEmphasisPositionVertical::parse(input)?;
      let horizontal = input
        .try_parse(TextEmphasisPositionHorizontal::parse)
        .unwrap_or(TextEmphasisPositionHorizontal::Right);
      Ok(TextEmphasisPosition { horizontal, vertical })
    }
  }
}

enum_property! {
  /// A value for the [box-decoration-break](https://www.w3.org/TR/css-break-3/#break-decoration) property.
  pub enum BoxDecorationBreak {
    /// The element is rendered with no breaks present, and then sliced by the breaks afterward.
    Slice,
    /// Each box fragment is independently wrapped with the border, padding, and margin.
    Clone,
  }
}

impl Default for BoxDecorationBreak {
  fn default() -> Self {
    BoxDecorationBreak::Slice
  }
}

impl ToCss for TextEmphasisPosition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.vertical.to_css(dest)?;
    if self.horizontal != TextEmphasisPositionHorizontal::Right {
      dest.write_char(' ')?;
      self.horizontal.to_css(dest)?;
    }
    Ok(())
  }
}

#[derive(Default)]
pub(crate) struct TextDecorationHandler<'i> {
  line: Option<(TextDecorationLine, VendorPrefix)>,
  thickness: Option<TextDecorationThickness>,
  style: Option<(TextDecorationStyle, VendorPrefix)>,
  color: Option<(CssColor, VendorPrefix)>,
  emphasis_style: Option<(TextEmphasisStyle<'i>, VendorPrefix)>,
  emphasis_color: Option<(CssColor, VendorPrefix)>,
  emphasis_position: Option<(TextEmphasisPosition, VendorPrefix)>,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for TextDecorationHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.finalize(dest, context);
          }
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: expr) => {{
        maybe_flush!($prop, $val, $vp);

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
        } else {
          self.$prop = Some(($val.clone(), *$vp));
          self.has_any = true;
        }
      }};
    }

    match property {
      TextDecorationLine(val, vp) => property!(line, val, vp),
      TextDecorationThickness(val) => {
        self.thickness = Some(val.clone());
        self.has_any = true;
      }
      TextDecorationStyle(val, vp) => property!(style, val, vp),
      TextDecorationColor(val, vp) => property!(color, val, vp),
      TextDecoration(val, vp) => {
        maybe_flush!(line, &val.line, vp);
        maybe_flush!(style, &val.style, vp);
        maybe_flush!(color, &val.color, vp);
        property!(line, &val.line, vp);
        self.thickness = Some(val.thickness.clone());
        property!(style, &val.style, vp);
        property!(color, &val.color, vp);
      }
      TextEmphasisStyle(val, vp) => property!(emphasis_style, val, vp),
      TextEmphasisColor(val, vp) => property!(emphasis_color, val, vp),
      TextEmphasis(val, vp) => {
        maybe_flush!(emphasis_style, &val.style, vp);
        maybe_flush!(emphasis_color, &val.color, vp);
        property!(emphasis_style, &val.style, vp);
        property!(emphasis_color, &val.color, vp);
      }
      TextEmphasisPosition(val, vp) => property!(emphasis_position, val, vp),
      TextAlign(align) => {
        use super::text::*;
        macro_rules! logical {
          ($ltr: ident, $rtl: ident) => {{
            let logical_supported = !context.should_compile_logical(compat::Feature::LogicalTextAlign);
            if logical_supported {
              dest.push(property.clone());
            } else {
              context.add_logical_rule(
                Property::TextAlign(TextAlign::$ltr),
                Property::TextAlign(TextAlign::$rtl),
              );
            }
          }};
        }

        match align {
          TextAlign::Start => logical!(Left, Right),
          TextAlign::End => logical!(Right, Left),
          _ => dest.push(property.clone()),
        }
      }
      Unparsed(val) if is_text_decoration_property(&val.property_id) => {
        self.finalize(dest, context);
        let mut unparsed = val.get_prefixed(context.targets, Feature::TextDecoration);
        context.add_unparsed_fallbacks(&mut unparsed);
        dest.push(Property::Unparsed(unparsed))
      }
      Unparsed(val) if is_text_emphasis_property(&val.property_id) => {
        self.finalize(dest, context);
        let mut unparsed = val.get_prefixed(context.targets, Feature::TextEmphasis);
        context.add_unparsed_fallbacks(&mut unparsed);
        dest.push(Property::Unparsed(unparsed))
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    let mut line = std::mem::take(&mut self.line);
    let mut thickness = std::mem::take(&mut self.thickness);
    let mut style = std::mem::take(&mut self.style);
    let mut color = std::mem::take(&mut self.color);
    let mut emphasis_style = std::mem::take(&mut self.emphasis_style);
    let mut emphasis_color = std::mem::take(&mut self.emphasis_color);
    let emphasis_position = std::mem::take(&mut self.emphasis_position);

    if let (Some((line, line_vp)), Some(thickness_val), Some((style, style_vp)), Some((color, color_vp))) =
      (&mut line, &mut thickness, &mut style, &mut color)
    {
      let intersection = *line_vp | *style_vp | *color_vp;
      if !intersection.is_empty() {
        let mut prefix = intersection;

        // Some browsers don't support thickness in the shorthand property yet.
        let supports_thickness = context.targets.is_compatible(compat::Feature::TextDecorationThicknessShorthand);
        let mut decoration = TextDecoration {
          line: line.clone(),
          thickness: if supports_thickness {
            thickness_val.clone()
          } else {
            TextDecorationThickness::default()
          },
          style: style.clone(),
          color: color.clone(),
        };

        // Only add prefixes if one of the new sub-properties was used
        if prefix.contains(VendorPrefix::None)
          && (*style != TextDecorationStyle::default() || *color != CssColor::current_color())
        {
          prefix = context.targets.prefixes(VendorPrefix::None, Feature::TextDecoration);

          let fallbacks = decoration.get_fallbacks(context.targets);
          for fallback in fallbacks {
            dest.push(Property::TextDecoration(fallback, prefix))
          }
        }

        dest.push(Property::TextDecoration(decoration, prefix));
        line_vp.remove(intersection);
        style_vp.remove(intersection);
        color_vp.remove(intersection);
        if supports_thickness || *thickness_val == TextDecorationThickness::default() {
          thickness = None;
        }
      }
    }

    macro_rules! color {
      ($key: ident, $prop: ident) => {
        if let Some((mut val, vp)) = $key {
          if !vp.is_empty() {
            let prefix = context.targets.prefixes(vp, Feature::$prop);
            if prefix.contains(VendorPrefix::None) {
              let fallbacks = val.get_fallbacks(context.targets);
              for fallback in fallbacks {
                dest.push(Property::$prop(fallback, prefix))
              }
            }
            dest.push(Property::$prop(val, prefix))
          }
        }
      };
    }

    macro_rules! single_property {
      ($key: ident, $prop: ident) => {
        if let Some((val, vp)) = $key {
          if !vp.is_empty() {
            let prefix = context.targets.prefixes(vp, Feature::$prop);
            dest.push(Property::$prop(val, prefix))
          }
        }
      };
    }

    single_property!(line, TextDecorationLine);
    single_property!(style, TextDecorationStyle);
    color!(color, TextDecorationColor);

    if let Some(thickness) = thickness {
      // Percentages in the text-decoration-thickness property are based on 1em.
      // If unsupported, compile this to a calc() instead.
      match thickness {
        TextDecorationThickness::LengthPercentage(LengthPercentage::Percentage(p))
          if should_compile!(context.targets, TextDecorationThicknessPercent) =>
        {
          let calc = Calc::Function(Box::new(MathFunction::Calc(Calc::Product(
            p.0,
            Box::new(Calc::Value(Box::new(LengthPercentage::Dimension(LengthValue::Em(1.0))))),
          ))));
          let thickness = TextDecorationThickness::LengthPercentage(LengthPercentage::Calc(Box::new(calc)));
          dest.push(Property::TextDecorationThickness(thickness));
        }
        thickness => dest.push(Property::TextDecorationThickness(thickness)),
      }
    }

    if let (Some((style, style_vp)), Some((color, color_vp))) = (&mut emphasis_style, &mut emphasis_color) {
      let intersection = *style_vp | *color_vp;
      if !intersection.is_empty() {
        let prefix = context.targets.prefixes(intersection, Feature::TextEmphasis);
        let mut emphasis = TextEmphasis {
          style: style.clone(),
          color: color.clone(),
        };

        if prefix.contains(VendorPrefix::None) {
          let fallbacks = emphasis.get_fallbacks(context.targets);
          for fallback in fallbacks {
            dest.push(Property::TextEmphasis(fallback, prefix))
          }
        }

        dest.push(Property::TextEmphasis(emphasis, prefix));
        style_vp.remove(intersection);
        color_vp.remove(intersection);
      }
    }

    single_property!(emphasis_style, TextEmphasisStyle);
    color!(emphasis_color, TextEmphasisColor);

    if let Some((pos, vp)) = emphasis_position {
      if !vp.is_empty() {
        let mut prefix = context.targets.prefixes(vp, Feature::TextEmphasisPosition);
        // Prefixed version does not support horizontal keyword.
        if pos.horizontal != TextEmphasisPositionHorizontal::Right {
          prefix = VendorPrefix::None;
        }
        dest.push(Property::TextEmphasisPosition(pos, prefix))
      }
    }
  }
}

/// A value for the [text-shadow](https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-shadow-property) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct TextShadow {
  /// The color of the text shadow.
  pub color: CssColor,
  /// The x offset of the text shadow.
  pub x_offset: Length,
  /// The y offset of the text shadow.
  pub y_offset: Length,
  /// The blur radius of the text shadow.
  pub blur: Length,
  /// The spread distance of the text shadow.
  pub spread: Length, // added in Level 4 spec
}

impl<'i> Parse<'i> for TextShadow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color = None;
    let mut lengths = None;

    loop {
      if lengths.is_none() {
        let value = input.try_parse::<_, _, ParseError<ParserError<'i>>>(|input| {
          let horizontal = Length::parse(input)?;
          let vertical = Length::parse(input)?;
          let blur = input.try_parse(Length::parse).unwrap_or(Length::zero());
          let spread = input.try_parse(Length::parse).unwrap_or(Length::zero());
          Ok((horizontal, vertical, blur, spread))
        });

        if let Ok(value) = value {
          lengths = Some(value);
          continue;
        }
      }

      if color.is_none() {
        if let Ok(value) = input.try_parse(CssColor::parse) {
          color = Some(value);
          continue;
        }
      }

      break;
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(TextShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2,
      spread: lengths.3,
    })
  }
}

impl ToCss for TextShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x_offset.to_css(dest)?;
    dest.write_char(' ')?;
    self.y_offset.to_css(dest)?;

    if self.blur != Length::zero() || self.spread != Length::zero() {
      dest.write_char(' ')?;
      self.blur.to_css(dest)?;

      if self.spread != Length::zero() {
        dest.write_char(' ')?;
        self.spread.to_css(dest)?;
      }
    }

    if self.color != CssColor::current_color() {
      dest.write_char(' ')?;
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}

impl IsCompatible for TextShadow {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.color.is_compatible(browsers)
      && self.x_offset.is_compatible(browsers)
      && self.y_offset.is_compatible(browsers)
      && self.blur.is_compatible(browsers)
      && self.spread.is_compatible(browsers)
  }
}

#[inline]
fn is_text_decoration_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::TextDecorationLine(_)
    | PropertyId::TextDecorationThickness
    | PropertyId::TextDecorationStyle(_)
    | PropertyId::TextDecorationColor(_)
    | PropertyId::TextDecoration(_) => true,
    _ => false,
  }
}

#[inline]
fn is_text_emphasis_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::TextEmphasisStyle(_)
    | PropertyId::TextEmphasisColor(_)
    | PropertyId::TextEmphasis(_)
    | PropertyId::TextEmphasisPosition(_) => true,
    _ => false,
  }
}

impl FallbackValues for SmallVec<[TextShadow; 1]> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    let mut fallbacks = ColorFallbackKind::empty();
    for shadow in self.iter() {
      fallbacks |= shadow.color.get_necessary_fallbacks(targets);
    }

    let mut res = Vec::new();
    if fallbacks.contains(ColorFallbackKind::RGB) {
      let rgb = self
        .iter()
        .map(|shadow| TextShadow {
          color: shadow.color.to_rgb(),
          ..shadow.clone()
        })
        .collect();
      res.push(rgb);
    }

    if fallbacks.contains(ColorFallbackKind::P3) {
      let p3 = self
        .iter()
        .map(|shadow| TextShadow {
          color: shadow.color.to_p3(),
          ..shadow.clone()
        })
        .collect();
      res.push(p3);
    }

    if fallbacks.contains(ColorFallbackKind::LAB) {
      for shadow in self.iter_mut() {
        shadow.color = shadow.color.to_lab();
      }
    }

    res
  }
}
