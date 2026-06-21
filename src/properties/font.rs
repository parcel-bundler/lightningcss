//! CSS properties related to fonts.

use std::collections::HashSet;

use super::{Property, PropertyId};
use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::*;
use crate::printer::Printer;
use crate::targets::should_compile;
use crate::traits::{IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::length::LengthValue;
use crate::values::number::CSSNumber;
use crate::values::string::CowArcStr;
use crate::values::{angle::Angle, length::LengthPercentage, percentage::Percentage};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A value for the [font-weight](https://www.w3.org/TR/css-fonts-4/#font-weight-prop) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum FontWeight {
  /// An absolute font weight.
  Absolute(AbsoluteFontWeight),
  /// The `bolder` keyword.
  Bolder,
  /// The `lighter` keyword.
  Lighter,
}

impl Default for FontWeight {
  fn default() -> FontWeight {
    FontWeight::Absolute(AbsoluteFontWeight::default())
  }
}

impl IsCompatible for FontWeight {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      FontWeight::Absolute(a) => a.is_compatible(browsers),
      FontWeight::Bolder | FontWeight::Lighter => true,
    }
  }
}

/// An [absolute font weight](https://www.w3.org/TR/css-fonts-4/#font-weight-absolute-values),
/// as used in the `font-weight` property.
///
/// See [FontWeight](FontWeight).
#[derive(Debug, Clone, PartialEq, Parse)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum AbsoluteFontWeight {
  /// An explicit weight.
  Weight(CSSNumber),
  /// Same as `400`.
  Normal,
  /// Same as `700`.
  Bold,
}

impl Default for AbsoluteFontWeight {
  fn default() -> AbsoluteFontWeight {
    AbsoluteFontWeight::Normal
  }
}

impl ToCss for AbsoluteFontWeight {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use AbsoluteFontWeight::*;
    match self {
      Weight(val) => val.to_css(dest),
      Normal => dest.write_str(if dest.minify { "400" } else { "normal" }),
      Bold => dest.write_str(if dest.minify { "700" } else { "bold" }),
    }
  }
}

impl IsCompatible for AbsoluteFontWeight {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      // Older browsers only supported 100, 200, 300, ...900 rather than arbitrary values.
      AbsoluteFontWeight::Weight(val) if !(*val >= 100.0 && *val <= 900.0 && *val % 100.0 == 0.0) => {
        Feature::FontWeightNumber.is_compatible(browsers)
      }
      _ => true,
    }
  }
}

enum_property! {
  /// An [absolute font size](https://www.w3.org/TR/css-fonts-3/#absolute-size-value),
  /// as used in the `font-size` property.
  ///
  /// See [FontSize](FontSize).
  #[allow(missing_docs)]
  pub enum AbsoluteFontSize {
    "xx-small": XXSmall,
    "x-small": XSmall,
    "small": Small,
    "medium": Medium,
    "large": Large,
    "x-large": XLarge,
    "xx-large": XXLarge,
    "xxx-large": XXXLarge,
  }
}

impl IsCompatible for AbsoluteFontSize {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    use AbsoluteFontSize::*;
    match self {
      XXXLarge => Feature::FontSizeXXXLarge.is_compatible(browsers),
      _ => true,
    }
  }
}

enum_property! {
  /// A [relative font size](https://www.w3.org/TR/css-fonts-3/#relative-size-value),
  /// as used in the `font-size` property.
  ///
  /// See [FontSize](FontSize).
  #[allow(missing_docs)]
  pub enum RelativeFontSize {
    Smaller,
    Larger,
  }
}

/// A value for the [font-size](https://www.w3.org/TR/css-fonts-4/#font-size-prop) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum FontSize {
  /// An explicit size.
  Length(LengthPercentage),
  /// An absolute font size keyword.
  Absolute(AbsoluteFontSize),
  /// A relative font size keyword.
  Relative(RelativeFontSize),
}

impl IsCompatible for FontSize {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      FontSize::Length(LengthPercentage::Dimension(LengthValue::Rem(..))) => {
        Feature::FontSizeRem.is_compatible(browsers)
      }
      FontSize::Length(l) => l.is_compatible(browsers),
      FontSize::Absolute(a) => a.is_compatible(browsers),
      FontSize::Relative(..) => true,
    }
  }
}

enum_property! {
  /// A [font stretch keyword](https://www.w3.org/TR/css-fonts-4/#font-stretch-prop),
  /// as used in the `font-stretch` property.
  ///
  /// See [FontStretch](FontStretch).
  pub enum FontStretchKeyword {
    /// 100%
    "normal": Normal,
    /// 50%
    "ultra-condensed": UltraCondensed,
    /// 62.5%
    "extra-condensed": ExtraCondensed,
    /// 75%
    "condensed": Condensed,
    /// 87.5%
    "semi-condensed": SemiCondensed,
    /// 112.5%
    "semi-expanded": SemiExpanded,
    /// 125%
    "expanded": Expanded,
    /// 150%
    "extra-expanded": ExtraExpanded,
    /// 200%
    "ultra-expanded": UltraExpanded,
  }
}

impl Default for FontStretchKeyword {
  fn default() -> FontStretchKeyword {
    FontStretchKeyword::Normal
  }
}

impl Into<Percentage> for &FontStretchKeyword {
  fn into(self) -> Percentage {
    use FontStretchKeyword::*;
    let val = match self {
      UltraCondensed => 0.5,
      ExtraCondensed => 0.625,
      Condensed => 0.75,
      SemiCondensed => 0.875,
      Normal => 1.0,
      SemiExpanded => 1.125,
      Expanded => 1.25,
      ExtraExpanded => 1.5,
      UltraExpanded => 2.0,
    };
    Percentage(val)
  }
}

/// A value for the [font-stretch](https://www.w3.org/TR/css-fonts-4/#font-stretch-prop) property.
#[derive(Debug, Clone, PartialEq, Parse)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum FontStretch {
  /// A font stretch keyword.
  Keyword(FontStretchKeyword),
  /// A percentage.
  Percentage(Percentage),
}

impl Default for FontStretch {
  fn default() -> FontStretch {
    FontStretch::Keyword(FontStretchKeyword::default())
  }
}

impl Into<Percentage> for &FontStretch {
  fn into(self) -> Percentage {
    match self {
      FontStretch::Percentage(val) => val.clone(),
      FontStretch::Keyword(keyword) => keyword.into(),
    }
  }
}

impl ToCss for FontStretch {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if dest.minify {
      let percentage: Percentage = self.into();
      return percentage.to_css(dest);
    }

    match self {
      FontStretch::Percentage(val) => val.to_css(dest),
      FontStretch::Keyword(val) => val.to_css(dest),
    }
  }
}

impl IsCompatible for FontStretch {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      FontStretch::Percentage(..) => Feature::FontStretchPercentage.is_compatible(browsers),
      FontStretch::Keyword(..) => true,
    }
  }
}

enum_property! {
  /// A [generic font family](https://www.w3.org/TR/css-fonts-4/#generic-font-families) name,
  /// as used in the `font-family` property.
  ///
  /// See [FontFamily](FontFamily).
  #[allow(missing_docs)]
  #[derive(Eq, Hash)]
  pub enum GenericFontFamily {
    "serif": Serif,
    "sans-serif": SansSerif,
    "cursive": Cursive,
    "fantasy": Fantasy,
    "monospace": Monospace,
    "system-ui": SystemUI,
    "emoji": Emoji,
    "math": Math,
    "fangsong": FangSong,
    "ui-serif": UISerif,
    "ui-sans-serif": UISansSerif,
    "ui-monospace": UIMonospace,
    "ui-rounded": UIRounded,

    // CSS wide keywords. These must be parsed as identifiers so they
    // don't get serialized as strings.
    // https://www.w3.org/TR/css-values-4/#common-keywords
    "initial": Initial,
    "inherit": Inherit,
    "unset": Unset,
    // Default is also reserved by the <custom-ident> type.
    // https://www.w3.org/TR/css-values-4/#custom-idents
    "default": Default,

    // CSS defaulting keywords
    // https://drafts.csswg.org/css-cascade-5/#defaulting-keywords
    "revert": Revert,
    "revert-layer": RevertLayer,
  }
}

impl IsCompatible for GenericFontFamily {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    use GenericFontFamily::*;
    match self {
      SystemUI => Feature::FontFamilySystemUi.is_compatible(browsers),
      UISerif | UISansSerif | UIMonospace | UIRounded => Feature::ExtendedSystemFonts.is_compatible(browsers),
      _ => true,
    }
  }
}

/// A value for the [font-family](https://www.w3.org/TR/css-fonts-4/#font-family-prop) property.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(untagged))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum FontFamily<'i> {
  /// A generic family name.
  Generic(GenericFontFamily),
  /// A custom family name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  FamilyName(FamilyName<'i>),
}

impl<'i> Parse<'i> for FontFamily<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(value) = input.try_parse(GenericFontFamily::parse) {
      return Ok(FontFamily::Generic(value));
    }

    let family = FamilyName::parse(input)?;
    Ok(FontFamily::FamilyName(family))
  }
}

impl<'i> ToCss for FontFamily<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      FontFamily::Generic(val) => val.to_css(dest),
      FontFamily::FamilyName(val) => val.to_css(dest),
    }
  }
}

/// A font [family name](https://drafts.csswg.org/css-fonts/#family-name-syntax).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct FamilyName<'i>(#[cfg_attr(feature = "serde", serde(borrow))] CowArcStr<'i>);

impl<'i> Parse<'i> for FamilyName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(value) = input.try_parse(|i| i.expect_string_cloned()) {
      return Ok(FamilyName(value.into()));
    }

    let value: CowArcStr<'i> = input.expect_ident()?.into();
    let mut string = None;
    while let Ok(ident) = input.try_parse(|i| i.expect_ident_cloned()) {
      if string.is_none() {
        string = Some(value.to_string());
      }

      if let Some(string) = &mut string {
        string.push(' ');
        string.push_str(&ident);
      }
    }

    let value = if let Some(string) = string {
      string.into()
    } else {
      value
    };

    Ok(FamilyName(value))
  }
}

impl<'i> ToCss for FamilyName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    // Generic family names such as sans-serif must be quoted if parsed as a string.
    // CSS wide keywords, as well as "default", must also be quoted.
    // https://www.w3.org/TR/css-fonts-4/#family-name-syntax
    let val = &self.0;
    if !val.is_empty() && !GenericFontFamily::parse_string(val).is_ok() {
      // Family names with two or more consecutive spaces must be quoted to preserve the spaces.
      let needs_quotes = val.contains("  ");
      if needs_quotes {
        serialize_string(&val, dest)?;
        return Ok(());
      }

      let mut id = String::new();
      let mut first = true;
      for slice in val.split(' ') {
        if first {
          first = false;
        } else {
          id.push(' ');
        }
        serialize_identifier(slice, &mut id)?;
      }
      if id.len() < val.len() + 2 {
        return dest.write_str(&id);
      }
    }
    serialize_string(&val, dest)?;
    Ok(())
  }
}

impl IsCompatible for FontFamily<'_> {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      FontFamily::Generic(g) => g.is_compatible(browsers),
      FontFamily::FamilyName(..) => true,
    }
  }
}

/// A value for the [font-style](https://www.w3.org/TR/css-fonts-4/#font-style-prop) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum FontStyle {
  /// Normal font style.
  Normal,
  /// Italic font style.
  Italic,
  /// Oblique font style, with a custom angle.
  Oblique(#[cfg_attr(feature = "serde", serde(default = "FontStyle::default_oblique_angle"))] Angle),
}

impl Default for FontStyle {
  fn default() -> FontStyle {
    FontStyle::Normal
  }
}

impl FontStyle {
  #[inline]
  pub(crate) fn default_oblique_angle() -> Angle {
    Angle::Deg(14.0)
  }
}

impl<'i> Parse<'i> for FontStyle {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "normal" => Ok(FontStyle::Normal),
      "italic" => Ok(FontStyle::Italic),
      "oblique" => {
        let angle = input.try_parse(Angle::parse).unwrap_or(FontStyle::default_oblique_angle());
        Ok(FontStyle::Oblique(angle))
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for FontStyle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      FontStyle::Normal => dest.write_str("normal"),
      FontStyle::Italic => dest.write_str("italic"),
      FontStyle::Oblique(angle) => {
        dest.write_str("oblique")?;
        if *angle != FontStyle::default_oblique_angle() {
          dest.write_char(' ')?;
          angle.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl IsCompatible for FontStyle {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      FontStyle::Oblique(angle) if *angle != FontStyle::default_oblique_angle() => {
        Feature::FontStyleObliqueAngle.is_compatible(browsers)
      }
      FontStyle::Normal | FontStyle::Italic | FontStyle::Oblique(..) => true,
    }
  }
}

enum_property! {
  /// A value for the [font-variant-caps](https://www.w3.org/TR/css-fonts-4/#font-variant-caps-prop) property.
  pub enum FontVariantCaps {
    /// No special capitalization features are applied.
    Normal,
    /// The small capitals feature is used for lower case letters.
    SmallCaps,
    /// Small capitals are used for both upper and lower case letters.
    AllSmallCaps,
    /// Petite capitals are used.
    PetiteCaps,
    /// Petite capitals are used for both upper and lower case letters.
    AllPetiteCaps,
    /// Enables display of mixture of small capitals for uppercase letters with normal lowercase letters.
    Unicase,
    /// Uses titling capitals.
    TitlingCaps,
  }
}

impl Default for FontVariantCaps {
  fn default() -> FontVariantCaps {
    FontVariantCaps::Normal
  }
}

impl FontVariantCaps {
  fn is_css2(&self) -> bool {
    matches!(self, FontVariantCaps::Normal | FontVariantCaps::SmallCaps)
  }

  fn parse_css2<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = Self::parse(input)?;
    if !value.is_css2() {
      return Err(input.new_custom_error(ParserError::InvalidValue));
    }
    Ok(value)
  }
}

impl IsCompatible for FontVariantCaps {
  fn is_compatible(&self, _browsers: crate::targets::Browsers) -> bool {
    true
  }
}

/// A value for the [line-height](https://www.w3.org/TR/2020/WD-css-inline-3-20200827/#propdef-line-height) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum LineHeight {
  /// The UA sets the line height based on the font.
  Normal,
  /// A multiple of the element's font size.
  Number(CSSNumber),
  /// An explicit height.
  Length(LengthPercentage),
}

impl Default for LineHeight {
  fn default() -> LineHeight {
    LineHeight::Normal
  }
}

impl IsCompatible for LineHeight {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    match self {
      LineHeight::Length(l) => l.is_compatible(browsers),
      LineHeight::Normal | LineHeight::Number(..) => true,
    }
  }
}

enum_property! {
  /// A keyword for the [vertical align](https://drafts.csswg.org/css2/#propdef-vertical-align) property.
  pub enum VerticalAlignKeyword {
    /// Align the baseline of the box with the baseline of the parent box.
    Baseline,
    /// Lower the baseline of the box to the proper position for subscripts of the parent’s box.
    Sub,
    /// Raise the baseline of the box to the proper position for superscripts of the parent’s box.
    Super,
    /// Align the top of the aligned subtree with the top of the line box.
    Top,
    /// Align the top of the box with the top of the parent’s content area.
    TextTop,
    /// Align the vertical midpoint of the box with the baseline of the parent box plus half the x-height of the parent.
    Middle,
    /// Align the bottom of the aligned subtree with the bottom of the line box.
    Bottom,
    /// Align the bottom of the box with the bottom of the parent’s content area.
    TextBottom,
  }
}

/// A value for the [vertical align](https://drafts.csswg.org/css2/#propdef-vertical-align) property.
// TODO: there is a more extensive spec in CSS3 but it doesn't seem any browser implements it? https://www.w3.org/TR/css-inline-3/#transverse-alignment
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum VerticalAlign {
  /// A vertical align keyword.
  Keyword(VerticalAlignKeyword),
  /// An explicit length.
  Length(LengthPercentage),
}

define_shorthand! {
  /// A value for the [font](https://www.w3.org/TR/css-fonts-4/#font-prop) shorthand property.
  pub struct Font<'i> {
    /// The font family.
    #[cfg_attr(feature = "serde", serde(borrow))]
    family: FontFamily(Vec<FontFamily<'i>>),
    /// The font size.
    size: FontSize(FontSize),
    /// The font style.
    style: FontStyle(FontStyle),
    /// The font weight.
    weight: FontWeight(FontWeight),
    /// The font stretch.
    stretch: FontStretch(FontStretch),
    /// The line height.
    line_height: LineHeight(LineHeight),
    /// How the text should be capitalized. Only CSS 2.1 values are supported.
    variant_caps: FontVariantCaps(FontVariantCaps),
  }
}

impl<'i> Parse<'i> for Font<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut style = None;
    let mut weight = None;
    let mut stretch = None;
    let size;
    let mut variant_caps = None;
    let mut count = 0;

    loop {
      // Skip "normal" since it is valid for several properties, but we don't know which ones it will be used for yet.
      if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
        count += 1;
        continue;
      }
      if style.is_none() {
        if let Ok(value) = input.try_parse(FontStyle::parse) {
          style = Some(value);
          count += 1;
          continue;
        }
      }
      if weight.is_none() {
        if let Ok(value) = input.try_parse(FontWeight::parse) {
          weight = Some(value);
          count += 1;
          continue;
        }
      }
      if variant_caps.is_none() {
        if let Ok(value) = input.try_parse(FontVariantCaps::parse_css2) {
          variant_caps = Some(value);
          count += 1;
          continue;
        }
      }

      if stretch.is_none() {
        if let Ok(value) = input.try_parse(FontStretchKeyword::parse) {
          stretch = Some(FontStretch::Keyword(value));
          count += 1;
          continue;
        }
      }
      size = Some(FontSize::parse(input)?);
      break;
    }

    if count > 4 {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }

    let size = match size {
      Some(s) => s,
      None => return Err(input.new_custom_error(ParserError::InvalidDeclaration)),
    };

    let line_height = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      Some(LineHeight::parse(input)?)
    } else {
      None
    };

    let family = input.parse_comma_separated(FontFamily::parse)?;
    Ok(Font {
      family,
      size,
      style: style.unwrap_or_default(),
      weight: weight.unwrap_or_default(),
      stretch: stretch.unwrap_or_default(),
      line_height: line_height.unwrap_or_default(),
      variant_caps: variant_caps.unwrap_or_default(),
    })
  }
}

impl<'i> ToCss for Font<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.style != FontStyle::default() {
      self.style.to_css(dest)?;
      dest.write_char(' ')?;
    }

    if self.variant_caps != FontVariantCaps::default() {
      self.variant_caps.to_css(dest)?;
      dest.write_char(' ')?;
    }

    if self.weight != FontWeight::default() {
      self.weight.to_css(dest)?;
      dest.write_char(' ')?;
    }

    if self.stretch != FontStretch::default() {
      self.stretch.to_css(dest)?;
      dest.write_char(' ')?;
    }

    self.size.to_css(dest)?;

    if self.line_height != LineHeight::default() {
      dest.delim('/', true)?;
      self.line_height.to_css(dest)?;
    }

    dest.write_char(' ')?;

    let len = self.family.len();
    for (idx, val) in self.family.iter().enumerate() {
      val.to_css(dest)?;
      if idx < len - 1 {
        dest.delim(',', false)?;
      }
    }

    Ok(())
  }
}

property_bitflags! {
  #[derive(Default, Debug)]
  struct FontProperty: u8 {
    const FontFamily = 1 << 0;
    const FontSize = 1 << 1;
    const FontStyle = 1 << 2;
    const FontWeight = 1 << 3;
    const FontStretch = 1 << 4;
    const LineHeight = 1 << 5;
    const FontVariantCaps = 1 << 6;
    const Font = Self::FontFamily.bits() | Self::FontSize.bits() | Self::FontStyle.bits() | Self::FontWeight.bits() | Self::FontStretch.bits() | Self::LineHeight.bits() | Self::FontVariantCaps.bits();
  }
}

#[derive(Default, Debug)]
pub(crate) struct FontHandler<'i> {
  family: Option<Vec<FontFamily<'i>>>,
  size: Option<FontSize>,
  style: Option<FontStyle>,
  weight: Option<FontWeight>,
  stretch: Option<FontStretch>,
  line_height: Option<LineHeight>,
  variant_caps: Option<FontVariantCaps>,
  flushed_properties: FontProperty,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for FontHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    macro_rules! flush {
      ($prop: ident, $val: expr) => {{
        if self.$prop.is_some() && self.$prop.as_ref().unwrap() != $val && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets)) {
          self.flush(dest, context);
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: ident) => {{
        flush!($prop, $val);
        self.$prop = Some($val.clone());
        self.has_any = true;
      }};
    }

    match property {
      FontFamily(val) => property!(family, val),
      FontSize(val) => property!(size, val),
      FontStyle(val) => property!(style, val),
      FontWeight(val) => property!(weight, val),
      FontStretch(val) => property!(stretch, val),
      FontVariantCaps(val) => property!(variant_caps, val),
      LineHeight(val) => property!(line_height, val),
      Font(val) => {
        flush!(family, &val.family);
        flush!(size, &val.size);
        flush!(style, &val.style);
        flush!(weight, &val.weight);
        flush!(stretch, &val.stretch);
        flush!(line_height, &val.line_height);
        flush!(variant_caps, &val.variant_caps);
        self.family = Some(val.family.clone());
        self.size = Some(val.size.clone());
        self.style = Some(val.style.clone());
        self.weight = Some(val.weight.clone());
        self.stretch = Some(val.stretch.clone());
        self.line_height = Some(val.line_height.clone());
        self.variant_caps = Some(val.variant_caps.clone());
        self.has_any = true;
        // TODO: reset other properties
      }
      Unparsed(val) if is_font_property(&val.property_id) => {
        self.flush(dest, context);
        self
          .flushed_properties
          .insert(FontProperty::try_from(&val.property_id).unwrap());
        dest.push(property.clone());
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, decls: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(decls, context);
    self.flushed_properties = FontProperty::empty();
  }
}

impl<'i> FontHandler<'i> {
  fn flush(&mut self, decls: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    macro_rules! push {
      ($prop: ident, $val: expr) => {
        decls.push(Property::$prop($val));
        self.flushed_properties.insert(FontProperty::$prop);
      };
    }

    let mut family = std::mem::take(&mut self.family);
    if !self.flushed_properties.contains(FontProperty::FontFamily) {
      family = compatible_font_family(family, !should_compile!(context.targets, FontFamilySystemUi));
    }
    let size = std::mem::take(&mut self.size);
    let style = std::mem::take(&mut self.style);
    let weight = std::mem::take(&mut self.weight);
    let stretch = std::mem::take(&mut self.stretch);
    let line_height = std::mem::take(&mut self.line_height);
    let variant_caps = std::mem::take(&mut self.variant_caps);

    if let Some(family) = &mut family {
      if family.len() > 1 {
        // Dedupe.
        let mut seen = HashSet::new();
        family.retain(|f| seen.insert(f.clone()));
      }
    }

    if family.is_some()
      && size.is_some()
      && style.is_some()
      && weight.is_some()
      && stretch.is_some()
      && line_height.is_some()
      && variant_caps.is_some()
    {
      let caps = variant_caps.unwrap();
      push!(
        Font,
        Font {
          family: family.unwrap(),
          size: size.unwrap(),
          style: style.unwrap(),
          weight: weight.unwrap(),
          stretch: stretch.unwrap(),
          line_height: line_height.unwrap(),
          variant_caps: if caps.is_css2() {
            caps
          } else {
            FontVariantCaps::default()
          },
        }
      );

      // The `font` property only accepts CSS 2.1 values for font-variant caps.
      // If we have a CSS 3+ value, we need to add a separate property.
      if !caps.is_css2() {
        push!(FontVariantCaps, variant_caps.unwrap());
      }
    } else {
      if let Some(val) = family {
        push!(FontFamily, val);
      }

      if let Some(val) = size {
        push!(FontSize, val);
      }

      if let Some(val) = style {
        push!(FontStyle, val);
      }

      if let Some(val) = variant_caps {
        push!(FontVariantCaps, val);
      }

      if let Some(val) = weight {
        push!(FontWeight, val);
      }

      if let Some(val) = stretch {
        push!(FontStretch, val);
      }

      if let Some(val) = line_height {
        push!(LineHeight, val);
      }
    }
  }
}

const SYSTEM_UI: FontFamily = FontFamily::Generic(GenericFontFamily::SystemUI);

const DEFAULT_SYSTEM_FONTS: &[&str] = &[
  // #1: Supported as the '-apple-system' value (macOS, Safari >= 9.2 < 11, Firefox >= 43)
  "-apple-system",
  // #2: Supported as the 'BlinkMacSystemFont' value (macOS, Chrome < 56)
  "BlinkMacSystemFont",
  "Segoe UI",  // Windows >= Vista
  "Roboto",    // Android >= 4
  "Noto Sans", // Plasma >= 5.5
  "Ubuntu",    // Ubuntu >= 10.10
  "Cantarell", // GNOME >= 3
  "Helvetica Neue",
];

/// [`system-ui`](https://www.w3.org/TR/css-fonts-4/#system-ui-def) is a special generic font family
/// It is platform dependent but if not supported by the target will simply be ignored
/// This list is an attempt at providing that support
#[inline]
fn compatible_font_family(mut family: Option<Vec<FontFamily>>, is_supported: bool) -> Option<Vec<FontFamily>> {
  if is_supported {
    return family;
  }

  if let Some(families) = &mut family {
    if let Some(position) = families.iter().position(|v| *v == SYSTEM_UI) {
      families.splice(
        (position + 1)..(position + 1),
        DEFAULT_SYSTEM_FONTS
          .iter()
          .map(|name| FontFamily::FamilyName(FamilyName(CowArcStr::from(*name)))),
      );
    }
  }

  return family;
}

#[inline]
fn is_font_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::FontFamily
    | PropertyId::FontSize
    | PropertyId::FontStyle
    | PropertyId::FontWeight
    | PropertyId::FontStretch
    | PropertyId::FontVariantCaps
    | PropertyId::LineHeight
    | PropertyId::Font => true,
    _ => false,
  }
}
