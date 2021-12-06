use cssparser::*;
use crate::macros::*;
use crate::values::{
  angle::Angle,
  length::LengthPercentage,
  percentage::Percentage
};
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::{Property, PropertyId};
use crate::declaration::DeclarationList;
use crate::printer::Printer;

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-weight-prop
#[derive(Debug, Clone, PartialEq)]
pub enum FontWeight {
  Absolute(AbsoluteFontWeight),
  Bolder,
  Lighter
}

impl Default for FontWeight {
  fn default() -> FontWeight {
    FontWeight::Absolute(AbsoluteFontWeight::default())
  }
}

impl Parse for FontWeight {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(val) = input.try_parse(AbsoluteFontWeight::parse) {
      return Ok(FontWeight::Absolute(val))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "bolder" => Ok(FontWeight::Bolder),
      "lighter" => Ok(FontWeight::Lighter),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for FontWeight {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use FontWeight::*;
    match self {
      Absolute(val) => val.to_css(dest),
      Bolder => dest.write_str("bolder"),
      Lighter => dest.write_str("lighter")
    }
  }
}

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-weight-absolute-values
#[derive(Debug, Clone, PartialEq)]
pub enum AbsoluteFontWeight {
  Weight(f32),
  Normal,
  Bold
}

impl Default for AbsoluteFontWeight {
  fn default() -> AbsoluteFontWeight {
    AbsoluteFontWeight::Normal
  }
}

impl Parse for AbsoluteFontWeight {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(val) = input.try_parse(f32::parse) {
      return Ok(AbsoluteFontWeight::Weight(val))
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "normal" => Ok(AbsoluteFontWeight::Normal),
      "bold" => Ok(AbsoluteFontWeight::Bold),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for AbsoluteFontWeight {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use AbsoluteFontWeight::*;
    match self {
      Weight(val) => val.to_css(dest),
      Normal => dest.write_str(if dest.minify { "400" } else { "normal" }),
      Bold => dest.write_str(if dest.minify { "700" } else { "bold" })
    }
  }
}

enum_property!(AbsoluteFontSize,
  ("xx-small", XXSmall),
  ("x-small", XSmall),
  ("small", Small),
  ("medium", Medium),
  ("large", Large),
  ("x-large", XLarge),
  ("xx-large", XXLarge)
);

enum_property!(RelativeFontSize,
  Smaller,
  Larger
);

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-size-prop
#[derive(Debug, Clone, PartialEq)]
pub enum FontSize {
  Length(LengthPercentage),
  Absolute(AbsoluteFontSize),
  Relative(RelativeFontSize)
}

impl Parse for FontSize {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(val) = input.try_parse(LengthPercentage::parse) {
      return Ok(FontSize::Length(val))
    }

    if let Ok(val) = input.try_parse(AbsoluteFontSize::parse) {
      return Ok(FontSize::Absolute(val))
    }

    let val = RelativeFontSize::parse(input)?;
    Ok(FontSize::Relative(val))
  }
}

impl ToCss for FontSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use FontSize::*;
    match self {
      Absolute(val) => val.to_css(dest),
      Length(val) => val.to_css(dest),
      Relative(val) => val.to_css(dest)
    }
  }
}

enum_property!(FontStretchKeyword,
  ("normal", Normal),
  ("ultra-condensed", UltraCondensed),
  ("extra-condensed", ExtraCondensed),
  ("condensed", Condensed),
  ("semi-condensed", SemiCondensed),
  ("semi-expanded", SemiExpanded),
  ("expanded", Expanded),
  ("extra-expanded", ExtraExpanded),
  ("ultra-expanded", UltraExpanded)
);

impl Default for FontStretchKeyword {
  fn default() -> FontStretchKeyword {
    FontStretchKeyword::Normal
  }
}

impl FontStretchKeyword {
  fn to_percentage(&self) -> Percentage {
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
      UltraExpanded => 2.0    
    };
    Percentage(val)
  }
}

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-stretch-prop
#[derive(Debug, Clone, PartialEq)]
pub enum FontStretch {
  Keyword(FontStretchKeyword),
  Percentage(Percentage)
}

impl Default for FontStretch {
  fn default() -> FontStretch {
    FontStretch::Keyword(FontStretchKeyword::default())
  }
}

impl Parse for FontStretch {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(val) = input.try_parse(Percentage::parse) {
      return Ok(FontStretch::Percentage(val))
    }

    let keyword = FontStretchKeyword::parse(input)?;
    Ok(FontStretch::Keyword(keyword))
  }
}

impl FontStretch {
  pub fn to_percentage(&self) -> Percentage {
    match self {
      FontStretch::Percentage(val) => val.clone(),
      FontStretch::Keyword(keyword) => keyword.to_percentage()
    }
  }
}

impl ToCss for FontStretch {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use FontStretch::*;
    if dest.minify {
      return self.to_percentage().to_css(dest)
    }

    match self {
      Percentage(val) => val.to_css(dest),
      Keyword(val) => val.to_css(dest)
    }
  }
}

// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#generic-font-families
enum_property!(GenericFontFamily,
  ("serif", Serif),
  ("sans-serif", SansSerif),
  ("cursive", Cursive),
  ("fantasy", Fantasy),
  ("monospace", Monospace),
  ("system-ui", SystemUI),
  ("emoji", Emoji),
  ("math", Math),
  ("fangsong", FangSong),
  ("ui-serif", UISerif),
  ("ui-sans-serif", UISansSerif),
  ("ui-monospace", UIMonospace),
  ("ui-rounded", UIRounded)
);

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-family-prop
#[derive(Debug, Clone, PartialEq)]
pub enum FontFamily {
  FamilyName(String),
  Generic(GenericFontFamily)
}

impl Parse for FontFamily {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(value) = input.try_parse(|i| i.expect_string_cloned()) {
      return Ok(FontFamily::FamilyName(value.as_ref().into()))
    }

    if let Ok(value) = input.try_parse(GenericFontFamily::parse) {
      return Ok(FontFamily::Generic(value))
    }

    let first_ident = input.expect_ident_cloned()?;
    let mut value = first_ident.as_ref().to_owned();
    while let Ok(ident) = input.try_parse(|i| i.expect_ident_cloned()) {
      value.push(' ');
      value.push_str(&ident);
    }

    Ok(FontFamily::FamilyName(value))
  }
}

impl ToCss for FontFamily {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      FontFamily::Generic(val) => val.to_css(dest),
      FontFamily::FamilyName(val) => {
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
          return dest.write_str(&id)
        }
        serialize_string(&val, dest)
      }
    }
  }
}

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-style-prop
#[derive(Debug, Clone, PartialEq)]
pub enum FontStyle {
  Normal,
  Italic,
  Oblique(Angle)
}

impl Default for FontStyle {
  fn default() -> FontStyle {
    FontStyle::Normal
  }
}

impl Parse for FontStyle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "normal" => Ok(FontStyle::Normal),
      "italic" => Ok(FontStyle::Italic),
      "oblique" => {
        let angle = input.try_parse(Angle::parse).unwrap_or(Angle::Deg(14.0));
        Ok(FontStyle::Oblique(angle))
      },
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for FontStyle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      FontStyle::Normal => dest.write_str("normal"),
      FontStyle::Italic => dest.write_str("italic"),
      FontStyle::Oblique(angle) => {
        dest.write_str("oblique")?;
        if *angle != Angle::Deg(14.0) {
          dest.write_char(' ')?;
          angle.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-variant-caps-prop
enum_property!(FontVariantCaps,
  ("normal", Normal),
  ("small-caps", SmallCaps),
  ("all-small-caps", AllSmallCaps),
  ("petite-caps", PetiteCaps),
  ("all-petite-caps", AllPetiteCaps),
  ("unicase", Unicase),
  ("titling-caps", TitlingCaps)
);

impl FontVariantCaps {
  pub fn to_css2(&self) -> Option<FontVariantCapsCSS2> {
    match self {
      FontVariantCaps::Normal => Some(FontVariantCapsCSS2::Normal),
      FontVariantCaps::SmallCaps => Some(FontVariantCapsCSS2::SmallCaps),
      _ => None
    }
  }
}

// The `font` property only supports font-variant-caps values from CSS 2.1
enum_property!(FontVariantCapsCSS2,
  ("normal", Normal),
  ("small-caps", SmallCaps)
);

impl Default for FontVariantCapsCSS2 {
  fn default() -> FontVariantCapsCSS2 {
    FontVariantCapsCSS2::Normal
  }
}

impl FontVariantCapsCSS2 {
  pub fn to_font_variant_caps(&self) -> FontVariantCaps {
    match self {
      FontVariantCapsCSS2::Normal => FontVariantCaps::Normal,
      FontVariantCapsCSS2::SmallCaps => FontVariantCaps::SmallCaps
    }
  }
}

/// https://www.w3.org/TR/2020/WD-css-inline-3-20200827/#propdef-line-height
#[derive(Debug, Clone, PartialEq)]
pub enum LineHeight {
  Normal,
  Number(f32),
  Length(LengthPercentage)
}

impl Default for LineHeight {
  fn default() -> LineHeight {
    LineHeight::Normal
  }
}

impl Parse for LineHeight {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(LineHeight::Normal)
    }

    if let Ok(val) = input.try_parse(f32::parse) {
      return Ok(LineHeight::Number(val))
    }

    Ok(LineHeight::Length(LengthPercentage::parse(input)?))
  }
}

impl ToCss for LineHeight {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      LineHeight::Normal => dest.write_str("normal"),
      LineHeight::Number(val) => val.to_css(dest),
      LineHeight::Length(val) => val.to_css(dest)
    }
  }
}

enum_property!(VerticalAlignKeyword,
  ("baseline", Baseline),
  ("sub", Sub),
  ("super", Super),
  ("top", Top),
  ("text-top", TextTop),
  ("middle", Middle),
  ("bottom", Bottom),
  ("text-bottom", TextBottom)
);

/// https://drafts.csswg.org/css2/#propdef-vertical-align
// TODO: there is a more extensive spec in CSS3 but it doesn't seem any browser implements it? https://www.w3.org/TR/css-inline-3/#transverse-alignment
#[derive(Debug, Clone, PartialEq)]
pub enum VerticalAlign {
  Keyword(VerticalAlignKeyword),
  Length(LengthPercentage)
}

impl Parse for VerticalAlign {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(len) = input.try_parse(LengthPercentage::parse) {
      return Ok(VerticalAlign::Length(len))
    }

    let kw = VerticalAlignKeyword::parse(input)?;
    Ok(VerticalAlign::Keyword(kw))
  }
}

impl ToCss for VerticalAlign {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      VerticalAlign::Keyword(kw) => kw.to_css(dest),
      VerticalAlign::Length(len) => len.to_css(dest)
    }
  }
}

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-prop
#[derive(Debug, Clone, PartialEq)]
pub struct Font {
  pub family: Vec<FontFamily>,
  pub size: FontSize,
  pub style: FontStyle,
  pub weight: FontWeight,
  pub stretch: FontStretch,
  pub line_height: LineHeight,
  pub variant_caps: FontVariantCapsCSS2
}

impl Parse for Font {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut style = None;
    let mut weight = None;
    let mut stretch = None;
    let size;
    let mut variant_caps = None;

    loop {
      if style.is_none() {
        if let Ok(value) = input.try_parse(FontStyle::parse) {
          style = Some(value);
          continue
        }
      }
      if weight.is_none() {
        if let Ok(value) = input.try_parse(FontWeight::parse) {
          weight = Some(value);
          continue
        }
      }
      if variant_caps.is_none() {
        if let Ok(value) = input.try_parse(FontVariantCapsCSS2::parse) {
          variant_caps = Some(value);
          continue
        }
      }

      if stretch.is_none() {
        if let Ok(value) = input.try_parse(FontStretchKeyword::parse) {
          stretch = Some(FontStretch::Keyword(value));
          continue
        }
      }
      size = Some(FontSize::parse(input)?);
      break
    }

    let size = match size {
      Some(s) => s,
      None => {
        return  Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
      }
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
      variant_caps: variant_caps.unwrap_or_default()
    })
  }
}

impl ToCss for Font {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if self.style != FontStyle::default() {
      self.style.to_css(dest)?;
      dest.write_char(' ')?;
    }

    if self.variant_caps != FontVariantCapsCSS2::default() {
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

#[derive(Default, Debug)]
pub(crate) struct FontHandler {
  family: Option<Vec<FontFamily>>,
  size: Option<FontSize>,
  style: Option<FontStyle>,
  weight: Option<FontWeight>,
  stretch: Option<FontStretch>,
  line_height: Option<LineHeight>,
  variant_caps: Option<FontVariantCaps>,
  has_any: bool
}

impl PropertyHandler for FontHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;

    macro_rules! property {
      ($prop: ident, $val: ident) => {{
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
        self.family = Some(val.family.clone());
        self.size = Some(val.size.clone());
        self.style = Some(val.style.clone());
        self.weight = Some(val.weight.clone());
        self.stretch = Some(val.stretch.clone());
        self.line_height = Some(val.line_height.clone());
        self.variant_caps = Some(val.variant_caps.to_font_variant_caps());
        self.has_any = true;
        // TODO: reset other properties
      }
      Unparsed(val) if is_font_property(&val.property_id) => {
        self.finalize(dest);
        dest.push(property.clone());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, decls: &mut DeclarationList) {
    if !self.has_any {
      return
    }

    self.has_any = false;

    let family = std::mem::take(&mut self.family);
    let size = std::mem::take(&mut self.size);
    let style = std::mem::take(&mut self.style);
    let weight = std::mem::take(&mut self.weight);
    let stretch = std::mem::take(&mut self.stretch);
    let line_height = std::mem::take(&mut self.line_height);
    let variant_caps = std::mem::take(&mut self.variant_caps);

    if family.is_some() && size.is_some() && style.is_some() && weight.is_some() && stretch.is_some() && line_height.is_some() && variant_caps.is_some() {
      let caps = variant_caps.unwrap().to_css2();
      decls.push(Property::Font(Font {
        family: family.unwrap(),
        size: size.unwrap(),
        style: style.unwrap(),
        weight: weight.unwrap(),
        stretch: stretch.unwrap(),
        line_height: line_height.unwrap(),
        variant_caps: caps.unwrap_or_default()
      }));

      // The `font` property only accepts CSS 2.1 values for font-variant caps.
      // If we have a CSS 3+ value, we need to add a separate property.
      if caps == None {
        decls.push(Property::FontVariantCaps(variant_caps.unwrap()))
      }
    } else {
      if let Some(val) = family {
        decls.push(Property::FontFamily(val))
      }

      if let Some(val) = size {
        decls.push(Property::FontSize(val))
      }

      if let Some(val) = style {
        decls.push(Property::FontStyle(val))
      }

      if let Some(val) = variant_caps {
        decls.push(Property::FontVariantCaps(val))
      }

      if let Some(val) = weight {
        decls.push(Property::FontWeight(val))
      }

      if let Some(val) = stretch {
        decls.push(Property::FontStretch(val))
      }

      if let Some(val) = line_height {
        decls.push(Property::LineHeight(val))
      }
    }
  }
}

#[inline]
fn is_font_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::FontFamily |
    PropertyId::FontSize |
    PropertyId::FontStyle |
    PropertyId::FontWeight |
    PropertyId::FontStretch |
    PropertyId::FontVariantCaps |
    PropertyId::LineHeight |
    PropertyId::Font => true,
    _ => false
  }
}
