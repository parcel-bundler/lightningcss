use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::{Property, VendorPrefix};
use crate::declaration::DeclarationList;
use super::prefixes::{Browsers, Feature};
use crate::macros::enum_property;
use crate::values::length::{Length, LengthPercentage};
use crate::values::color::CssColor;
use crate::printer::Printer;
use bitflags::bitflags;
use std::fmt::Write;

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-transform-property
enum_property!(TextTransformCase,
  None,
  Uppercase,
  Lowercase,
  Capitalize
);

impl Default for TextTransformCase {
  fn default() -> TextTransformCase {
    TextTransformCase::None
  }
}

bitflags! {
  pub struct TextTransformOther: u8 {
    const FullWidth    = 0b00000001;
    const FullSizeKana = 0b00000010;
  }
}

impl Parse for TextTransformOther {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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

#[derive(Debug, Clone, PartialEq)]
pub struct TextTransform {
  case: TextTransformCase,
  other: TextTransformOther
}

impl Parse for TextTransform {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut case = None;
    let mut other = TextTransformOther::empty();

    loop {
      if case.is_none() {
        if let Ok(c) = input.try_parse(TextTransformCase::parse) {
          case = Some(c);
          if c == TextTransformCase::None {
            other = TextTransformOther::empty();
            break
          }
          continue
        }
      }

      if let Ok(o) = input.try_parse(TextTransformOther::parse) {
        other |= o;
        continue
      }

      break
    }

    Ok(TextTransform {
      case: case.unwrap_or_default(),
      other
    })
  }
}

impl ToCss for TextTransform {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#white-space-property
enum_property!(WhiteSpace,
  ("normal", Normal),
  ("pre", Pre),
  ("nowrap", NoWrap),
  ("pre-wrap", PreWrap),
  ("break-spaces", BreakSpaces),
  ("pre-line", PreLine)
);

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#word-break-property
enum_property!(WordBreak,
  ("normal", Normal),
  ("keep-all", KeepAll),
  ("break-all", BreakAll),
  ("break-word", BreakWord)
);

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#line-break-property
enum_property!(LineBreak,
  Auto,
  Loose,
  Normal,
  Strict,
  Anywhere
);
// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#hyphenation
enum_property!(Hyphens,
  None,
  Manual,
  Auto
);

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#overflow-wrap-property
enum_property!(OverflowWrap,
  ("normal", Normal),
  ("break-word", BreakWord),
  ("anywhere", Anywhere)
);

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-align-property
enum_property!(TextAlign,
  ("start", Start),
  ("end", End),
  ("left", Left),
  ("right", Right),
  ("center", Center),
  ("justify", Justify),
  ("match-parent", MatchParent),
  ("justify-all", JustifyAll)
);

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-align-last-property
enum_property!(TextAlignLast,
  ("auto", Auto),
  ("start", Start),
  ("end", End),
  ("left", Left),
  ("right", Right),
  ("center", Center),
  ("justify", Justify),
  ("match-parent", MatchParent)
);

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-justify-property
enum_property!(TextJustify,
  ("auto", Auto),
  ("none", None),
  ("inter-word", InterWord),
  ("inter-character", InterCharacter)
);

/// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#word-spacing-property
#[derive(Debug, Clone, PartialEq)]
pub enum Spacing {
  Normal,
  Length(Length)
}

impl Parse for Spacing {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(Spacing::Normal)
    }

    let length = Length::parse(input)?;
    Ok(Spacing::Length(length))
  }
}

impl ToCss for Spacing {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      Spacing::Normal => dest.write_str("normal"),
      Spacing::Length(len) => len.to_css(dest)
    }
  }
}

/// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-indent-property
#[derive(Debug, Clone, PartialEq)]
pub struct TextIndent {
  value: LengthPercentage,
  hanging: bool,
  each_line: bool
}

impl Parse for TextIndent {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut value = None;
    let mut hanging = false;
    let mut each_line = false;

    loop {
      if value.is_none() {
        if let Ok(val) = input.try_parse(LengthPercentage::parse) {
          value = Some(val);
          continue
        }
      }

      if !hanging {
        if input.try_parse(|input| input.expect_ident_matching("hanging")).is_ok() {
          hanging = true;
          continue
        }
      }

      if !each_line {
        if input.try_parse(|input| input.expect_ident_matching("each-line")).is_ok() {
          each_line = true;
          continue
        }
      }

      break
    }

    if let Some(value) = value {
      Ok(TextIndent {
        value,
        hanging,
        each_line
      })
    } else {
      Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }
}

impl ToCss for TextIndent {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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

// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-line-property
bitflags! {
  pub struct TextDecorationLine: u8 {
    const Underline     = 0b00000001;
    const Overline      = 0b00000010;
    const LineThrough   = 0b00000100;
    const Blink         = 0b00001000;
    const SpellingError = 0b00010000;
    const GrammarError  = 0b00100000;
  }
}

impl Default for TextDecorationLine {
  fn default() -> TextDecorationLine {
    TextDecorationLine::empty()
  }
}

impl Parse for TextDecorationLine {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut value = TextDecorationLine::empty();
    let mut any = false;

    loop {
      let flag: Result<_, ParseError<'i, ()>> = input.try_parse(|input| {
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
        break
      }
    }

    if !any {
      return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }

    Ok(value)
  }
}

impl ToCss for TextDecorationLine {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    if self.is_empty() {
      return dest.write_str("none")
    }

    if self.contains(TextDecorationLine::SpellingError) {
      return dest.write_str("spelling-error")
    }

    if self.contains(TextDecorationLine::GrammarError) {
      return dest.write_str("grammar-error")
    }

    let mut needs_space = false;
    macro_rules! val {
      ($val: ident, $str: expr) => {
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

// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-style-property
enum_property!(TextDecorationStyle,
  Solid,
  Double,
  Dotted,
  Dashed,
  Wavy
);

impl Default for TextDecorationStyle {
  fn default() -> TextDecorationStyle {
    TextDecorationStyle::Solid
  }
}

/// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-width-property
#[derive(Debug, Clone, PartialEq)]
pub enum TextDecorationThickness {
  Auto,
  FromFont,
  LengthPercentage(LengthPercentage)
}

impl Default for TextDecorationThickness {
  fn default() -> TextDecorationThickness {
    TextDecorationThickness::Auto
  }
}

impl Parse for TextDecorationThickness {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(TextDecorationThickness::Auto)
    }

    if input.try_parse(|input| input.expect_ident_matching("from-font")).is_ok() {
      return Ok(TextDecorationThickness::FromFont)
    }

    let lp = LengthPercentage::parse(input)?;
    Ok(TextDecorationThickness::LengthPercentage(lp))
  }
}

impl ToCss for TextDecorationThickness {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      TextDecorationThickness::Auto => dest.write_str("auto"),
      TextDecorationThickness::FromFont => dest.write_str("from-font"),
      TextDecorationThickness::LengthPercentage(lp) => lp.to_css(dest)
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextDecoration {
  line: TextDecorationLine,
  thickness: TextDecorationThickness,
  style: TextDecorationStyle,
  color: CssColor
}

impl Parse for TextDecoration {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
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
              continue
            }
          }
        };
      }

      prop!(line, TextDecorationLine);
      prop!(thickness, TextDecorationThickness);
      prop!(style, TextDecorationStyle);
      prop!(color, CssColor);
      break
    }

    Ok(TextDecoration {
      line: line.unwrap_or_default(),
      thickness: thickness.unwrap_or_default(),
      style: style.unwrap_or_default(),
      color: color.unwrap_or(CssColor::current_color())
    })
  }
}

impl ToCss for TextDecoration {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.line.to_css(dest)?;
    if self.line.is_empty() {
      return Ok(())
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

// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-decoration-skip-ink-property
enum_property!(TextDecorationSkipInk,
  Auto,
  None,
  All
);

enum_property!(TextEmphasisFillMode,
  Filled,
  Open
);

enum_property!(TextEmphasisShape,
  ("dot", Dot),
  ("circle", Circle),
  ("double-circle", DoubleCircle),
  ("triangle", Triangle),
  ("sesame", Sesame)
);

// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-style-property
#[derive(Debug, Clone, PartialEq)]
pub enum TextEmphasisStyle {
  None,
  Keyword {
    fill: TextEmphasisFillMode,
    shape: Option<TextEmphasisShape>
  },
  String(String)
}

impl Default for TextEmphasisStyle {
  fn default() -> TextEmphasisStyle {
    TextEmphasisStyle::None
  }
}

impl Parse for TextEmphasisStyle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(TextEmphasisStyle::None)
    }

    if let Ok(s) = input.try_parse(|input| input.expect_string().map(|s| s.as_ref().to_owned())) {
      return Ok(TextEmphasisStyle::String(s))
    }

    let mut shape = input.try_parse(TextEmphasisShape::parse).ok();
    let fill = input.try_parse(TextEmphasisFillMode::parse).ok();
    if shape.is_none() {
      shape = input.try_parse(TextEmphasisShape::parse).ok();
    }

    if shape.is_none() && fill.is_none() {
      return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }

    let fill = fill.unwrap_or(TextEmphasisFillMode::Filled);
    Ok(TextEmphasisStyle::Keyword { fill, shape })
  }
}

impl ToCss for TextEmphasisStyle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      TextEmphasisStyle::None => dest.write_str("none"),
      TextEmphasisStyle::String(s) => serialize_string(&s, dest),
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

/// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-property
#[derive(Debug, Clone, PartialEq)]
pub struct TextEmphasis {
  style: TextEmphasisStyle,
  color: CssColor
}

impl Parse for TextEmphasis {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut style = None;
    let mut color = None;

    loop {
      if style.is_none() {
        if let Ok(s) = input.try_parse(TextEmphasisStyle::parse) {
          style = Some(s);
          continue
        }
      }

      if color.is_none() {
        if let Ok(c) = input.try_parse(CssColor::parse) {
          color = Some(c);
          continue
        }
      }

      break
    }

    Ok(TextEmphasis {
      style: style.unwrap_or_default(),
      color: color.unwrap_or(CssColor::current_color())
    })
  }
}

impl ToCss for TextEmphasis {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.style.to_css(dest)?;

    if self.style != TextEmphasisStyle::None && self.color != CssColor::current_color() {
      dest.write_char(' ')?;
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}

enum_property!(TextEmphasisPositionVertical,
  Over,
  Under
);

enum_property!(TextEmphasisPositionHorizontal,
  Left,
  Right
);

/// https://www.w3.org/TR/2020/WD-css-text-decor-4-20200506/#text-emphasis-position-property
#[derive(Debug, Clone, PartialEq)]
pub struct TextEmphasisPosition {
  vertical: TextEmphasisPositionVertical,
  horizontal: TextEmphasisPositionHorizontal
}

impl Parse for TextEmphasisPosition {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(horizontal) = input.try_parse(TextEmphasisPositionHorizontal::parse) {
      let vertical = TextEmphasisPositionVertical::parse(input)?;
      Ok(TextEmphasisPosition { horizontal, vertical })
    } else {
      let vertical = TextEmphasisPositionVertical::parse(input)?;
      let horizontal = input.try_parse(TextEmphasisPositionHorizontal::parse).unwrap_or(TextEmphasisPositionHorizontal::Right);
      Ok(TextEmphasisPosition { horizontal, vertical })
    }
  }
}

impl ToCss for TextEmphasisPosition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.vertical.to_css(dest)?;
    if self.horizontal != TextEmphasisPositionHorizontal::Right {
      dest.write_char(' ')?;
      self.horizontal.to_css(dest)?;
    }
    Ok(())
  }
}

#[derive(Default)]
pub struct TextDecorationHandler {
  targets: Option<Browsers>,
  line: Option<(TextDecorationLine, VendorPrefix)>,
  thickness: Option<TextDecorationThickness>,
  style: Option<(TextDecorationStyle, VendorPrefix)>,
  color: Option<(CssColor, VendorPrefix)>,
  emphasis_style: Option<(TextEmphasisStyle, VendorPrefix)>,
  emphasis_color: Option<(CssColor, VendorPrefix)>,
  emphasis_position: Option<(TextEmphasisPosition, VendorPrefix)>
}

impl TextDecorationHandler {
  pub fn new(targets: Option<Browsers>) -> TextDecorationHandler {
    TextDecorationHandler {
      targets,
      ..TextDecorationHandler::default()
    }
  }
}

impl PropertyHandler for TextDecorationHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.finalize(dest);
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
          self.$prop = Some(($val.clone(), *$vp))
        }
      }};
    }

    match property {
      TextDecorationLine(val, vp) => property!(line, val, vp),
      TextDecorationThickness(val) => self.thickness = Some(val.clone()),
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
      _ => return false
    }
    
    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    let mut line = std::mem::take(&mut self.line);
    let mut thickness = std::mem::take(&mut self.thickness);
    let mut style = std::mem::take(&mut self.style);
    let mut color = std::mem::take(&mut self.color);
    let mut emphasis_style = std::mem::take(&mut self.emphasis_style);
    let mut emphasis_color = std::mem::take(&mut self.emphasis_color);
    let emphasis_position = std::mem::take(&mut self.emphasis_position);

    if let (Some((line, line_vp)), Some(thickness_val), Some((style, style_vp)), Some((color, color_vp))) = (&mut line, &mut thickness, &mut style, &mut color) {
      let intersection = *line_vp | *style_vp | *color_vp;
      if !intersection.is_empty() {
        let mut prefix = intersection;
        
        // Only add prefixes if one of the new sub-properties was used
        if prefix.contains(VendorPrefix::None) && (*style != TextDecorationStyle::default() || *color != CssColor::current_color()) {
          if let Some(targets) = self.targets {
            prefix = Feature::TextDecoration.prefixes_for(targets)
          }
        }

        dest.push(Property::TextDecoration(TextDecoration {
          line: line.clone(),
          thickness: thickness_val.clone(),
          style: style.clone(),
          color: color.clone()
        }, prefix));
        line_vp.remove(intersection);
        style_vp.remove(intersection);
        color_vp.remove(intersection);
        thickness = None;
      }
    }

    macro_rules! single_property {
      ($key: ident, $prop: ident) => {
        if let Some((val, vp)) = $key {
          if !vp.is_empty() {
            let mut prefix = vp;
            if prefix.contains(VendorPrefix::None) {
              if let Some(targets) = self.targets {
                prefix = Feature::$prop.prefixes_for(targets);
              }
            }
            dest.push(Property::$prop(val, prefix))
          }
        }
      };
    }

    single_property!(line, TextDecorationLine);
    single_property!(style, TextDecorationStyle);
    single_property!(color, TextDecorationColor);

    if let Some(thickness) = thickness {
      dest.push(Property::TextDecorationThickness(thickness))
    }

    if let (Some((style, style_vp)), Some((color, color_vp))) = (&mut emphasis_style, &mut emphasis_color) {
      let intersection = *style_vp | *color_vp;
      if !intersection.is_empty() {
        let mut prefix = intersection;
        if prefix.contains(VendorPrefix::None) {
          if let Some(targets) = self.targets {
            prefix = Feature::TextEmphasis.prefixes_for(targets)
          }
        }

        dest.push(Property::TextEmphasis(TextEmphasis {
          style: style.clone(),
          color: color.clone()
        }, prefix));
        style_vp.remove(intersection);
        color_vp.remove(intersection);
      }
    }

    single_property!(emphasis_style, TextEmphasisStyle);
    single_property!(emphasis_color, TextEmphasisColor);

    if let Some((pos, vp)) = emphasis_position {
      if !vp.is_empty() {
        let mut prefix = vp;
        if prefix.contains(VendorPrefix::None) {
          if let Some(targets) = self.targets {
            prefix = Feature::TextEmphasisPosition.prefixes_for(targets);
            // Prefixed version does not support horizontal keyword.
            if pos.horizontal != TextEmphasisPositionHorizontal::Right {
              prefix = VendorPrefix::None;
            }
          }
        }
        dest.push(Property::TextEmphasisPosition(pos, prefix))
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextShadow {
  pub color: CssColor,
  pub x_offset: Length,
  pub y_offset: Length,
  pub blur: Length,
  pub spread: Length, // added in Level 4 spec
}

impl Parse for TextShadow {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut color = None;
    let mut lengths = None;

    loop {
      if lengths.is_none() {
        let value = input.try_parse::<_, _, ParseError<()>>(|input| {
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

      break
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(TextShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2,
      spread: lengths.3
    })
  }
}

impl ToCss for TextShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
