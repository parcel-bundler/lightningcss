use cssparser::*;
use crate::values::percentage::Percentage;
use crate::traits::{Parse, ToCss};
use crate::parser::{PropertyDeclarationParser, DeclarationBlock};
use crate::printer::Printer;
use crate::properties::font::{FontFamily, FontStyle, FontWeight, FontStretch};
use crate::properties::custom::CustomProperty;
use crate::macros::enum_property;
use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub struct FontFaceRule {
  pub properties: Vec<FontFaceProperty>
}

#[derive(Debug, Clone, PartialEq)]
pub enum FontFaceProperty {
  Source(Vec<Source>),
  FontFamily(FontFamily),
  FontStyle(FontStyle),
  FontWeight(FontWeight),
  FontStretch(FontStretch),
  Custom(CustomProperty)
}

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-face-src-parsing
#[derive(Debug, Clone, PartialEq)]
pub enum Source {
  Url(UrlSource),
  Local(FontFamily)
}

impl Parse for Source {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(url) = input.try_parse(UrlSource::parse) {
      return Ok(Source::Url(url))
    }

    input.expect_function_matching("local")?;
    let local = input.parse_nested_block(FontFamily::parse)?;
    Ok(Source::Local(local))
  }
}

impl ToCss for Source {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      Source::Url(url) => url.to_css(dest),
      Source::Local(local) => {
        dest.write_str("local(")?;
        local.to_css(dest)?;
        dest.write_char(')')
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UrlSource {
  pub url: String,
  pub format: Option<Format>
}

impl Parse for UrlSource {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let url = input.expect_url()?.as_ref().to_owned();

    let format = if input.try_parse(|input| input.expect_function_matching("format")).is_ok() {
      Some(input.parse_nested_block(Format::parse)?)
    } else {
      None
    };

    Ok(UrlSource { url, format })
  }
}

impl ToCss for UrlSource {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use cssparser::ToCss;
    Token::UnquotedUrl(CowRcStr::from(self.url.as_ref())).to_css(dest)?;
    if let Some(format) = &self.format {
      dest.whitespace()?;
      dest.write_str("format(")?;
      format.to_css(dest)?;
      dest.write_char(')')?;
    }
    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Format {
  format: FontFormat,
  supports: Vec<FontTechnology>
}

impl Parse for Format {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let format = FontFormat::parse(input)?;
    let mut supports = vec![];
    if input.try_parse(|input| input.expect_ident_matching("supports")).is_ok() {
      loop {
        if let Ok(technology) = input.try_parse(FontTechnology::parse) {
          supports.push(technology)
        } else {
          break
        }
      }
    }
    Ok(Format { format, supports })
  }
}


impl ToCss for Format {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.format.to_css(dest)?;
    if !self.supports.is_empty() {
      dest.write_str(" supports ")?;
      let mut first = true;
      for technology in &self.supports {
        if first {
          first = false;
        } else {
          dest.write_char(' ')?;
        }
        technology.to_css(dest)?;
      }
    }
    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FontFormat {
  WOFF,
  WOFF2,
  TrueType,
  OpenType,
  EmbeddedOpenType,
  Collection,
  SVG,
  String(String)
}

impl Parse for FontFormat {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let s = input.expect_ident_or_string()?;
    match_ignore_ascii_case! { &s,
      "woff" => Ok(FontFormat::WOFF),
      "woff2" => Ok(FontFormat::WOFF2),
      "truetype" => Ok(FontFormat::TrueType),
      "opentype" => Ok(FontFormat::OpenType),
      "embedded-opentype" => Ok(FontFormat::EmbeddedOpenType),
      "collection" => Ok(FontFormat::Collection),
      "svg" => Ok(FontFormat::SVG),
      _ => Ok(FontFormat::String(s.as_ref().to_owned()))
    }
  }
}

impl ToCss for FontFormat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use FontFormat::*;
    let s = match self {
      WOFF => "woff",
      WOFF2 => "woff2",
      TrueType => "truetype",
      OpenType => "opentype",
      EmbeddedOpenType => "embedded-opentype",
      Collection => "collection",
      SVG => "svg",
      String(s) => return serialize_string(&s, dest)
    };
    dest.write_str(s)
  }
}

enum_property!(FontFeatureTechnology,
  OpenType,
  AAT,
  Graphite
);

enum_property!(ColorFontTechnology,
  COLRv0,
  COLRv1,
  SVG,
  SBIX,
  CBDT
);

#[derive(Debug, Clone, PartialEq)]
pub enum FontTechnology {
  Features(FontFeatureTechnology),
  Variations,
  Color(ColorFontTechnology),
  Palettes
}

impl Parse for FontTechnology {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    match input.next()? {
      Token::Function(f) => {
        match_ignore_ascii_case! { &f,
          "features" => Ok(FontTechnology::Features(input.parse_nested_block(FontFeatureTechnology::parse)?)),
          "color" => Ok(FontTechnology::Color(input.parse_nested_block(ColorFontTechnology::parse)?)),
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(f.clone())
          ))
        }
      }
      Token::Ident(ident) => {
        match_ignore_ascii_case! { &ident,
          "variations" => Ok(FontTechnology::Variations),
          "palettes" => Ok(FontTechnology::Palettes),
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        }
      }
      tok => Err(location.new_unexpected_token_error(tok.clone()))
    }
  }
}

impl ToCss for FontTechnology {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      FontTechnology::Features(f) => {
        dest.write_str("features(")?;
        f.to_css(dest)?;
        dest.write_char(')')
      }
      FontTechnology::Color(c) => {
        dest.write_str("color(")?;
        c.to_css(dest)?;
        dest.write_char(')')
      }
      FontTechnology::Variations => dest.write_str("variations"),
      FontTechnology::Palettes => dest.write_str("palettes")
    }
  }
}

pub struct FontFaceDeclarationParser;

/// Parse a declaration within {} block: `color: blue`
impl<'i> cssparser::DeclarationParser<'i> for FontFaceDeclarationParser {
  type Declaration = FontFaceProperty;
  type Error = ();

  fn parse_value<'t>(
      &mut self,
      name: CowRcStr<'i>,
      input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    macro_rules! property {
      ($property: ident, $type: ident) => {
        if let Ok(c) = $type::parse(input) {
          return Ok(FontFaceProperty::$property(c))
        }
      };
    }

    let state = input.state();
    match_ignore_ascii_case! { &name,
      "src" => {
        if let Ok(sources) = input.parse_comma_separated(Source::parse) {
          return Ok(FontFaceProperty::Source(sources))
        }
      },
      "font-family" => property!(FontFamily, FontFamily),
      "font-weight" => property!(FontWeight, FontWeight),
      "font-style" => property!(FontStyle, FontStyle),
      "font-stretch" => property!(FontStretch, FontStretch),
      _ => {}
    }

    input.reset(&state);
    return Ok(FontFaceProperty::Custom(CustomProperty::parse(name, input)?))
  }
}

/// Default methods reject all at rules.
impl<'i> AtRuleParser<'i> for FontFaceDeclarationParser {
  type PreludeNoBlock = ();
  type PreludeBlock = ();
  type AtRule = FontFaceProperty;
  type Error = ();
}

impl ToCss for FontFaceRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("@font-face")?;
    dest.delim('{', true)?;
    let len = self.properties.len();
    for (i, prop) in self.properties.iter().enumerate() {
      if !dest.minify {
        dest.write_str("\n  ")?;
      }
      prop.to_css(dest)?;
      if i != len - 1 || !dest.minify {
        dest.write_char(';')?;
      }
    }
    dest.newline()?;
    dest.write_char('}')
  }
}

impl ToCss for FontFaceProperty {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use FontFaceProperty::*;
    macro_rules! property {
      ($prop: literal, $value: expr) => {{
        dest.write_str($prop)?;
        dest.delim(':', false)?;
        $value.to_css(dest)
      }};
      ($prop: literal, $value: expr, $multi: expr) => {{
        dest.write_str($prop)?;
        dest.delim(':', false)?;
        let len = $value.len();
        for (idx, val) in $value.iter().enumerate() {
          val.to_css(dest)?;
          if idx < len - 1 {
            dest.delim(',', false)?;
          }
        }
        Ok(())
      }};
    }

    match self {
      Source(value) => property!("src", value, true),
      FontFamily(value) => property!("font-family", value),
      FontStyle(value) => property!("font-style", value),
      FontWeight(value) => property!("font-weight", value),
      FontStretch(value) => property!("font-stretch", value),
      Custom(custom) => {
        dest.write_str(custom.name.as_ref())?;
        dest.delim(':', false)?;
        dest.write_str(custom.value.as_ref())
      }
    }
  }
}
