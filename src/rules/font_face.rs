use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::properties::font::{FontFamily, FontStyle, FontWeight, FontStretch};
use crate::values::size::Size2D;
use crate::properties::custom::CustomProperty;
use crate::macros::enum_property;
use crate::values::url::Url;
use crate::error::{ParserError, PrinterError};

#[derive(Debug, PartialEq, Clone)]
pub struct FontFaceRule<'i> {
  pub properties: Vec<FontFaceProperty<'i>>,
  pub loc: SourceLocation
}

#[derive(Debug, Clone, PartialEq)]
pub enum FontFaceProperty<'i> {
  Source(Vec<Source<'i>>),
  FontFamily(FontFamily<'i>),
  FontStyle(FontStyle),
  FontWeight(Size2D<FontWeight>),
  FontStretch(Size2D<FontStretch>),
  Custom(CustomProperty<'i>)
}

/// https://www.w3.org/TR/2021/WD-css-fonts-4-20210729/#font-face-src-parsing
#[derive(Debug, Clone, PartialEq)]
pub enum Source<'i> {
  Url(UrlSource<'i>),
  Local(FontFamily<'i>)
}

impl<'i> Parse<'i> for Source<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(url) = input.try_parse(UrlSource::parse) {
      return Ok(Source::Url(url))
    }

    input.expect_function_matching("local")?;
    let local = input.parse_nested_block(FontFamily::parse)?;
    Ok(Source::Local(local))
  }
}

impl<'i> ToCss for Source<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
pub struct UrlSource<'i> {
  pub url: Url<'i>,
  pub format: Option<Format<'i>>
}

impl<'i> Parse<'i> for UrlSource<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let url = Url::parse(input)?;

    let format = if input.try_parse(|input| input.expect_function_matching("format")).is_ok() {
      Some(input.parse_nested_block(Format::parse)?)
    } else {
      None
    };

    Ok(UrlSource { url, format })
  }
}

impl<'i> ToCss for UrlSource<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.url.to_css(dest)?;
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
pub struct Format<'i> {
  pub format: FontFormat<'i>,
  pub supports: Vec<FontTechnology>
}

impl<'i> Parse<'i> for Format<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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


impl<'i> ToCss for Format<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
pub enum FontFormat<'i> {
  WOFF,
  WOFF2,
  TrueType,
  OpenType,
  EmbeddedOpenType,
  Collection,
  SVG,
  String(CowRcStr<'i>)
}

impl<'i> Parse<'i> for FontFormat<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let s = input.expect_ident_or_string()?;
    match_ignore_ascii_case! { &s,
      "woff" => Ok(FontFormat::WOFF),
      "woff2" => Ok(FontFormat::WOFF2),
      "truetype" => Ok(FontFormat::TrueType),
      "opentype" => Ok(FontFormat::OpenType),
      "embedded-opentype" => Ok(FontFormat::EmbeddedOpenType),
      "collection" => Ok(FontFormat::Collection),
      "svg" => Ok(FontFormat::SVG),
      _ => Ok(FontFormat::String(s.clone()))
    }
  }
}

impl<'i> ToCss for FontFormat<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use FontFormat::*;
    let s = match self {
      WOFF => "woff",
      WOFF2 => "woff2",
      TrueType => "truetype",
      OpenType => "opentype",
      EmbeddedOpenType => "embedded-opentype",
      Collection => "collection",
      SVG => "svg",
      String(s) => &s
    };
    // Browser support for keywords rather than strings is very limited.
    // https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face/src
    serialize_string(&s, dest)?;
    Ok(())
  }
}

enum_property! {
  pub enum FontFeatureTechnology {
    OpenType,
    AAT,
    Graphite,
  }
}

enum_property! {
  pub enum ColorFontTechnology {
    COLRv0,
    COLRv1,
    SVG,
    SBIX,
    CBDT,
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FontTechnology {
  Features(FontFeatureTechnology),
  Variations,
  Color(ColorFontTechnology),
  Palettes
}

impl<'i> Parse<'i> for FontTechnology {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

pub(crate) struct FontFaceDeclarationParser;

/// Parse a declaration within {} block: `color: blue`
impl<'i> cssparser::DeclarationParser<'i> for FontFaceDeclarationParser {
  type Declaration = FontFaceProperty<'i>;
  type Error = ParserError<'i>;

  fn parse_value<'t>(
      &mut self,
      name: CowRcStr<'i>,
      input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    macro_rules! property {
      ($property: ident, $type: ty) => {
        if let Ok(c) = <$type>::parse(input) {
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
      "font-weight" => property!(FontWeight, Size2D<FontWeight>),
      "font-style" => property!(FontStyle, FontStyle),
      "font-stretch" => property!(FontStretch, Size2D<FontStretch>),
      _ => {}
    }

    input.reset(&state);
    return Ok(FontFaceProperty::Custom(CustomProperty::parse(name, input)?))
  }
}

/// Default methods reject all at rules.
impl<'i> AtRuleParser<'i> for FontFaceDeclarationParser {
  type Prelude = ();
  type AtRule = FontFaceProperty<'i>;
  type Error = ParserError<'i>;
}

impl<'i> ToCss for FontFaceRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@font-face")?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    let len = self.properties.len();
    for (i, prop) in self.properties.iter().enumerate() {
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

impl<'i> ToCss for FontFaceProperty<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
        custom.value.to_css(dest)
      }
    }
  }
}
