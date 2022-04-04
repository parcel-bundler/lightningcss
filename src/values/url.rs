use crate::dependencies::{Dependency, UrlDependency};
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
use cssparser::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Url<'i> {
  pub url: CowArcStr<'i>,
  pub loc: SourceLocation,
}

impl<'i> Parse<'i> for Url<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let url = input.expect_url()?.into();
    Ok(Url { url, loc })
  }
}

impl<'i> ToCss for Url<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let dep = if dest.dependencies.is_some() {
      Some(UrlDependency::new(self, dest.filename()))
    } else {
      None
    };

    // If adding dependencies, always write url() with quotes so that the placeholder can
    // be replaced without escaping more easily. Quotes may be removed later during minification.
    if let Some(dep) = dep {
      dest.write_str("url(")?;
      serialize_string(&dep.placeholder, dest)?;
      dest.write_char(')')?;

      if let Some(dependencies) = &mut dest.dependencies {
        dependencies.push(Dependency::Url(dep))
      }

      return Ok(());
    }

    use cssparser::ToCss;
    if dest.minify {
      let mut buf = String::new();
      Token::UnquotedUrl(CowRcStr::from(self.url.as_ref())).to_css(&mut buf)?;

      // If the unquoted url is longer than it would be quoted (e.g. `url("...")`)
      // then serialize as a string and choose the shorter version.
      if buf.len() > self.url.len() + 7 {
        let mut buf2 = String::new();
        serialize_string(&self.url, &mut buf2)?;
        if buf2.len() + 5 < buf.len() {
          dest.write_str("url(")?;
          dest.write_str(&buf2)?;
          return dest.write_char(')');
        }
      }

      dest.write_str(&buf)?;
    } else {
      Token::UnquotedUrl(CowRcStr::from(self.url.as_ref())).to_css(dest)?;
    }

    Ok(())
  }
}

impl<'i> Url<'i> {
  pub fn is_absolute(&self) -> bool {
    let url = self.url.as_ref();

    // Quick checks. If the url starts with '.', it is relative.
    if url.starts_with('.') {
      return false;
    }

    // If the url starts with '/' it is absolute.
    if url.starts_with('/') {
      return true;
    }

    // Otherwise, we might have a scheme. These must start with an ascii alpha character.
    // https://url.spec.whatwg.org/#scheme-start-state
    if !url.starts_with(|c| matches!(c, 'a'..='z' | 'A'..='Z')) {
      return false;
    }

    // https://url.spec.whatwg.org/#scheme-state
    for b in url.as_bytes() {
      let c = *b as char;
      match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '+' | '-' | '.' => {}
        ':' => return true,
        _ => break,
      }
    }

    false
  }
}
