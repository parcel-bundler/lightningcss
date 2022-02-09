use std::borrow::Cow;
use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::dependencies::{Dependency, UrlDependency};
use crate::error::{ParserError, PrinterError};
use crate::values::string::to_cow;

#[derive(Debug, Clone, PartialEq)]
pub struct Url<'i> {
  pub url: Cow<'i, str>,
  pub loc: SourceLocation
}

impl<'i> Parse<'i> for Url<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let url = to_cow(input.expect_url()?);
    Ok(Url { url, loc })
  }
}

impl<'i> ToCss for Url<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let dep = if dest.dependencies.is_some() {
      Some(UrlDependency::new(self, dest.filename))
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

      return Ok(())
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
          return dest.write_char(')')
        }
      }

      dest.write_str(&buf)?;
    } else {
      Token::UnquotedUrl(CowRcStr::from(self.url.as_ref())).to_css(dest)?;
    }

    Ok(())
  }
}
