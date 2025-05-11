//! CSS url() values.

use crate::dependencies::{Dependency, Location, UrlDependency};
use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A CSS [url()](https://www.w3.org/TR/css-values-4/#urls) value and its source location.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "visitor", visit(visit_url, URLS))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Url<'i> {
  /// The url string.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub url: CowArcStr<'i>,
  /// The location where the `url()` was seen in the CSS source file.
  pub loc: Location,
}

impl<'i> PartialEq for Url<'i> {
  fn eq(&self, other: &Self) -> bool {
    self.url == other.url
  }
}

impl<'i> Parse<'i> for Url<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let url = input.expect_url()?.into();
    Ok(Url { url, loc: loc.into() })
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
      dest.write_str("url(")?;
      serialize_string(&self.url, dest)?;
      dest.write_char(')')?;
    }

    Ok(())
  }
}

impl<'i> Url<'i> {
  /// Returns whether the URL is absolute, and not relative.
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

    // If the url starts with '#' we have a fragment URL.
    // These are resolved relative to the document rather than the CSS file.
    // https://drafts.csswg.org/css-values-4/#local-urls
    if url.starts_with('#') {
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
