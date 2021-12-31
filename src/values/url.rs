use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use crate::dependencies::{Dependency, UrlDependency};
use crate::error::ParserError;

#[derive(Debug, Clone, PartialEq)]
pub struct Url {
  pub url: String,
  pub loc: SourceLocation
}

impl Parse for Url {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let url = input.expect_url()?.as_ref().to_owned();
    Ok(Url { url, loc })
  }
}

impl ToCss for Url {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let dep = if dest.dependencies.is_some() {
      Some(UrlDependency::new(self, dest.filename))
    } else {
      None
    };

    let url = if let Some(dep) = &dep {
      &dep.placeholder
    } else {
      &self.url
    };

    use cssparser::ToCss;
    if dest.minify {
      let mut buf = String::new();
      Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(&mut buf)?;

      // If the unquoted url is longer than it would be quoted (e.g. `url("...")`)
      // then serialize as a string and choose the shorter version.
      if buf.len() > url.len() + 7 {
        let mut buf2 = String::new();
        serialize_string(&url, &mut buf2)?;
        if buf2.len() + 5 < buf.len() {
          dest.write_str("url(")?;
          dest.write_str(&buf2)?;
          return dest.write_char(')')
        }
      }

      dest.write_str(&buf)?;
    } else {
      Token::UnquotedUrl(CowRcStr::from(url.as_ref())).to_css(dest)?;
    }

    if let Some(dependencies) = &mut dest.dependencies {
      dependencies.push(Dependency::Url(dep.unwrap()))
    }

    Ok(())
  }
}
