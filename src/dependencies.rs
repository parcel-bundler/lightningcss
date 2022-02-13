use crate::rules::import::ImportRule;
use crate::values::url::Url;
use serde::Serialize;
use cssparser::SourceLocation;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::css_modules::hash;

#[derive(Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Dependency {
  Import(ImportDependency),
  Url(UrlDependency)
}

#[derive(Serialize)]
pub struct ImportDependency {
  pub url: String,
  pub supports: Option<String>,
  pub media: Option<String>,
  pub loc: SourceRange
}

impl ImportDependency {
  pub fn new(rule: &ImportRule, filename: &str) -> ImportDependency {
    let supports = if let Some(supports) = &rule.supports {
      let mut s = String::new();
      let mut printer = Printer::new(&mut s, None, false, None);
      supports.to_css(&mut printer).unwrap();
      Some(s)
    } else {
      None
    };

    let media = if !rule.media.media_queries.is_empty() {
      let mut s = String::new();
      let mut printer = Printer::new(&mut s, None, false, None);
      rule.media.to_css(&mut printer).unwrap();
      Some(s)
    } else {
      None
    };

    ImportDependency {
      url: rule.url.as_ref().to_owned(),
      supports,
      media,
      loc: SourceRange::new(filename, SourceLocation { line: rule.loc.line, column: rule.loc.column }, 8, rule.url.len() + 2) // TODO: what about @import url(...)?
    }
  }
}

#[derive(Serialize)]
pub struct UrlDependency {
  pub url: String,
  pub placeholder: String,
  pub loc: SourceRange
}

impl UrlDependency {
  pub fn new(url: &Url, filename: &str) -> UrlDependency {
    let placeholder = hash(&format!("{}_{}", filename, url.url));
    UrlDependency {
      url: url.url.to_string(),
      placeholder,
      loc: SourceRange::new(filename, url.loc, 4, url.url.len())
    }
  }
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceRange {
  pub file_path: String,
  pub start: Location,
  pub end: Location,
}

#[derive(Serialize)]
pub struct Location {
  pub line: u32,
  pub column: u32
}

impl SourceRange {
  fn new(filename: &str, loc: SourceLocation, offset: u32, len: usize) -> SourceRange {
    SourceRange {
      file_path: filename.into(),
      start: Location {
        line: loc.line + 1,
        column: loc.column + offset
      },
      end: Location {
        line: loc.line + 1,
        column: loc.column + offset + (len as u32) - 1
      }
    }
  }
}
