//! Dependency analysis.
//!
//! Dependencies in CSS can be analyzed using the `analyze_dependencies` option
//! when printing a style sheet. These include other style sheets referenved via
//! the `@import` rule, as well as `url()` references. See [PrinterOptions](PrinterOptions).
//!
//! When dependency analysis is enabled, `@import` rules are removed, and `url()`
//! dependencies are replaced with hashed placeholders that can be substituted with
//! the final urls later (e.g. after bundling and content hashing).

use crate::css_modules::hash;
use crate::printer::PrinterOptions;
use crate::rules::import::ImportRule;
use crate::traits::ToCss;
use crate::values::url::Url;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::SourceLocation;
#[cfg(any(feature = "serde", feature = "nodejs"))]
use serde::Serialize;

/// Options for `analyze_dependencies` in `PrinterOptions`.
#[derive(Default)]
pub struct DependencyOptions {
  /// Whether to remove `@import` rules.
  pub remove_imports: bool,
}

/// A dependency.
#[derive(Debug)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(
  any(feature = "serde", feature = "nodejs"),
  serde(tag = "type", rename_all = "lowercase")
)]
pub enum Dependency {
  /// An `@import` dependency.
  Import(ImportDependency),
  /// A `url()` dependency.
  Url(UrlDependency),
}

/// An `@import` dependency.
#[derive(Debug)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
pub struct ImportDependency {
  /// The url to import.
  pub url: String,
  /// The placeholder that the URL was replaced with.
  pub placeholder: String,
  /// An optional `supports()` condition.
  pub supports: Option<String>,
  /// A media query.
  pub media: Option<String>,
  /// The location of the dependency in the source file.
  pub loc: SourceRange,
}

impl ImportDependency {
  /// Creates a new dependency from an `@import` rule.
  pub fn new(rule: &ImportRule, filename: &str) -> ImportDependency {
    let supports = if let Some(supports) = &rule.supports {
      let s = supports.to_css_string(PrinterOptions::default()).unwrap();
      Some(s)
    } else {
      None
    };

    let media = if !rule.media.media_queries.is_empty() {
      let s = rule.media.to_css_string(PrinterOptions::default()).unwrap();
      Some(s)
    } else {
      None
    };

    let placeholder = hash(&format!("{}_{}", filename, rule.url), false);

    ImportDependency {
      url: rule.url.as_ref().to_owned(),
      placeholder,
      supports,
      media,
      loc: SourceRange::new(
        filename,
        Location {
          line: rule.loc.line + 1,
          column: rule.loc.column,
        },
        8,
        rule.url.len() + 2,
      ), // TODO: what about @import url(...)?
    }
  }
}

/// A `url()` dependency.
#[derive(Debug)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
pub struct UrlDependency {
  /// The url of the dependency.
  pub url: String,
  /// The placeholder that the URL was replaced with.
  pub placeholder: String,
  /// The location of the dependency in the source file.
  pub loc: SourceRange,
}

impl UrlDependency {
  /// Creates a new url dependency.
  pub fn new(url: &Url, filename: &str) -> UrlDependency {
    let placeholder = hash(&format!("{}_{}", filename, url.url), false);
    UrlDependency {
      url: url.url.to_string(),
      placeholder,
      loc: SourceRange::new(filename, url.loc, 4, url.url.len()),
    }
  }
}

/// Represents the range of source code where a dependency was found.
#[derive(Debug)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(rename_all = "camelCase"))]
pub struct SourceRange {
  /// The filename in which the dependency was found.
  pub file_path: String,
  /// The starting line and column position of the dependency.
  pub start: Location,
  /// The ending line and column position of the dependency.
  pub end: Location,
}

/// A line and column position within a source file.
#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(serde::Serialize))]
#[cfg_attr(any(feature = "serde"), derive(serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct Location {
  /// The line number, starting from 1.
  pub line: u32,
  /// The column number, starting from 1.
  pub column: u32,
}

impl From<SourceLocation> for Location {
  fn from(loc: SourceLocation) -> Location {
    Location {
      line: loc.line + 1,
      column: loc.column,
    }
  }
}

impl SourceRange {
  fn new(filename: &str, loc: Location, offset: u32, len: usize) -> SourceRange {
    SourceRange {
      file_path: filename.into(),
      start: Location {
        line: loc.line,
        column: loc.column + offset,
      },
      end: Location {
        line: loc.line,
        column: loc.column + offset + (len as u32) - 1,
      },
    }
  }
}
