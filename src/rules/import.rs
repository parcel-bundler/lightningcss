//! The `@import` rule.

use super::layer::LayerName;
use super::supports::SupportsCondition;
use super::Location;
use crate::dependencies::{Dependency, ImportDependency};
use crate::error::PrinterError;
use crate::media_query::MediaList;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A [@import](https://drafts.csswg.org/css-cascade/#at-import) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ImportRule<'i> {
  /// The url to import.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub url: CowArcStr<'i>,
  /// An optional cascade layer name, or `None` for an anonymous layer.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub layer: Option<Option<LayerName<'i>>>,
  /// An optional `supports()` condition.
  pub supports: Option<SupportsCondition<'i>>,
  /// A media query.
  #[cfg_attr(feature = "serde", serde(default))]
  pub media: MediaList<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for ImportRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let dep = if dest.dependencies.is_some() {
      Some(ImportDependency::new(self, dest.filename()))
    } else {
      None
    };

    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@import ")?;
    if let Some(dep) = dep {
      serialize_string(&dep.placeholder, dest)?;

      if let Some(dependencies) = &mut dest.dependencies {
        dependencies.push(Dependency::Import(dep))
      }
    } else {
      serialize_string(&self.url, dest)?;
    }

    if let Some(layer) = &self.layer {
      dest.write_str(" layer")?;
      if let Some(name) = layer {
        dest.write_char('(')?;
        name.to_css(dest)?;
        dest.write_char(')')?;
      }
    }

    if let Some(supports) = &self.supports {
      dest.write_str(" supports")?;
      if matches!(supports, SupportsCondition::Declaration { .. }) {
        supports.to_css(dest)?;
      } else {
        dest.write_char('(')?;
        supports.to_css(dest)?;
        dest.write_char(')')?;
      }
    }
    if !self.media.media_queries.is_empty() {
      dest.write_char(' ')?;
      self.media.to_css(dest)?;
    }
    dest.write_str(";")
  }
}
