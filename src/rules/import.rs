//! The `@import` rule.

use super::layer::LayerName;
use super::supports::SupportsCondition;
use super::Location;
use crate::error::PrinterError;
use crate::media_query::MediaList;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::string::CowArcStr;
use cssparser::*;

/// A [@import](https://drafts.csswg.org/css-cascade/#at-import) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ImportRule<'i> {
  /// The url to import.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub url: CowArcStr<'i>,
  /// An optional cascade layer name, or `None` for an anonymous layer.
  pub layer: Option<Option<LayerName<'i>>>,
  /// An optional `supports()` condition.
  pub supports: Option<SupportsCondition<'i>>,
  /// A media query.
  pub media: MediaList<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> ToCss for ImportRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    dest.write_str("@import ")?;
    serialize_string(&self.url, dest)?;

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
      if matches!(
        supports,
        SupportsCondition::Declaration(_) | SupportsCondition::Parens(_)
      ) {
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
