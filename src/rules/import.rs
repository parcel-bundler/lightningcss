use cssparser::*;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::media_query::MediaList;
use super::supports::SupportsCondition;
use crate::error::PrinterError;

/// https://drafts.csswg.org/css-cascade/#at-import
#[derive(Debug, PartialEq, Clone)]
pub struct ImportRule {
  pub url: String,
  pub supports: Option<SupportsCondition>,
  pub media: MediaList,
  pub loc: SourceLocation
}

impl ToCss for ImportRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@import ")?;
    serialize_string(&self.url, dest)?;
    if let Some(supports) = &self.supports {
      dest.write_str(" supports")?;
      if matches!(supports, SupportsCondition::Declaration(_) | SupportsCondition::Parens(_)) {
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
