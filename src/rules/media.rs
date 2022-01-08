use cssparser::SourceLocation;
use crate::media_query::MediaList;
use crate::traits::ToCss;
use crate::printer::Printer;
use super::{CssRuleList, MinifyContext};
use crate::rules::{ToCssWithContext, StyleContext};
use crate::error::PrinterError;

#[derive(Debug, PartialEq)]
pub struct MediaRule {
  pub query: MediaList,
  pub rules: CssRuleList,
  pub loc: SourceLocation
}

impl MediaRule {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext) {
    self.rules.minify(context)
  }
}

impl ToCssWithContext for MediaRule {
  fn to_css_with_context<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@media ")?;
    self.query.to_css(dest)?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    dest.newline()?;
    self.rules.to_css_with_context(dest, context)?;
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}
