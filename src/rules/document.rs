use cssparser::SourceLocation;
use crate::traits::ToCss;
use crate::printer::Printer;
use super::{CssRuleList, MinifyContext};
use crate::error::{MinifyError, PrinterError};

#[derive(Debug, PartialEq, Clone)]
pub struct MozDocumentRule {
  pub rules: CssRuleList,
  pub loc: SourceLocation
}

impl MozDocumentRule {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext) -> Result<(), MinifyError> {
    self.rules.minify(context, false)
  }
}

impl ToCss for MozDocumentRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@-moz-document url-prefix()")?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    dest.newline()?;
    self.rules.to_css(dest)?;
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}
