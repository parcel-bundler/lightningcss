use super::Location;
use crate::traits::ToCss;
use crate::printer::Printer;
use super::{CssRuleList, MinifyContext};
use crate::error::{MinifyError, PrinterError};

#[derive(Debug, PartialEq, Clone)]
pub struct MozDocumentRule<'i> {
  pub rules: CssRuleList<'i>,
  pub loc: Location
}

impl<'i> MozDocumentRule<'i> {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext<'_, 'i>) -> Result<(), MinifyError> {
    self.rules.minify(context, false)
  }
}

impl<'i> ToCss for MozDocumentRule<'i> {
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
