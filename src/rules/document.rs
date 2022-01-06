use cssparser::SourceLocation;
use crate::traits::ToCss;
use crate::printer::Printer;
use super::CssRuleList;
use crate::declaration::DeclarationHandler;
use crate::targets::Browsers;
use crate::error::PrinterError;
use crate::logical::LogicalProperties;

#[derive(Debug, PartialEq)]
pub struct MozDocumentRule {
  pub rules: CssRuleList,
  pub loc: SourceLocation
}

impl MozDocumentRule {
  pub(crate) fn minify(
    &mut self,
    targets: Option<Browsers>,
    handler: &mut DeclarationHandler,
    important_handler: &mut DeclarationHandler,
    logical_properties: &mut LogicalProperties
  ) {
    self.rules.minify(targets, handler, important_handler, logical_properties)
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
