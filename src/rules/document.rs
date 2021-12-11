use cssparser::SourceLocation;
use crate::traits::ToCss;
use crate::printer::Printer;
use super::CssRuleList;
use crate::declaration::DeclarationHandler;
use crate::targets::Browsers;

#[derive(Debug, PartialEq)]
pub struct MozDocumentRule {
  pub rules: CssRuleList,
  pub loc: SourceLocation
}

impl MozDocumentRule {
  pub(crate) fn minify(&mut self, targets: Option<Browsers>, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    self.rules.minify(targets, handler, important_handler)
  }
}

impl ToCss for MozDocumentRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@-moz-document url-prefix()")?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    for rule in self.rules.0.iter() {
      dest.newline()?;
      rule.to_css(dest)?;
    }
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}
