use cssparser::SourceLocation;
use crate::printer::Printer;
use crate::declaration::DeclarationHandler;
use super::style::StyleRule;
use crate::rules::{ToCssWithContext, StyleContext};
use crate::error::PrinterError;

#[derive(Debug, PartialEq)]
pub struct NestingRule {
  pub style: StyleRule,
  pub loc: SourceLocation
}

impl NestingRule {
  pub(crate) fn minify(&mut self, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    self.style.minify(handler, important_handler)
  }
}

impl ToCssWithContext for NestingRule {
  fn to_css_with_context<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    if context.is_none() {
      dest.write_str("@nest ")?;
    }
    self.style.to_css_with_context(dest, context)
  }
}
