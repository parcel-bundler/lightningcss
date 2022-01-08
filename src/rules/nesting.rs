use cssparser::SourceLocation;
use crate::printer::Printer;
use super::style::StyleRule;
use crate::rules::{ToCssWithContext, StyleContext};
use crate::error::PrinterError;
use super::MinifyContext;

#[derive(Debug, PartialEq)]
pub struct NestingRule {
  pub style: StyleRule,
  pub loc: SourceLocation
}

impl NestingRule {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext, parent_is_unused: bool) -> bool {
    self.style.minify(context, parent_is_unused)
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
