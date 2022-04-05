use super::style::StyleRule;
use super::Location;
use super::MinifyContext;
use crate::error::{MinifyError, PrinterError};
use crate::printer::Printer;
use crate::rules::{StyleContext, ToCssWithContext};

#[derive(Debug, PartialEq, Clone)]
pub struct NestingRule<'i> {
  pub style: StyleRule<'i>,
  pub loc: Location,
}

impl<'i> NestingRule<'i> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.style.minify(context, parent_is_unused)
  }
}

impl<'a, 'i> ToCssWithContext<'a, 'i> for NestingRule<'i> {
  fn to_css_with_context<W>(
    &self,
    dest: &mut Printer<W>,
    context: Option<&StyleContext<'a, 'i>>,
  ) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    if context.is_none() {
      dest.write_str("@nest ")?;
    }
    self.style.to_css_with_context(dest, context)
  }
}
