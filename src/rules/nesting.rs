//! The `@nest` rule.

use super::style::StyleRule;
use super::Location;
use super::MinifyContext;
use crate::error::{MinifyError, PrinterError};
use crate::printer::Printer;
use crate::rules::{StyleContext, ToCssWithContext};

/// A [@nest](https://www.w3.org/TR/css-nesting-1/#at-nest) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NestingRule<'i> {
  /// The style rule that defines the selector and declarations for the `@nest` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub style: StyleRule<'i>,
  /// The location of the rule in the source file.
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
