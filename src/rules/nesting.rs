//! The `@nest` rule.

use super::style::StyleRule;
use super::Location;
use super::MinifyContext;
use crate::error::{MinifyError, PrinterError};
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::traits::ToCss;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
/// A [@nest](https://www.w3.org/TR/css-nesting-1/#at-nest) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct NestingRule<'i, R = DefaultAtRule> {
  /// The style rule that defines the selector and declarations for the `@nest` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub style: StyleRule<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i, T: Clone> NestingRule<'i, T> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.style.minify(context, parent_is_unused)
  }
}

impl<'a, 'i, T: ToCss> ToCss for NestingRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    if dest.context().is_none() {
      dest.write_str("@nest ")?;
    }
    self.style.to_css(dest)
  }
}
