//! The `@-moz-document` rule.

use super::Location;
use super::{CssRuleList, MinifyContext};
use crate::error::{MinifyError, PrinterError};
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::traits::ToCss;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@-moz-document](https://www.w3.org/TR/2012/WD-css3-conditional-20120911/#at-document) rule.
///
/// Note that only the `url-prefix()` function with no arguments is supported, and only the `-moz` prefix
/// is allowed since Firefox was the only browser that ever implemented this rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct MozDocumentRule<'i, R = DefaultAtRule> {
  /// Nested rules within the `@-moz-document` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub rules: CssRuleList<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i, T: Clone> MozDocumentRule<'i, T> {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext<'_, 'i>) -> Result<(), MinifyError> {
    self.rules.minify(context, false)
  }
}

impl<'i, T: ToCss> ToCss for MozDocumentRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
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
