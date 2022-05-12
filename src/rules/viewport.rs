//! The `@viewport` rule.

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::PrinterError;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::vendor_prefix::VendorPrefix;

/// A [@viewport](https://drafts.csswg.org/css-device-adapt/#atviewport-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ViewportRule<'i> {
  /// The vendor prefix for this rule, e.g. `@-ms-viewport`.
  pub vendor_prefix: VendorPrefix,
  /// The declarations within the `@viewport` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> ToCss for ViewportRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    dest.write_char('@')?;
    self.vendor_prefix.to_css(dest)?;
    dest.write_str("viewport")?;
    self.declarations.to_css_block(dest)
  }
}
