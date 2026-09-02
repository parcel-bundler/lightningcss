//! The `@position-try` rule.

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::PrinterError;
use crate::traits::ToCss;
use crate::values::ident::DashedIdent;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@position-try](https://drafts.csswg.org/css-anchor-position-1/#position-try) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct PositionTryRule<'i> {
  /// The name of the position-try fallback.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// Declarations in the `@position-try` rule.
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for PositionTryRule<'i> {
  fn to_css<PrinterT: crate::printer::PrinterTrait>(&self, dest: &mut PrinterT) -> Result<(), PrinterError> {
    dest.add_mapping(self.loc);
    dest.write_str("@position-try ")?;
    self.name.to_css(dest)?;
    self.declarations.to_css_block(dest)
  }
}
