//! The `@counter-style` rule.

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::PrinterError;
use crate::traits::ToCss;
use crate::values::ident::CustomIdent;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@counter-style](https://drafts.csswg.org/css-counter-styles/#the-counter-style-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct CounterStyleRule<'i> {
  /// The name of the counter style to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: CustomIdent<'i>,
  // TODO: eventually parse these properties
  /// Declarations in the `@counter-style` rule.
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for CounterStyleRule<'i> {
  fn to_css<PrinterT: crate::printer::PrinterTrait>(&self, dest: &mut PrinterT) -> Result<(), PrinterError> {
    dest.add_mapping(self.loc);
    dest.write_str("@counter-style ")?;
    self.name.to_css(dest)?;
    self.declarations.to_css_block(dest)
  }
}
