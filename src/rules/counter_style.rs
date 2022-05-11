//! The `@counter-style` rule.

use super::Location;
use crate::declaration::DeclarationBlock;
use crate::error::PrinterError;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::ident::CustomIdent;

/// A [@counter-style](https://drafts.csswg.org/css-counter-styles/#the-counter-style-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CounterStyleRule<'i> {
  /// The name of the counter style to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: CustomIdent<'i>,
  // TODO: eventually parse these properties
  /// Declarations in the `@counter-style` rule.
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> ToCss for CounterStyleRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.add_mapping(self.loc);
    dest.write_str("@counter-style ")?;
    self.name.to_css(dest)?;
    self.declarations.to_css_block(dest)
  }
}
