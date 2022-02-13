use super::Location;
use crate::traits::ToCss;
use crate::declaration::DeclarationBlock;
use crate::printer::Printer;
use crate::values::ident::CustomIdent;
use crate::error::PrinterError;

#[derive(Debug, PartialEq, Clone)]
pub struct CounterStyleRule<'i> {
  pub name: CustomIdent<'i>,
  // TODO: eventually parse these properties
  pub declarations: DeclarationBlock<'i>,
  pub loc: Location
}

impl<'i> ToCss for CounterStyleRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@counter-style ")?;
    self.name.to_css(dest)?;
    self.declarations.to_css(dest)
  }
}
