use super::Location;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::vendor_prefix::VendorPrefix;
use crate::declaration::DeclarationBlock;
use crate::error::PrinterError;

#[derive(Debug, PartialEq, Clone)]
pub struct ViewportRule<'i> {
  pub vendor_prefix: VendorPrefix,
  pub declarations: DeclarationBlock<'i>,
  pub loc: Location
}

impl<'i> ToCss for ViewportRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_char('@')?;
    self.vendor_prefix.to_css(dest)?;
    dest.write_str("viewport")?;
    self.declarations.to_css(dest)
  }
}
