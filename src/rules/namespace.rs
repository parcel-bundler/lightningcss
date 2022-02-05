use cssparser::*;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::error::PrinterError;

#[derive(Debug, PartialEq, Clone)]
pub struct NamespaceRule<'i> {
  pub prefix: Option<CowRcStr<'i>>,
  pub url: CowRcStr<'i>,
  pub loc: SourceLocation
}

impl<'i> ToCss for NamespaceRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@namespace ")?;
    if let Some(prefix) = &self.prefix {
      serialize_identifier(&prefix, dest)?;
      dest.write_char(' ')?;
    }

    serialize_string(&self.url, dest)?;
    dest.write_char(';')
  }
}
