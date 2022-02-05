use cssparser::{SourceLocation, serialize_identifier, CowRcStr};
use crate::media_query::MediaList;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::error::PrinterError;

#[derive(Debug, PartialEq, Clone)]
pub struct CustomMediaRule<'i> {
  pub name: CowRcStr<'i>,
  pub query: MediaList<'i>,
  pub loc: SourceLocation
}

impl<'i> ToCss for CustomMediaRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    dest.write_str("@custom-media ")?;
    serialize_identifier(&self.name, dest)?;
    dest.write_char(' ')?;
    self.query.to_css(dest)
  }
}
