//! The `@namespace` rule.

use super::Location;
use crate::error::PrinterError;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::string::CowArcStr;
use cssparser::*;

/// A [@namespace](https://drafts.csswg.org/css-namespaces/#declaration) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NamespaceRule<'i> {
  /// An optional namespace prefix to declare, or `None` to declare the default namespace.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub prefix: Option<CowArcStr<'i>>,
  /// The url of the namespace.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub url: CowArcStr<'i>,
  /// The location of the rule in the source file.
  pub loc: Location,
}

impl<'i> ToCss for NamespaceRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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
