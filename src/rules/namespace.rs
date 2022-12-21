//! The `@namespace` rule.

use super::Location;
use crate::error::PrinterError;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::ident::Ident;
use crate::values::string::CSSString;
use crate::visitor::Visit;

/// A [@namespace](https://drafts.csswg.org/css-namespaces/#declaration) rule.
#[derive(Debug, PartialEq, Clone, Visit)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NamespaceRule<'i> {
  /// An optional namespace prefix to declare, or `None` to declare the default namespace.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[skip_visit]
  pub prefix: Option<Ident<'i>>,
  /// The url of the namespace.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[skip_visit]
  pub url: CSSString<'i>,
  /// The location of the rule in the source file.
  #[skip_visit]
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
      prefix.to_css(dest)?;
      dest.write_char(' ')?;
    }

    self.url.to_css(dest)?;
    dest.write_char(';')
  }
}
