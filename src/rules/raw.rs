//! Raw CSS recovered from invalid syntax.

use super::Location;
use crate::error::PrinterError;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A raw CSS fragment recovered from invalid syntax.
#[derive(Debug, PartialEq, Clone, Hash)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Raw<'i> {
  /// The raw CSS text.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub value: CowArcStr<'i>,
  /// The location of the raw fragment in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> Raw<'i> {
  pub(crate) fn from(value: &'i str, loc: Location) -> Self {
    Self {
      value: value.into(),
      loc,
    }
  }
}

impl<'i> ToCss for Raw<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str(&self.value)
  }
}
