use crate::traits::ToCss;
use crate::printer::Printer;
use crate::error::PrinterError;
use crate::properties::{Property, PropertyId};

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalProperty<'i> {
  pub property_id: PropertyId<'i>,
  pub ltr: Option<Box<Property<'i>>>,
  pub rtl: Option<Box<Property<'i>>>
}

impl<'i> ToCss for LogicalProperty<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    if let Some(ltr) = &self.ltr {
      dest.write_str("var(--ltr,")?;
      dest.whitespace()?;
      ltr.value_to_css(dest)?;
      dest.write_char(')')?;
    }

    if self.ltr.is_some() && self.rtl.is_some() {
      dest.whitespace()?;
    }

    if let Some(rtl) = &self.rtl {
      dest.write_str("var(--rtl,")?;
      dest.whitespace()?;
      rtl.value_to_css(dest)?;
      dest.write_char(')')?;
    }

    Ok(())
  }
}

#[derive(Debug, PartialEq)]
pub(crate) enum PropertyCategory {
  Logical,
  Physical
}

impl Default for PropertyCategory {
  fn default() -> PropertyCategory {
    PropertyCategory::Physical
  }
}
