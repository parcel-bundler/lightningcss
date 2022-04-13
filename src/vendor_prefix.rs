//! Vendor prefixes.

#![allow(non_upper_case_globals)]

use crate::error::PrinterError;
use crate::printer::Printer;
use crate::traits::ToCss;
use bitflags::bitflags;

bitflags! {
  /// Bit flags that represent one or more vendor prefixes, such as
  /// `-webkit` or `-moz`.
  ///
  /// Multiple flags can be combined to represent
  /// more than one prefix. During printing, the rule or property will
  /// be duplicated for each prefix flag that is enabled. This enables
  /// vendor prefixes to be added without increasing memory usage.
  pub struct VendorPrefix: u8 {
    /// No vendor prefixes.
    const None   = 0b00000001;
    /// The `-webkit` vendor prefix.
    const WebKit = 0b00000010;
    /// The `-moz` vendor prefix.
    const Moz    = 0b00000100;
    /// The `-ms` vendor prefix.
    const Ms     = 0b00001000;
    /// The `-o` vendor prefix.
    const O      = 0b00010000;
  }
}

impl Default for VendorPrefix {
  fn default() -> VendorPrefix {
    VendorPrefix::None
  }
}

impl VendorPrefix {
  /// Returns a vendor prefix flag from a prefix string (without the leading `-`).
  pub fn from_str(s: &str) -> VendorPrefix {
    match s {
      "webkit" => VendorPrefix::WebKit,
      "moz" => VendorPrefix::Moz,
      "ms" => VendorPrefix::Ms,
      "o" => VendorPrefix::O,
      _ => unreachable!(),
    }
  }
}

impl ToCss for VendorPrefix {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    cssparser::ToCss::to_css(self, dest)?;
    Ok(())
  }
}

impl cssparser::ToCss for VendorPrefix {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result
  where
    W: std::fmt::Write,
  {
    match *self {
      VendorPrefix::WebKit => dest.write_str("-webkit-"),
      VendorPrefix::Moz => dest.write_str("-moz-"),
      VendorPrefix::Ms => dest.write_str("-ms-"),
      VendorPrefix::O => dest.write_str("-o-"),
      _ => Ok(()),
    }
  }
}
