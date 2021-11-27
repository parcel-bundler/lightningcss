#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use crate::traits::ToCss;
use crate::printer::Printer;

bitflags! {
  pub struct VendorPrefix: u8 {
    const None   = 0b00000001;
    const WebKit = 0b00000010;
    const Moz    = 0b00000100;
    const Ms     = 0b00001000;
    const O      = 0b00010000;
  }
}

impl Default for VendorPrefix {
  fn default() -> VendorPrefix {
    VendorPrefix::None
  }
}

impl VendorPrefix {
  pub fn from_str(s: &str) -> VendorPrefix {
    match s {
      "webkit" => VendorPrefix::WebKit,
      "moz" => VendorPrefix::Moz,
      "ms" => VendorPrefix::Ms,
      "o" => VendorPrefix::O,
      _ => unreachable!()
    }
  }
}

impl ToCss for VendorPrefix {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match *self {
      VendorPrefix::WebKit => dest.write_str("-webkit-"),
      VendorPrefix::Moz => dest.write_str("-moz-"),
      VendorPrefix::Ms => dest.write_str("-ms-"),
      VendorPrefix::O => dest.write_str("-o-"),
      _ => Ok(())
    }
  }
}
