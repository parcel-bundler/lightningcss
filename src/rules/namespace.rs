use cssparser::*;
use crate::traits::ToCss;
use crate::printer::Printer;
use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub struct NamespaceRule {
  pub prefix: Option<String>,
  pub url: String
}

impl ToCss for NamespaceRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("@namespace ")?;
    if let Some(prefix) = &self.prefix {
      serialize_identifier(&prefix, dest)?;
      dest.write_char(' ')?;
    }

    serialize_string(&self.url, dest)?;
    dest.write_char(';')
  }
}
