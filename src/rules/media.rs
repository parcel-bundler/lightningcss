use crate::media_query::MediaList;
use crate::traits::ToCss;
use crate::printer::Printer;
use std::fmt::Write;
use super::CssRule;

#[derive(Debug, PartialEq)]
pub struct MediaRule {
  pub query: MediaList,
  pub rules: Vec<CssRule>
}

impl ToCss for MediaRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("@media ")?;
    self.query.to_css(dest)?;
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    for rule in self.rules.iter() {
      dest.newline()?;
      rule.to_css(dest)?;
    }
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}
