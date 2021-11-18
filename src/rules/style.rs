use cssparser::SourceLocation;
use selectors::SelectorList;
use crate::selector::Selectors;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::declaration::{DeclarationBlock, DeclarationHandler};

#[derive(Debug, PartialEq)]
pub struct StyleRule {
  pub selectors: SelectorList<Selectors>,
  pub declarations: DeclarationBlock,
  pub loc: SourceLocation
}

impl StyleRule {
  pub fn minify(&mut self, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    self.declarations.minify(handler, important_handler);
  }
}

impl ToCss for StyleRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.add_mapping(self.loc);
    self.selectors.to_css(dest)?;
    self.declarations.to_css(dest)
  }
}
