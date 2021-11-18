use selectors::SelectorList;
use crate::selector::Selectors;
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::declaration::DeclarationBlock;

#[derive(Debug, PartialEq)]
pub struct StyleRule {
  pub selectors: SelectorList<Selectors>,
  pub declarations: DeclarationBlock
}

impl ToCss for StyleRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.selectors.to_css(dest)?;
    self.declarations.to_css(dest)
  }
}
