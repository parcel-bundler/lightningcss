use crate::traits::ToCss;
use crate::declaration::DeclarationBlock;
use crate::printer::Printer;
use crate::values::ident::CustomIdent;

#[derive(Debug, PartialEq)]
pub struct CounterStyleRule {
  pub name: CustomIdent,
  // TODO: eventually parse these properties
  pub declarations: DeclarationBlock
}

impl ToCss for CounterStyleRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.write_str("@counter-style ")?;
    self.name.to_css(dest)?;
    self.declarations.to_css(dest)
  }
}
