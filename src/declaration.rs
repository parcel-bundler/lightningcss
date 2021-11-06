use cssparser::*;
use crate::properties::Property;
use crate::traits::{PropertyHandler, ToCss};
use crate::printer::Printer;
use crate::properties::{
  align::AlignHandler,
  background::BackgroundHandler,
  flex::FlexHandler,
  font::FontHandler,
  margin_padding::*,
  outline::OutlineHandler,
  border::BorderHandler,
  transition::TransitionHandler,
  animation::AnimationHandler
};
use crate::properties::prefixes::Browsers;

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
  pub property: Property,
  pub important: bool
}

impl Declaration {
  pub fn parse<'i, 't>(name: CowRcStr<'i>, input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let property = input.parse_until_before(Delimiter::Bang, |input| Property::parse(name, input))?;
    let important = input.try_parse(|input| {
      input.expect_delim('!')?;
      input.expect_ident_matching("important")
    }).is_ok();
    Ok(Declaration { property, important })
  }
}

impl ToCss for Declaration {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.property.to_css(dest, self.important)
  }
}

#[derive(Default)]
pub struct DeclarationHandler {
  important: bool,
  background: BackgroundHandler,
  border: BorderHandler,
  outline: OutlineHandler,
  flex: FlexHandler,
  align: AlignHandler,
  margin: MarginHandler,
  padding: PaddingHandler,
  scroll_margin: ScrollMarginHandler,
  scroll_padding: ScrollPaddingHandler,
  font: FontHandler,
  transition: TransitionHandler,
  animation: AnimationHandler
}

impl DeclarationHandler {
  pub fn new(important: bool, targets: Option<Browsers>) -> Self {
    DeclarationHandler {
      important,
      transition: TransitionHandler::new(targets),
      ..DeclarationHandler::default()
    }
  }

  pub fn handle_property(&mut self, decl: &Declaration) -> bool {
    let property = &decl.property;
    self.background.handle_property(property) ||
    self.border.handle_property(property) ||
    self.outline.handle_property(property) ||
    self.flex.handle_property(property) ||
    self.align.handle_property(property) ||
    self.margin.handle_property(property) ||
    self.padding.handle_property(property) ||
    self.scroll_margin.handle_property(property) ||
    self.scroll_padding.handle_property(property) ||
    self.font.handle_property(property) ||
    self.transition.handle_property(property) ||
    self.animation.handle_property(property)
  }

  pub fn finalize(&mut self) -> Vec<Declaration> {
    let important = self.important;
    self.background.finalize().drain(..)
      .chain(self.border.finalize().drain(..))
      .chain(self.outline.finalize().drain(..))
      .chain(self.flex.finalize().drain(..))
      .chain(self.align.finalize().drain(..))
      .chain(self.margin.finalize().drain(..))
      .chain(self.padding.finalize().drain(..))
      .chain(self.scroll_margin.finalize().drain(..))
      .chain(self.scroll_padding.finalize().drain(..))
      .chain(self.font.finalize().drain(..))
      .chain(self.transition.finalize().drain(..))
      .chain(self.animation.finalize().drain(..))
      .map(|property| Declaration { property, important })
      .collect()
  }
}
