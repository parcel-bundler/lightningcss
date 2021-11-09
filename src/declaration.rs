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
  animation::AnimationHandler,
  prefix_handler::PrefixHandler,
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
  animation: AnimationHandler,
  prefix: PrefixHandler
}

impl DeclarationHandler {
  pub fn new(important: bool, targets: Option<Browsers>) -> Self {
    DeclarationHandler {
      important,
      border: BorderHandler::new(targets),
      flex: FlexHandler::new(targets),
      align: AlignHandler::new(targets),
      transition: TransitionHandler::new(targets),
      animation: AnimationHandler::new(targets),
      prefix: PrefixHandler::new(targets),
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
    self.animation.handle_property(property) ||
    self.prefix.handle_property(property)
  }

  pub fn finalize(&mut self) -> Vec<Declaration> {
    let important = self.important;
    let mut background = self.background.finalize();
    let mut border = self.border.finalize();
    let mut outline = self.outline.finalize();
    let mut flex = self.flex.finalize();
    let mut align = self.align.finalize();
    let mut margin = self.margin.finalize();
    let mut padding = self.padding.finalize();
    let mut scroll_margin = self.scroll_margin.finalize();
    let mut scroll_padding = self.scroll_padding.finalize();
    let mut font = self.font.finalize();
    let mut transition = self.transition.finalize();
    let mut animation = self.animation.finalize();
    let mut prefixed = self.prefix.finalize();

    let mut decls = Vec::with_capacity(background.len() + border.len() + outline.len() + flex.len() + align.len() + margin.len() + padding.len() + scroll_margin.len() + scroll_padding.len() + font.len() + transition.len() + animation.len() + prefixed.len());
    decls.extend(background.drain(..).map(|property| Declaration { property, important }));
    decls.extend(border.drain(..).map(|property| Declaration { property, important }));
    decls.extend(outline.drain(..).map(|property| Declaration { property, important }));
    decls.extend(flex.drain(..).map(|property| Declaration { property, important }));
    decls.extend(align.drain(..).map(|property| Declaration { property, important }));
    decls.extend(margin.drain(..).map(|property| Declaration { property, important }));
    decls.extend(padding.drain(..).map(|property| Declaration { property, important }));
    decls.extend(scroll_margin.drain(..).map(|property| Declaration { property, important }));
    decls.extend(scroll_padding.drain(..).map(|property| Declaration { property, important }));
    decls.extend(font.drain(..).map(|property| Declaration { property, important }));
    decls.extend(transition.drain(..).map(|property| Declaration { property, important }));
    decls.extend(animation.drain(..).map(|property| Declaration { property, important }));
    decls.extend(prefixed.drain(..).map(|property| Declaration { property, important }));
    decls
  }
}
