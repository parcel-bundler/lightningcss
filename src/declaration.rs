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
  display::DisplayHandler,
  transform::TransformHandler,
  text::TextDecorationHandler,
  position::PositionHandler,
  overflow::OverflowHandler,
  list::ListStyleHandler,
};
use crate::properties::prefixes::Browsers;
use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub struct DeclarationBlock {
  pub declarations: Vec<Declaration>
}

impl ToCss for DeclarationBlock {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    let len = self.declarations.len();
    for (i, decl) in self.declarations.iter().enumerate() {
      dest.newline()?;
      decl.to_css(dest)?;
      if i != len - 1 || !dest.minify {
        dest.write_char(';')?;
      }
    }
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

impl DeclarationBlock {
  pub fn minify(&mut self, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    let mut decls: Vec<Declaration> = vec![];
    for decl in self.declarations.iter() {
      let handled = 
        (decl.important && important_handler.handle_property(decl)) ||
        (!decl.important && handler.handle_property(decl));

      if !handled {
        decls.push(decl.clone());
      }
    }

    decls.extend(handler.finalize());
    decls.extend(important_handler.finalize());
    self.declarations = decls;
  }
}

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
pub struct DeclarationList {
  important: bool,
  pub declarations: Vec<Declaration>
}

impl DeclarationList {
  pub fn new(important: bool) -> DeclarationList {
    DeclarationList {
      important,
      declarations: Vec::new()
    }
  }

  pub fn push(&mut self, property: Property) {
    self.declarations.push(Declaration { property, important: self.important })
  }

  pub fn extend(&mut self, properties: &mut Vec<Property>) {
    let important = self.important;
    self.declarations.extend(properties.drain(..).map(|property| Declaration { property, important }))
  }
}

#[derive(Default)]
pub struct DeclarationHandler {
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
  text: TextDecorationHandler,
  list: ListStyleHandler,
  transition: TransitionHandler,
  animation: AnimationHandler,
  display: DisplayHandler,
  position: PositionHandler,
  inset: InsetHandler,
  overflow: OverflowHandler,
  transform: TransformHandler,
  prefix: PrefixHandler,
  decls: DeclarationList
}

impl DeclarationHandler {
  pub fn new(important: bool, targets: Option<Browsers>) -> Self {
    DeclarationHandler {
      background: BackgroundHandler::new(targets),
      border: BorderHandler::new(targets),
      flex: FlexHandler::new(targets),
      align: AlignHandler::new(targets),
      transition: TransitionHandler::new(targets),
      animation: AnimationHandler::new(targets),
      display: DisplayHandler::new(targets),
      position: PositionHandler::new(targets),
      transform: TransformHandler::new(targets),
      text: TextDecorationHandler::new(targets),
      prefix: PrefixHandler::new(targets),
      decls: DeclarationList::new(important),
      ..DeclarationHandler::default()
    }
  }

  pub fn handle_property(&mut self, decl: &Declaration) -> bool {
    let property = &decl.property;
    self.background.handle_property(property, &mut self.decls) ||
    self.border.handle_property(property, &mut self.decls) ||
    self.outline.handle_property(property, &mut self.decls) ||
    self.flex.handle_property(property, &mut self.decls) ||
    self.align.handle_property(property, &mut self.decls) ||
    self.margin.handle_property(property, &mut self.decls) ||
    self.padding.handle_property(property, &mut self.decls) ||
    self.scroll_margin.handle_property(property, &mut self.decls) ||
    self.scroll_padding.handle_property(property, &mut self.decls) ||
    self.font.handle_property(property, &mut self.decls) ||
    self.text.handle_property(property, &mut self.decls) ||
    self.list.handle_property(property, &mut self.decls) ||
    self.transition.handle_property(property, &mut self.decls) ||
    self.animation.handle_property(property, &mut self.decls) ||
    self.display.handle_property(property, &mut self.decls) ||
    self.position.handle_property(property, &mut self.decls) ||
    self.inset.handle_property(property, &mut self.decls) ||
    self.overflow.handle_property(property, &mut self.decls) ||
    self.transform.handle_property(property, &mut self.decls) ||
    self.prefix.handle_property(property, &mut self.decls)
  }

  pub fn finalize(&mut self) -> Vec<Declaration> {
    self.background.finalize(&mut self.decls);
    self.border.finalize(&mut self.decls);
    self.outline.finalize(&mut self.decls);
    self.flex.finalize(&mut self.decls);
    self.align.finalize(&mut self.decls);
    self.margin.finalize(&mut self.decls);
    self.padding.finalize(&mut self.decls);
    self.scroll_margin.finalize(&mut self.decls);
    self.scroll_padding.finalize(&mut self.decls);
    self.font.finalize(&mut self.decls);
    self.text.finalize(&mut self.decls);
    self.list.finalize(&mut self.decls);
    self.transition.finalize(&mut self.decls);
    self.animation.finalize(&mut self.decls);
    self.display.finalize(&mut self.decls);
    self.position.finalize(&mut self.decls);
    self.inset.finalize(&mut self.decls);
    self.overflow.finalize(&mut self.decls);
    self.transform.finalize(&mut self.decls);
    self.prefix.finalize(&mut self.decls);
    std::mem::take(&mut self.decls.declarations)
  }
}
