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
  grid::GridHandler,
};
use crate::targets::Browsers;
use crate::parser::ParserOptions;
use crate::error::{ParserError, PrinterError};
use crate::logical::LogicalProperties;

#[derive(Debug, PartialEq)]
pub struct DeclarationBlock {
  pub declarations: Vec<Declaration>
}

impl DeclarationBlock {
  pub fn parse<'i, 't>(input: &mut Parser<'i, 't>, options: &ParserOptions) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut parser = DeclarationListParser::new(input, PropertyDeclarationParser { options });
    let mut declarations = vec![];
    while let Some(decl) = parser.next() {
      match decl {
        Ok(decl) => declarations.push(decl),
        Err((err, _)) => return Err(err)
      }
    }

    Ok(DeclarationBlock {
      declarations
    })
  }
}

impl ToCss for DeclarationBlock {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
  pub(crate) fn minify(
    &mut self,
    handler: &mut DeclarationHandler,
    important_handler: &mut DeclarationHandler,
    logical_properties: &mut LogicalProperties
  ) {
    let mut decls: Vec<Declaration> = vec![];
    for decl in self.declarations.iter() {
      let handled = 
        (decl.important && important_handler.handle_property(decl, logical_properties)) ||
        (!decl.important && handler.handle_property(decl, logical_properties));

      if !handled {
        decls.push(decl.clone());
      }
    }

    decls.extend(handler.finalize(logical_properties));
    decls.extend(important_handler.finalize(logical_properties));
    self.declarations = decls;
  }
}

struct PropertyDeclarationParser<'a> {
  options: &'a ParserOptions
}

/// Parse a declaration within {} block: `color: blue`
impl<'a, 'i> cssparser::DeclarationParser<'i> for PropertyDeclarationParser<'a> {
  type Declaration = Declaration;
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    Declaration::parse(name, input, self.options)
  }
}

/// Default methods reject all at rules.
impl<'a, 'i> AtRuleParser<'i> for PropertyDeclarationParser<'a> {
  type Prelude = ();
  type AtRule = Declaration;
  type Error = ParserError<'i>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
  pub property: Property,
  pub important: bool
}

impl Declaration {
  pub fn parse<'i, 't>(name: CowRcStr<'i>, input: &mut Parser<'i, 't>, options: &ParserOptions) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let property = input.parse_until_before(Delimiter::Bang, |input| Property::parse(name, input, options))?;
    let important = input.try_parse(|input| {
      input.expect_delim('!')?;
      input.expect_ident_matching("important")
    }).is_ok();
    Ok(Declaration { property, important })
  }
}

impl ToCss for Declaration {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    self.property.to_css(dest, self.important)
  }
}

#[derive(Default)]
pub(crate) struct DeclarationList {
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

pub(crate) struct DeclarationHandler {
  background: BackgroundHandler,
  border: BorderHandler,
  outline: OutlineHandler,
  flex: FlexHandler,
  grid: GridHandler,
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
      outline: OutlineHandler::default(),
      flex: FlexHandler::new(targets),
      grid: GridHandler::default(),
      align: AlignHandler::new(targets),
      margin: MarginHandler::default(),
      padding: PaddingHandler::default(),
      scroll_margin: ScrollMarginHandler::default(),
      scroll_padding: ScrollPaddingHandler::default(),
      font: FontHandler::default(),
      text: TextDecorationHandler::new(targets),
      list: ListStyleHandler::default(),
      transition: TransitionHandler::new(targets),
      animation: AnimationHandler::new(targets),
      display: DisplayHandler::new(targets),
      position: PositionHandler::new(targets),
      inset: InsetHandler::default(),
      overflow: OverflowHandler::new(targets),
      transform: TransformHandler::new(targets),
      prefix: PrefixHandler::new(targets),
      decls: DeclarationList::new(important)
    }
  }

  pub fn handle_property(&mut self, decl: &Declaration, logical_properties: &mut LogicalProperties) -> bool {
    let property = &decl.property;
    self.background.handle_property(property, &mut self.decls, logical_properties) ||
    self.border.handle_property(property, &mut self.decls, logical_properties) ||
    self.outline.handle_property(property, &mut self.decls, logical_properties) ||
    self.flex.handle_property(property, &mut self.decls, logical_properties) ||
    self.grid.handle_property(property, &mut self.decls, logical_properties) ||
    self.align.handle_property(property, &mut self.decls, logical_properties) ||
    self.margin.handle_property(property, &mut self.decls, logical_properties) ||
    self.padding.handle_property(property, &mut self.decls, logical_properties) ||
    self.scroll_margin.handle_property(property, &mut self.decls, logical_properties) ||
    self.scroll_padding.handle_property(property, &mut self.decls, logical_properties) ||
    self.font.handle_property(property, &mut self.decls, logical_properties) ||
    self.text.handle_property(property, &mut self.decls, logical_properties) ||
    self.list.handle_property(property, &mut self.decls, logical_properties) ||
    self.transition.handle_property(property, &mut self.decls, logical_properties) ||
    self.animation.handle_property(property, &mut self.decls, logical_properties) ||
    self.display.handle_property(property, &mut self.decls, logical_properties) ||
    self.position.handle_property(property, &mut self.decls, logical_properties) ||
    self.inset.handle_property(property, &mut self.decls, logical_properties) ||
    self.overflow.handle_property(property, &mut self.decls, logical_properties) ||
    self.transform.handle_property(property, &mut self.decls, logical_properties) ||
    self.prefix.handle_property(property, &mut self.decls, logical_properties)
  }

  pub fn finalize(&mut self, logical_properties: &mut LogicalProperties) -> Vec<Declaration> {
    self.background.finalize(&mut self.decls, logical_properties);
    self.border.finalize(&mut self.decls, logical_properties);
    self.outline.finalize(&mut self.decls, logical_properties);
    self.flex.finalize(&mut self.decls, logical_properties);
    self.grid.finalize(&mut self.decls, logical_properties);
    self.align.finalize(&mut self.decls, logical_properties);
    self.margin.finalize(&mut self.decls, logical_properties);
    self.padding.finalize(&mut self.decls, logical_properties);
    self.scroll_margin.finalize(&mut self.decls, logical_properties);
    self.scroll_padding.finalize(&mut self.decls, logical_properties);
    self.font.finalize(&mut self.decls, logical_properties);
    self.text.finalize(&mut self.decls, logical_properties);
    self.list.finalize(&mut self.decls, logical_properties);
    self.transition.finalize(&mut self.decls, logical_properties);
    self.animation.finalize(&mut self.decls, logical_properties);
    self.display.finalize(&mut self.decls, logical_properties);
    self.position.finalize(&mut self.decls, logical_properties);
    self.inset.finalize(&mut self.decls, logical_properties);
    self.overflow.finalize(&mut self.decls, logical_properties);
    self.transform.finalize(&mut self.decls, logical_properties);
    self.prefix.finalize(&mut self.decls, logical_properties);
    std::mem::take(&mut self.decls.declarations)
  }
}
