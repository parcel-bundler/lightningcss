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

#[derive(Debug, PartialEq)]
pub struct DeclarationBlock {
  pub important_declarations: Vec<Property>,
  pub declarations: Vec<Property>
}

impl DeclarationBlock {
  pub fn parse<'i, 't>(input: &mut Parser<'i, 't>, options: &ParserOptions) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut important_declarations = DeclarationList::new();
    let mut declarations = DeclarationList::new();
    let mut parser = DeclarationListParser::new(input, PropertyDeclarationParser {
      important_declarations: &mut important_declarations,
      declarations: &mut declarations,
      options
    });
    while let Some(res) = parser.next() {
      if let Err((err, _)) = res {
        return Err(err)
      }
    }

    Ok(DeclarationBlock {
      important_declarations,
      declarations
    })
  }
}

impl ToCss for DeclarationBlock {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();

    let mut i = 0;
    let len = self.declarations.len() + self.important_declarations.len();

    macro_rules! write {
      ($decls: expr, $important: literal) => {
        for decl in &$decls {
          dest.newline()?;
          decl.to_css(dest, $important)?;
          if i != len - 1 || !dest.minify {
            dest.write_char(';')?;
          }
          i += 1;
        }
      };
    }

    write!(self.declarations, false);
    write!(self.important_declarations, true);
    
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

impl DeclarationBlock {
  pub(crate) fn minify(&mut self, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    macro_rules! handle {
      ($decls: expr, $handler: expr) => {
        for decl in $decls.iter() {
          let handled = $handler.handle_property(decl);
    
          if !handled {
            $handler.decls.push(decl.clone());
          }
        }
      };
    }

    handle!(self.important_declarations, important_handler);
    handle!(self.declarations, handler);

    handler.finalize();
    important_handler.finalize();
    self.important_declarations = std::mem::take(&mut important_handler.decls);
    self.declarations = std::mem::take(&mut handler.decls);
  }
}

struct PropertyDeclarationParser<'a> {
  important_declarations: &'a mut Vec<Property>,
  declarations: &'a mut Vec<Property>,
  options: &'a ParserOptions
}

/// Parse a declaration within {} block: `color: blue`
impl<'a, 'i> cssparser::DeclarationParser<'i> for PropertyDeclarationParser<'a> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    parse_declaration(name, input, &mut self.declarations, &mut self.important_declarations, &self.options)
  }
}

/// Default methods reject all at rules.
impl<'a, 'i> AtRuleParser<'i> for PropertyDeclarationParser<'a> {
  type Prelude = ();
  type AtRule = ();
  type Error = ParserError<'i>;
}

pub(crate) fn parse_declaration<'i, 't>(
  name: CowRcStr<'i>,
  input: &mut cssparser::Parser<'i, 't>,
  declarations: &mut DeclarationList,
  important_declarations: &mut DeclarationList,
  options: &ParserOptions
) -> Result<(), cssparser::ParseError<'i, ParserError<'i>>> {
  let property = input.parse_until_before(Delimiter::Bang, |input| Property::parse(name, input, options))?;
  let important = input.try_parse(|input| {
    input.expect_delim('!')?;
    input.expect_ident_matching("important")
  }).is_ok();
  if important {
    important_declarations.push(property);
  } else {
    declarations.push(property);
  }
  Ok(())
}

pub(crate) type DeclarationList = Vec<Property>;

#[derive(Default)]
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
  pub fn new(targets: Option<Browsers>) -> Self {
    DeclarationHandler {
      background: BackgroundHandler::new(targets),
      border: BorderHandler::new(targets),
      flex: FlexHandler::new(targets),
      align: AlignHandler::new(targets),
      transition: TransitionHandler::new(targets),
      animation: AnimationHandler::new(targets),
      display: DisplayHandler::new(targets),
      position: PositionHandler::new(targets),
      overflow: OverflowHandler::new(targets),
      transform: TransformHandler::new(targets),
      text: TextDecorationHandler::new(targets),
      prefix: PrefixHandler::new(targets),
      decls: DeclarationList::new(),
      ..DeclarationHandler::default()
    }
  }

  pub fn handle_property(&mut self, property: &Property) -> bool {
    self.background.handle_property(property, &mut self.decls) ||
    self.border.handle_property(property, &mut self.decls) ||
    self.outline.handle_property(property, &mut self.decls) ||
    self.flex.handle_property(property, &mut self.decls) ||
    self.grid.handle_property(property, &mut self.decls) ||
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

  pub fn finalize(&mut self) {
    self.background.finalize(&mut self.decls);
    self.border.finalize(&mut self.decls);
    self.outline.finalize(&mut self.decls);
    self.flex.finalize(&mut self.decls);
    self.grid.finalize(&mut self.decls);
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
  }
}
