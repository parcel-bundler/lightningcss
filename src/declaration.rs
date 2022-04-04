use crate::context::PropertyHandlerContext;
use crate::error::{ParserError, PrinterError};
use crate::parser::ParserOptions;
use crate::printer::Printer;
use crate::properties::box_shadow::BoxShadowHandler;
use crate::properties::masking::MaskHandler;
use crate::properties::Property;
use crate::properties::{
  align::AlignHandler,
  animation::AnimationHandler,
  background::BackgroundHandler,
  border::BorderHandler,
  display::DisplayHandler,
  flex::FlexHandler,
  font::FontHandler,
  grid::GridHandler,
  list::ListStyleHandler,
  margin_padding::*,
  outline::OutlineHandler,
  overflow::OverflowHandler,
  position::PositionHandler,
  prefix_handler::{FallbackHandler, PrefixHandler},
  size::SizeHandler,
  text::TextDecorationHandler,
  transform::TransformHandler,
  transition::TransitionHandler,
};
use crate::targets::Browsers;
use crate::traits::{PropertyHandler, ToCss};
use cssparser::*;

#[derive(Debug, PartialEq, Clone)]
pub struct DeclarationBlock<'i> {
  pub important_declarations: Vec<Property<'i>>,
  pub declarations: Vec<Property<'i>>,
}

impl<'i> DeclarationBlock<'i> {
  pub fn parse<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut important_declarations = DeclarationList::new();
    let mut declarations = DeclarationList::new();
    let mut parser = DeclarationListParser::new(
      input,
      PropertyDeclarationParser {
        important_declarations: &mut important_declarations,
        declarations: &mut declarations,
        options,
      },
    );
    while let Some(res) = parser.next() {
      if let Err((err, _)) = res {
        return Err(err);
      }
    }

    Ok(DeclarationBlock {
      important_declarations,
      declarations,
    })
  }
}

impl<'i> ToCss for DeclarationBlock<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
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

impl<'i> DeclarationBlock<'i> {
  pub(crate) fn minify(
    &mut self,
    handler: &mut DeclarationHandler<'i>,
    important_handler: &mut DeclarationHandler<'i>,
    context: &mut PropertyHandlerContext<'i>,
  ) {
    macro_rules! handle {
      ($decls: expr, $handler: expr, $important: literal) => {
        for decl in $decls.iter() {
          context.is_important = $important;
          let handled = $handler.handle_property(decl, context);

          if !handled {
            $handler.decls.push(decl.clone());
          }
        }
      };
    }

    handle!(self.important_declarations, important_handler, true);
    handle!(self.declarations, handler, false);

    handler.finalize(context);
    important_handler.finalize(context);
    self.important_declarations = std::mem::take(&mut important_handler.decls);
    self.declarations = std::mem::take(&mut handler.decls);
  }
}

struct PropertyDeclarationParser<'a, 'i> {
  important_declarations: &'a mut Vec<Property<'i>>,
  declarations: &'a mut Vec<Property<'i>>,
  options: &'a ParserOptions,
}

/// Parse a declaration within {} block: `color: blue`
impl<'a, 'i> cssparser::DeclarationParser<'i> for PropertyDeclarationParser<'a, 'i> {
  type Declaration = ();
  type Error = ParserError<'i>;

  fn parse_value<'t>(
    &mut self,
    name: CowRcStr<'i>,
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self::Declaration, cssparser::ParseError<'i, Self::Error>> {
    parse_declaration(
      name,
      input,
      &mut self.declarations,
      &mut self.important_declarations,
      &self.options,
    )
  }
}

/// Default methods reject all at rules.
impl<'a, 'i> AtRuleParser<'i> for PropertyDeclarationParser<'a, 'i> {
  type Prelude = ();
  type AtRule = ();
  type Error = ParserError<'i>;
}

pub(crate) fn parse_declaration<'i, 't>(
  name: CowRcStr<'i>,
  input: &mut cssparser::Parser<'i, 't>,
  declarations: &mut DeclarationList<'i>,
  important_declarations: &mut DeclarationList<'i>,
  options: &ParserOptions,
) -> Result<(), cssparser::ParseError<'i, ParserError<'i>>> {
  let property = input.parse_until_before(Delimiter::Bang, |input| Property::parse(name, input, options))?;
  let important = input
    .try_parse(|input| {
      input.expect_delim('!')?;
      input.expect_ident_matching("important")
    })
    .is_ok();
  if important {
    important_declarations.push(property);
  } else {
    declarations.push(property);
  }
  Ok(())
}

pub(crate) type DeclarationList<'i> = Vec<Property<'i>>;

pub(crate) struct DeclarationHandler<'i> {
  background: BackgroundHandler<'i>,
  border: BorderHandler<'i>,
  outline: OutlineHandler,
  flex: FlexHandler,
  grid: GridHandler<'i>,
  align: AlignHandler,
  size: SizeHandler,
  margin: MarginHandler,
  padding: PaddingHandler,
  scroll_margin: ScrollMarginHandler,
  scroll_padding: ScrollPaddingHandler,
  font: FontHandler<'i>,
  text: TextDecorationHandler<'i>,
  list: ListStyleHandler<'i>,
  transition: TransitionHandler<'i>,
  animation: AnimationHandler<'i>,
  display: DisplayHandler<'i>,
  position: PositionHandler,
  inset: InsetHandler,
  overflow: OverflowHandler,
  transform: TransformHandler,
  box_shadow: BoxShadowHandler,
  mask: MaskHandler<'i>,
  fallback: FallbackHandler,
  prefix: PrefixHandler,
  decls: DeclarationList<'i>,
}

impl<'i> DeclarationHandler<'i> {
  pub fn new(targets: Option<Browsers>) -> Self {
    DeclarationHandler {
      background: BackgroundHandler::new(targets),
      border: BorderHandler::new(targets),
      outline: OutlineHandler::new(targets),
      flex: FlexHandler::new(targets),
      grid: GridHandler::default(),
      align: AlignHandler::new(targets),
      size: SizeHandler::default(),
      margin: MarginHandler::default(),
      padding: PaddingHandler::default(),
      scroll_margin: ScrollMarginHandler::default(),
      scroll_padding: ScrollPaddingHandler::default(),
      font: FontHandler::default(),
      text: TextDecorationHandler::new(targets),
      list: ListStyleHandler::new(targets),
      transition: TransitionHandler::new(targets),
      animation: AnimationHandler::new(targets),
      display: DisplayHandler::new(targets),
      position: PositionHandler::new(targets),
      inset: InsetHandler::default(),
      overflow: OverflowHandler::new(targets),
      transform: TransformHandler::new(targets),
      box_shadow: BoxShadowHandler::new(targets),
      mask: MaskHandler::default(),
      fallback: FallbackHandler::new(targets),
      prefix: PrefixHandler::new(targets),
      decls: DeclarationList::new(),
    }
  }

  pub fn handle_property(&mut self, property: &Property<'i>, context: &mut PropertyHandlerContext<'i>) -> bool {
    self.background.handle_property(property, &mut self.decls, context)
      || self.border.handle_property(property, &mut self.decls, context)
      || self.outline.handle_property(property, &mut self.decls, context)
      || self.flex.handle_property(property, &mut self.decls, context)
      || self.grid.handle_property(property, &mut self.decls, context)
      || self.align.handle_property(property, &mut self.decls, context)
      || self.size.handle_property(property, &mut self.decls, context)
      || self.margin.handle_property(property, &mut self.decls, context)
      || self.padding.handle_property(property, &mut self.decls, context)
      || self.scroll_margin.handle_property(property, &mut self.decls, context)
      || self.scroll_padding.handle_property(property, &mut self.decls, context)
      || self.font.handle_property(property, &mut self.decls, context)
      || self.text.handle_property(property, &mut self.decls, context)
      || self.list.handle_property(property, &mut self.decls, context)
      || self.transition.handle_property(property, &mut self.decls, context)
      || self.animation.handle_property(property, &mut self.decls, context)
      || self.display.handle_property(property, &mut self.decls, context)
      || self.position.handle_property(property, &mut self.decls, context)
      || self.inset.handle_property(property, &mut self.decls, context)
      || self.overflow.handle_property(property, &mut self.decls, context)
      || self.transform.handle_property(property, &mut self.decls, context)
      || self.box_shadow.handle_property(property, &mut self.decls, context)
      || self.mask.handle_property(property, &mut self.decls, context)
      || self.fallback.handle_property(property, &mut self.decls, context)
      || self.prefix.handle_property(property, &mut self.decls, context)
  }

  pub fn finalize(&mut self, context: &mut PropertyHandlerContext<'i>) {
    self.background.finalize(&mut self.decls, context);
    self.border.finalize(&mut self.decls, context);
    self.outline.finalize(&mut self.decls, context);
    self.flex.finalize(&mut self.decls, context);
    self.grid.finalize(&mut self.decls, context);
    self.align.finalize(&mut self.decls, context);
    self.size.finalize(&mut self.decls, context);
    self.margin.finalize(&mut self.decls, context);
    self.padding.finalize(&mut self.decls, context);
    self.scroll_margin.finalize(&mut self.decls, context);
    self.scroll_padding.finalize(&mut self.decls, context);
    self.font.finalize(&mut self.decls, context);
    self.text.finalize(&mut self.decls, context);
    self.list.finalize(&mut self.decls, context);
    self.transition.finalize(&mut self.decls, context);
    self.animation.finalize(&mut self.decls, context);
    self.display.finalize(&mut self.decls, context);
    self.position.finalize(&mut self.decls, context);
    self.inset.finalize(&mut self.decls, context);
    self.overflow.finalize(&mut self.decls, context);
    self.transform.finalize(&mut self.decls, context);
    self.box_shadow.finalize(&mut self.decls, context);
    self.mask.finalize(&mut self.decls, context);
    self.fallback.finalize(&mut self.decls, context);
    self.prefix.finalize(&mut self.decls, context);
  }
}
