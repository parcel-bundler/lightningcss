use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use super::Property;
use crate::values::{image::Image, ident::CustomIdent};
use crate::declaration::DeclarationList;
use crate::macros::{enum_property, shorthand_property, shorthand_handler};
use crate::printer::Printer;

/// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#text-markers
#[derive(Debug, Clone, PartialEq)]
pub enum ListStyleType {
  None,
  CounterStyle(CounterStyle),
  String(String)
}

impl Default for ListStyleType {
  fn default() -> ListStyleType {
    ListStyleType::CounterStyle(CounterStyle::Name(CustomIdent("disc".into())))
  }
}

impl Parse for ListStyleType {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(val) = input.try_parse(CounterStyle::parse) {
      return Ok(ListStyleType::CounterStyle(val))
    }

    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(ListStyleType::None)
    }

    let s = input.expect_string()?.as_ref().to_owned();
    Ok(ListStyleType::String(s))
  }
}

impl ToCss for ListStyleType {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      ListStyleType::None => dest.write_str("none"),
      ListStyleType::CounterStyle(style) => style.to_css(dest),
      ListStyleType::String(s) => serialize_string(&s, dest)
    }
  }
}

/// https://www.w3.org/TR/css-counter-styles-3/#typedef-counter-style
#[derive(Debug, Clone, PartialEq)]
pub enum CounterStyle {
  Name(CustomIdent),
  Symbols(SymbolsType, Vec<Symbol>)
}

impl Parse for CounterStyle {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_function_matching("symbols")).is_ok() {
      return input.parse_nested_block(|input| {
        let t = input.try_parse(SymbolsType::parse).unwrap_or(SymbolsType::Symbolic);
        println!("{:?}", t);

        let mut symbols = Vec::new();
        while let Ok(s) = input.try_parse(Symbol::parse) {
          symbols.push(s);
        }

        Ok(CounterStyle::Symbols(t, symbols))
      })
    }

    let name = CustomIdent::parse(input)?;
    Ok(CounterStyle::Name(name))
  }
}

impl ToCss for CounterStyle {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      CounterStyle::Name(name) => name.to_css(dest),
      CounterStyle::Symbols(t, symbols) => {
        dest.write_str("symbols(")?;
        let mut needs_space = false;
        if *t != SymbolsType::Symbolic {
          t.to_css(dest)?;
          needs_space = true;
        }
        
        for symbol in symbols {
          if needs_space {
            dest.write_char(' ')?;
          }
          symbol.to_css(dest)?;
          needs_space = true;
        }
        dest.write_char(')')
      }
    }
  }
}

enum_property!(SymbolsType,
  Cyclic,
  Numeric,
  Alphabetic,
  Symbolic,
  Fixed
);

/// https://www.w3.org/TR/css-counter-styles-3/#funcdef-symbols
#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
  String(String),
  Image(Image)
}

impl Parse for Symbol {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(img) = input.try_parse(Image::parse) {
      return Ok(Symbol::Image(img))
    }

    let s = input.expect_string()?.as_ref().to_owned();
    Ok(Symbol::String(s))
  }
}

impl ToCss for Symbol {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      Symbol::String(s) => serialize_string(&s, dest),
      Symbol::Image(img) => img.to_css(dest)
    }
  }
}

// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-position-property
enum_property!(ListStylePosition,
  Inside,
  Outside
);

impl Default for ListStylePosition {
  fn default() -> ListStylePosition {
    ListStylePosition::Outside
  }
}

// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#marker-side
enum_property!(MarkerSide,
  ("match-self", MatchSelf),
  ("match-parent", MatchParent)
);

// https://www.w3.org/TR/2020/WD-css-lists-3-20201117/#list-style-property
shorthand_property!(ListStyle {
  list_style_type: ListStyleType,
  image: Image,
  position: ListStylePosition,
});

shorthand_handler!(ListStyleHandler -> ListStyle {
  list_style_type: ListStyleType(ListStyleType),
  image: ListStyleImage(Image),
  position: ListStylePosition(ListStylePosition),
});
