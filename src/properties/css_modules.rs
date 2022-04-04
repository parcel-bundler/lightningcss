use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::ident::{CustomIdent, CustomIdentList};
use crate::values::string::CowArcStr;
use cssparser::*;
use smallvec::SmallVec;

/// The `composes` property from CSS modules.
/// https://github.com/css-modules/css-modules/#dependencies
#[derive(Debug, Clone, PartialEq)]
pub struct Composes<'i> {
  pub names: CustomIdentList<'i>,
  pub from: Option<ComposesFrom<'i>>,
  pub loc: SourceLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComposesFrom<'i> {
  Global,
  File(CowArcStr<'i>),
}

impl<'i> Parse<'i> for Composes<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let mut names = SmallVec::new();
    while let Ok(name) = input.try_parse(parse_one_ident) {
      names.push(name);
    }

    if names.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration));
    }

    let from = if input.try_parse(|input| input.expect_ident_matching("from")).is_ok() {
      if let Ok(file) = input.try_parse(|input| input.expect_string_cloned()) {
        Some(ComposesFrom::File(file.into()))
      } else {
        input.expect_ident_matching("global")?;
        Some(ComposesFrom::Global)
      }
    } else {
      None
    };

    Ok(Composes { names, from, loc })
  }
}

fn parse_one_ident<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<CustomIdent<'i>, ParseError<'i, ParserError<'i>>> {
  let name = CustomIdent::parse(input)?;
  if name.0.eq_ignore_ascii_case("from") {
    return Err(input.new_error_for_next_token());
  }

  Ok(name)
}

impl ToCss for Composes<'_> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    for name in &self.names {
      if first {
        first = false;
      } else {
        dest.write_char(' ')?;
      }
      name.to_css(dest)?;
    }

    if let Some(from) = &self.from {
      dest.write_str(" from ")?;
      match from {
        ComposesFrom::Global => dest.write_str("global")?,
        ComposesFrom::File(file) => serialize_string(&file, dest)?,
      }
    }

    Ok(())
  }
}
