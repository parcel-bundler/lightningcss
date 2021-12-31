use cssparser::*;
use crate::values::ident::CustomIdent;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use smallvec::SmallVec;
use crate::error::ParserError;

/// The `composes` property from CSS modules.
/// https://github.com/css-modules/css-modules/#dependencies
#[derive(Debug, Clone, PartialEq)]
pub struct Composes {
  pub names: SmallVec<[CustomIdent; 1]>,
  pub from: Option<ComposesFrom>
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComposesFrom {
  Global,
  File(String)
}

impl Parse for Composes {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut names = SmallVec::new();
    while let Ok(name) = input.try_parse(parse_one_ident) {
      names.push(name);
    }

    if names.is_empty() {
      return Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }

    let from = if input.try_parse(|input| input.expect_ident_matching("from")).is_ok() {
      if let Ok(file) = input.try_parse(|input| input.expect_string().map(|s| s.as_ref().to_owned())) {
        Some(ComposesFrom::File(file))
      } else {
        input.expect_ident_matching("global")?;
        Some(ComposesFrom::Global)
      }
    } else {
      None
    };

    Ok(Composes {
      names,
      from
    })
  }
}

fn parse_one_ident<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CustomIdent, ParseError<'i, ParserError<'i>>> {
  let name = CustomIdent::parse(input)?;
  if name.0.eq_ignore_ascii_case("from") {
    return Err(input.new_error_for_next_token())
  }

  Ok(name)
}

impl ToCss for Composes {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
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
        ComposesFrom::File(file) => serialize_string(&file, dest)?
      }
    }

    Ok(())
  }
}
