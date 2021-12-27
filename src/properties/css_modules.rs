use cssparser::*;
use crate::values::ident::CustomIdent;
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;

/// The `composes` property from CSS modules.
/// https://github.com/css-modules/css-modules/#dependencies
#[derive(Debug, Clone, PartialEq)]
pub struct Composes {
  pub name: CustomIdent,
  pub from: Option<ComposesFrom>
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComposesFrom {
  Global,
  File(String)
}

impl Parse for Composes {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let name = CustomIdent::parse(input)?;

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
      name,
      from
    })
  }
}

impl ToCss for Composes {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.name.to_css(dest)?;
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
