use cssparser::*;
use crate::properties::PropertyId;
use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;
use crate::prefixes::Feature;
use crate::error::ParserError;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct CustomProperty {
  pub name: String,
  pub value: String
}

impl CustomProperty {
  pub fn parse<'i, 't>(
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
    used_vars: &mut Option<HashSet<String>>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = parse_unknown_value(input, used_vars)?;
    Ok(CustomProperty {
      name: name.as_ref().into(),
      value
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnparsedProperty {
  pub property_id: PropertyId,
  pub value: String
}

impl UnparsedProperty {
  pub fn parse<'i, 't>(
    property_id: PropertyId,
    input: &mut Parser<'i, 't>,
    used_vars: &mut Option<HashSet<String>>
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = parse_unknown_value(input, used_vars)?;
    Ok(UnparsedProperty {
      property_id,
      value
    })
  }

  pub fn get_prefixed(&self, targets: Option<Browsers>, feature: Feature) -> UnparsedProperty {
    let mut clone = self.clone();
    if self.property_id.prefix().contains(VendorPrefix::None) {
      if let Some(targets) = targets {
        clone.property_id = clone.property_id.with_prefix(feature.prefixes_for(targets))
      }
    }
    clone
  }

  pub fn with_property_id(&self, property_id: PropertyId) -> UnparsedProperty {
    UnparsedProperty {
      property_id,
      value: self.value.clone()
    }
  }
}

fn parse_unknown_value<'i, 't>(
  input: &mut Parser<'i, 't>,
  used_vars: &mut Option<HashSet<String>>,
) -> Result<String, ParseError<'i, ParserError<'i>>> {
  input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
    // Need at least one token
    let before_first = input.position();
    let first_is_whitespace = {
      let first = input.next_including_whitespace()?;
      matches!(first, Token::WhiteSpace(_))
    };

    let after_first = input.position();
    let mut has_two = false;
    loop {
      match input.next_including_whitespace_and_comments() {
        Ok(token) => {
          has_two = true;
          if let Some(vars) = used_vars {
            if is_var(token) {
              // Ignore errors.
              let _ = parse_var(input, vars);
            }
          }
        },
        Err(..) => {
          // If there is only one token, preserve it, even if it is whitespace.
          // e.g. `--foo: ;` is valid. 
          let mut slice = if !has_two || !first_is_whitespace {
            input.slice_from(before_first)
          } else {
            input.slice_from(after_first)
          };
          if has_two {
            slice = slice.trim_end();
          }
          return Ok(slice.into())
        },
      };
    }
  })
}

#[inline]
fn is_var(token: &Token) -> bool {
  matches!(token, Token::Function(f) if f.eq_ignore_ascii_case("var"))
}

fn parse_var<'i, 't>(
  input: &mut Parser<'i, 't>,
  used_vars: &mut HashSet<String>,
) -> Result<(), ParseError<'i, ParserError<'i>>> {
  input.parse_nested_block(|input| -> Result<(), ParseError<'i, ParserError<'i>>> {
    let name = input.expect_ident()?.as_ref().to_owned();
    used_vars.insert(name);
    if input.try_parse(|input| input.expect_comma()).is_ok() {
      parse_fallback(input, used_vars)?;
    }
    Ok(())
  })
}

fn parse_fallback<'i, 't>(
  input: &mut Parser<'i, 't>,
  used_vars: &mut HashSet<String>,
) -> Result<(), ParseError<'i, ParserError<'i>>> {
  while let Ok(token) = input.next() {
    if is_var(token) {
      parse_var(input, used_vars)?;
    }
  }
  Ok(())
}
