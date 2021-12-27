use cssparser::*;
use crate::properties::PropertyId;
use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;
use crate::prefixes::Feature;

#[derive(Debug, Clone, PartialEq)]
pub struct CustomProperty {
  pub name: String,
  pub value: String
}

impl CustomProperty {
  pub fn parse<'i, 't>(
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ()>> {
    let value = parse_unknown_value(input)?;
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
    input: &mut Parser<'i, 't>
  ) -> Result<Self, ParseError<'i, ()>> {
    let value = parse_unknown_value(input)?;
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
}

fn parse_unknown_value<'i, 't>(input: &mut Parser<'i, 't>) -> Result<String, ParseError<'i, ()>> {
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
        Ok(_) => {
          has_two = true;
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
