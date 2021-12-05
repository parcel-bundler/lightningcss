use cssparser::*;
use crate::properties::PropertyId;
use crate::vendor_prefix::VendorPrefix;

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
  pub vendor_prefix: VendorPrefix,
  pub value: String
}

impl UnparsedProperty {
  pub fn parse<'i, 't>(
    property_id: PropertyId,
    vendor_prefix: VendorPrefix,
    input: &mut Parser<'i, 't>
  ) -> Result<Self, ParseError<'i, ()>> {
    let value = parse_unknown_value(input)?;
    Ok(UnparsedProperty {
      property_id,
      vendor_prefix,
      value
    })
  }
}

fn parse_unknown_value<'i, 't>(input: &mut Parser<'i, 't>) -> Result<String, ParseError<'i, ()>> {
  input.skip_whitespace();
  input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
    // Need at least one token
    let start = input.state();
    input.next_including_whitespace()?;
    input.reset(&start);

    let start = input.position();
    loop {
      match input.next_including_whitespace_and_comments() {
        Ok(_) => {},
        Err(..) => {
          return Ok(input.slice_from(start).trim_end().into())
        },
      };
    }
  })
}
