use cssparser::*;

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
    input.skip_whitespace();
    input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
      // Need at least one token
      let start = input.state();
      input.next_including_whitespace()?;
      input.reset(&start);

      // parse_declaration_value_block(input, references, missing_closing_characters)
      let start = input.position();
      loop {
        match input.next_including_whitespace_and_comments() {
          Ok(_) => {},
          Err(..) => {
            return Ok(CustomProperty {
              name: name.as_ref().into(),
              value: input.slice_from(start).trim_end().into()
            })
          },
        };
      }
    })
  }
}
