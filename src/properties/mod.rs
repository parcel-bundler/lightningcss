mod custom;

use cssparser::*;
use custom::*;

#[derive(Debug)]
pub enum Property {
  BackgroundColor(Color),
  Color(Color),
  Custom(CustomProperty)
}

impl Property {
  pub fn parse<'i, 't>(name: CowRcStr<'i>, input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    macro_rules! property {
      ($property: ident, $type: ident) => {
        if let Ok(c) = $type::parse(input) {
          return Ok(Property::$property(c))
        }
      };
    }    
    
    let state = input.state();
    match name.as_ref() {
      "background-color" => property!(BackgroundColor, Color),
      "color" => property!(Color, Color),
      _ => {}
    }

    input.reset(&state);
    return Ok(Property::Custom(CustomProperty::parse(name, input)?))
  }
}
