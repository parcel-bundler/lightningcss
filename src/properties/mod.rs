mod custom;

use cssparser::*;
use custom::*;
use crate::values::{image::*};

#[derive(Debug)]
pub enum Property {
  BackgroundColor(Color),
  BackgroundImage(Vec<Image>),
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
      ($property: ident, $type: ident, $multi: expr) => {
        if let Ok(c) = input.parse_comma_separated(|input| $type::parse(input)) {
          return Ok(Property::$property(c))
        }
      }
    }
    
    let state = input.state();
    match name.as_ref() {
      "background-color" => property!(BackgroundColor, Color),
      "background-image" => property!(BackgroundImage, Image, true),
      "color" => property!(Color, Color),
      _ => {}
    }

    input.reset(&state);
    return Ok(Property::Custom(CustomProperty::parse(name, input)?))
  }
}
