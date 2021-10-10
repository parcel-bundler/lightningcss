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

  pub fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    use Property::*;

    macro_rules! property {
      ($prop: literal, $value: expr) => {{
        dest.write_str($prop)?;
        dest.write_str(": ")?;
        $value.to_css(dest)?;
        dest.write_str(";")
      }};
      ($prop: literal, $value: expr, $multi: expr) => {{
        dest.write_str($prop)?;
        dest.write_str(": ")?;
        let len = $value.len();
        for (idx, val) in $value.iter().enumerate() {
          val.to_css(dest)?;
          if idx < len - 1 {
            dest.write_str(", ")?;
          }
        }
        dest.write_str(";")?;
        Ok(())
      }};
    }

    match self {
      BackgroundColor(color) => property!("background-color", color),
      BackgroundImage(image) => property!("background-image", image, true),
      Color(color) => property!("color", color),
      Custom(custom) => {
        dest.write_str(custom.name.as_ref())?;
        dest.write_str(": ")?;
        dest.write_str(custom.value.as_ref())?;
        dest.write_str(";")
      }
    }
  }
}
