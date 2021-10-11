mod custom;

use cssparser::*;
use custom::*;
use crate::values::{image::*, length::*};

#[derive(Debug)]
pub enum Property {
  BackgroundColor(Color),
  BackgroundImage(Vec<Image>),
  Color(Color),
  Custom(CustomProperty),

  Width(Size),
  Height(Size),
  MinWidth(MinMaxSize),
  MinHeight(MinMaxSize),
  MaxWidth(MinMaxSize),
  MaxHeight(MinMaxSize),
  BlockSize(Size),
  InlineSize(Size),
  MinBlockSize(MinMaxSize),
  MinInlineSize(MinMaxSize),
  MaxBlockSize(MinMaxSize),
  MaxInlineSize(MinMaxSize),

  Top(LengthPercentageOrAuto),
  Bottom(LengthPercentageOrAuto),
  Left(LengthPercentageOrAuto),
  Right(LengthPercentageOrAuto),
  InsetBlockStart(LengthPercentageOrAuto),
  InsetBlockEnd(LengthPercentageOrAuto),
  InsetInlineStart(LengthPercentageOrAuto),
  InsetInlineEnd(LengthPercentageOrAuto)
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
      "width" => property!(Width, Size),
      "height" => property!(Height, Size),
      "min-width" => property!(MinWidth, MinMaxSize),
      "min-height" => property!(MinHeight, MinMaxSize),
      "max-width" => property!(MaxWidth, MinMaxSize),
      "max-height" => property!(MaxHeight, MinMaxSize),
      "block-size" => property!(BlockSize, Size),
      "inline-size" => property!(InlineSize, Size),
      "min-block-size" => property!(MinBlockSize, MinMaxSize),
      "min-inline-size" => property!(MinInlineSize, MinMaxSize),
      "max-block-size" => property!(MaxBlockSize, MinMaxSize),
      "max-inline-size" => property!(MaxInlineSize, MinMaxSize),
      "top" => property!(Top, LengthPercentageOrAuto),
      "bottom" => property!(Bottom, LengthPercentageOrAuto),
      "left" => property!(Left, LengthPercentageOrAuto),
      "right" => property!(Right, LengthPercentageOrAuto),
      "inset-block-start" => property!(InsetBlockStart, LengthPercentageOrAuto),
      "inset-block-end" => property!(InsetBlockEnd, LengthPercentageOrAuto),
      "inset-inline-start" => property!(InsetInlineStart, LengthPercentageOrAuto),
      "inset-inline-end" => property!(InsetInlineEnd, LengthPercentageOrAuto),
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
      Width(val) => property!("width", val),
      Height(val) => property!("height", val),
      MinWidth(val) => property!("min-width", val),
      MinHeight(val) => property!("min-height", val),
      MaxWidth(val) => property!("max-width", val),
      MaxHeight(val) => property!("max-height", val),
      BlockSize(val) => property!("block-size", val),
      InlineSize(val) => property!("inline-size", val),
      MinBlockSize(val) => property!("min-block-size", val),
      MinInlineSize(val) => property!("min-inline-size", val),
      MaxBlockSize(val) => property!("max-block-size", val),
      MaxInlineSize(val) => property!("max-inline-size", val),
      Top(val) => property!("top", val),
      Bottom(val) => property!("bottom", val),
      Left(val) => property!("left", val),
      Right(val) => property!("right", val),
      InsetBlockStart(val) => property!("inset-block-start", val),
      InsetBlockEnd(val) => property!("inset-block-end", val),
      InsetInlineStart(val) => property!("inset-inline-start", val),
      InsetInlineEnd(val) => property!("inset-inline-end", val),
      Custom(custom) => {
        dest.write_str(custom.name.as_ref())?;
        dest.write_str(": ")?;
        dest.write_str(custom.value.as_ref())?;
        dest.write_str(";")
      }
    }
  }
}
