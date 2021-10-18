mod custom;

use cssparser::*;
use custom::*;
use crate::values::{image::*, length::*, border::*, rect::*, color::*};
use super::values::traits::Parse;

#[derive(Debug, Clone)]
pub enum Property {
  BackgroundColor(CssColor),
  BackgroundImage(Vec<Image>),
  // BackgroundPositionX
  // BackgroundPositionY
  // BackgroundPosition
  // BackgroundSize
  // BackgroundRepeat
  // BackgroundAttachment
  // BackgroundClip
  // BackgroundOrigin
  // BackgroundBlendMode
  // Background

  Color(CssColor),
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
  InsetInlineEnd(LengthPercentageOrAuto),

  BorderTopColor(CssColor),
  BorderBottomColor(CssColor),
  BorderLeftColor(CssColor),
  BorderRightColor(CssColor),
  BorderBlockStartColor(CssColor),
  BorderBlockEndColor(CssColor),
  BorderInlineStartColor(CssColor),
  BorderInlineEndColor(CssColor),

  BorderTopStyle(BorderStyle),
  BorderBottomStyle(BorderStyle),
  BorderLeftStyle(BorderStyle),
  BorderRightStyle(BorderStyle),
  BorderBlockStartStyle(BorderStyle),
  BorderBlockEndStyle(BorderStyle),
  BorderInlineStartStyle(BorderStyle),
  BorderInlineEndStyle(BorderStyle),

  BorderTopWidth(BorderSideWidth),
  BorderBottomWidth(BorderSideWidth),
  BorderLeftWidth(BorderSideWidth),
  BorderRightWidth(BorderSideWidth),
  BorderBlockStartWidth(BorderSideWidth),
  BorderBlockEndWidth(BorderSideWidth),
  BorderInlineStartWidth(BorderSideWidth),
  BorderInlineEndWidth(BorderSideWidth),

  // BorderBlock
  // BorderInline

  // BorderTopLeftRadius
  // BorderTopRightRadius
  // BorderBottomLeftRadius
  // BorderBottomRightRadius
  // BorderStartStartRadius
  // BorderStartEndRadius
  // BorderEndStartRadius
  // BorderEndEndRadius
  // BorderRadius

  // BorderImageSource
  // BorderImageOutset
  // BorderImageRepeat
  // BorderImageWidth
  // BorderImageSlice
  // BorderImage

  BorderColor(Rect<CssColor>),
  BorderStyle(Rect<BorderStyle>),
  BorderWidth(Rect<BorderSideWidth>),

  BorderBlockColor(CssColor),
  BorderBlockStyle(BorderStyle),
  BorderBlockWidth(BorderSideWidth),

  BorderInlineColor(CssColor),
  BorderInlineStyle(BorderStyle),
  BorderInlineWidth(BorderSideWidth),

  Border(Border),
  BorderTop(Border),
  BorderBottom(Border),
  BorderLeft(Border),
  BorderRight(Border),
  BorderBlock(Border),
  BorderBlockStart(Border),
  BorderBlockEnd(Border),
  BorderInline(Border),
  BorderInlineStart(Border),
  BorderInlineEnd(Border)

  // Margin
  // MarginBlock
  // MarginInline
  // Padding
  // ScrollMargin
  // ScrollPadding
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
      "background-color" => property!(BackgroundColor, CssColor),
      "background-image" => property!(BackgroundImage, Image, true),
      "color" => property!(Color, CssColor),
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
      "border-top-color" => property!(BorderTopColor, CssColor),
      "border-bottom-color" => property!(BorderBottomColor, CssColor),
      "border-left-color" => property!(BorderLeftColor, CssColor),
      "border-right-color" => property!(BorderRightColor, CssColor),
      "border-block-start-color" => property!(BorderBlockStartColor, CssColor),
      "border-block-end-color" => property!(BorderBlockEndColor, CssColor),
      "border-inline-start-color" => property!(BorderInlineStartColor, CssColor),
      "border-inline-end-color" => property!(BorderInlineEndColor, CssColor),
      "border-top-style" => property!(BorderTopStyle, BorderStyle),
      "border-bottom-style" => property!(BorderBottomStyle, BorderStyle),
      "border-left-style" => property!(BorderLeftStyle, BorderStyle),
      "border-right-style" => property!(BorderRightStyle, BorderStyle),
      "border-block-start-style" => property!(BorderBlockStartStyle, BorderStyle),
      "border-block-end-style" => property!(BorderBlockEndStyle, BorderStyle),
      "border-inline-start-style" => property!(BorderInlineStartStyle, BorderStyle),
      "border-inline-end-style" => property!(BorderInlineEndStyle, BorderStyle),
      "border-top-width" => property!(BorderTopWidth, BorderSideWidth),
      "border-bottom-width" => property!(BorderBottomWidth, BorderSideWidth),
      "border-left-width" => property!(BorderLeftWidth, BorderSideWidth),
      "border-right-width" => property!(BorderRightWidth, BorderSideWidth),
      "border-block-start-width" => property!(BorderBlockStartWidth, BorderSideWidth),
      "border-block-end-width" => property!(BorderBlockEndWidth, BorderSideWidth),
      "border-inline-start-width" => property!(BorderInlineStartWidth, BorderSideWidth),
      "border-inline-end-width" => property!(BorderInlineEndWidth, BorderSideWidth),
      "border-color" => property!(BorderColor, Rect),
      "border-style" => property!(BorderStyle, Rect),
      "border-width" => property!(BorderWidth, Rect),
      "border-block-color" => property!(BorderBlockColor, CssColor),
      "border-block-style" => property!(BorderBlockStyle, BorderStyle),
      "border-block-width" => property!(BorderBlockWidth, BorderSideWidth),
      "border-inline-color" => property!(BorderInlineColor, CssColor),
      "border-inline-style" => property!(BorderInlineStyle, BorderStyle),
      "border-inline-width" => property!(BorderInlineWidth, BorderSideWidth),
      "border" => property!(Border, Border),
      "border-top" => property!(BorderTop, Border),
      "border-bottom" => property!(BorderBottom, Border),
      "border-left" => property!(BorderLeft, Border),
      "border-right" => property!(BorderRight, Border),
      "border-block" => property!(BorderBlock, Border),
      "border-block-start" => property!(BorderBlockStart, Border),
      "border-block-end" => property!(BorderBlockEnd, Border),
      "border-inline" => property!(BorderInline, Border),
      "border-inline-start" => property!(BorderInlineStart, Border),
      "border-inline-end" => property!(BorderInlineEnd, Border),
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
      BorderTopColor(val) => property!("border-top-color", val),
      BorderBottomColor(val) => property!("border-bottom-color", val),
      BorderLeftColor(val) => property!("border-left-color", val),
      BorderRightColor(val) => property!("border-right-color", val),
      BorderBlockStartColor(val) => property!("border-block-start-color", val),
      BorderBlockEndColor(val) => property!("border-block-end-color", val),
      BorderInlineStartColor(val) => property!("border-inline-start-color", val),
      BorderInlineEndColor(val) => property!("border-inline-end-color", val),
      BorderTopStyle(val) => property!("border-top-style", val),
      BorderBottomStyle(val) => property!("border-bottom-style", val),
      BorderLeftStyle(val) => property!("border-left-style", val),
      BorderRightStyle(val) => property!("border-right-style", val),
      BorderBlockStartStyle(val) => property!("border-block-start-style", val),
      BorderBlockEndStyle(val) => property!("border-block-end-style", val),
      BorderInlineStartStyle(val) => property!("border-inline-start-style", val),
      BorderInlineEndStyle(val) => property!("border-inline-end-style", val),
      BorderTopWidth(val) => property!("border-top-width", val),
      BorderBottomWidth(val) => property!("border-bottom-width", val),
      BorderLeftWidth(val) => property!("border-left-width", val),
      BorderRightWidth(val) => property!("border-right-width", val),
      BorderBlockStartWidth(val) => property!("border-block-start-width", val),
      BorderBlockEndWidth(val) => property!("border-block-end-width", val),
      BorderInlineStartWidth(val) => property!("border-inline-start-width", val),
      BorderInlineEndWidth(val) => property!("border-inline-end-width", val),
      BorderColor(val) => property!("border-color", val),
      BorderStyle(val) => property!("border-style", val),
      BorderWidth(val) => property!("border-width", val),
      BorderBlockColor(val) => property!("border-block-color", val),
      BorderBlockStyle(val) => property!("border-block-style", val),
      BorderBlockWidth(val) => property!("border-block-width", val),
      BorderInlineColor(val) => property!("border-inline-color", val),
      BorderInlineStyle(val) => property!("border-inline-style", val),
      BorderInlineWidth(val) => property!("border-inline-width", val),
      Border(val) => property!("border", val),
      BorderTop(val) => property!("border-top", val),
      BorderBottom(val) => property!("border-bottom", val),
      BorderLeft(val) => property!("border-left", val),
      BorderRight(val) => property!("border-right", val),
      BorderBlock(val) => property!("border-block", val),
      BorderBlockStart(val) => property!("border-block-start", val),
      BorderBlockEnd(val) => property!("border-block-end", val),
      BorderInline(val) => property!("border-inline", val),
      BorderInlineStart(val) => property!("border-inline-start", val),
      BorderInlineEnd(val) => property!("border-inline-end", val),
      Custom(custom) => {
        dest.write_str(custom.name.as_ref())?;
        dest.write_str(": ")?;
        dest.write_str(custom.value.as_ref())?;
        dest.write_str(";")
      }
    }
  }
}
