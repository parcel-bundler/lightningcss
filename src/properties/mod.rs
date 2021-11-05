pub mod custom;
pub mod margin_padding;
pub mod background;
pub mod outline;
pub mod flex;
pub mod align;
pub mod font;
pub mod box_shadow;
pub mod border;
pub mod border_image;
pub mod border_radius;
pub mod transition;
pub mod animation;
pub mod transform;

use cssparser::*;
use custom::*;
use background::*;
use outline::*;
use flex::*;
use align::*;
use font::*;
use box_shadow::*;
use border::*;
use border_image::*;
use border_radius::*;
use transition::*;
use animation::*;
use transform::*;
use crate::values::{image::*, length::*, position::*, alpha::*, size::*, rect::*, color::*, time::Time, ident::CustomIdent, easing::EasingFunction};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use smallvec::{SmallVec, smallvec};

macro_rules! define_properties {
  (
    $( $name: tt: $property: ident($type: ty), )+
  ) => {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Property {
      $(
        $property($type),
      )+
      Custom(CustomProperty),
    }

    impl Property {
      pub fn parse<'i, 't>(name: CowRcStr<'i>, input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
        let state = input.state();
        match name.as_ref() {
          $(
            $name => {
              if let Ok(c) = <$type>::parse(input) {
                return Ok(Property::$property(c))
              }
            }
          )+
          _ => {}
        }

        input.reset(&state);
        return Ok(Property::Custom(CustomProperty::parse(name, input)?))
      }

      pub fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
        use Property::*;

        match self {
          $(
            $property(val) => {
              dest.write_str($name)?;
              dest.delim(':', false)?;
              val.to_css(dest)
            }
          )+
          Custom(custom) => {
            dest.write_str(custom.name.as_ref())?;
            dest.delim(':', false)?;
            dest.write_str(custom.value.as_ref())
          }
        }
      }
    }
  };
}

define_properties! {
  "background-color": BackgroundColor(CssColor),
  "background-image": BackgroundImage(SmallVec<[Image; 1]>),
  "background-position-x": BackgroundPositionX(SmallVec<[HorizontalPosition; 1]>),
  "background-position-y": BackgroundPositionY(SmallVec<[VerticalPosition; 1]>),
  "background-position": BackgroundPosition(SmallVec<[Position; 1]>),
  "background-size": BackgroundSize(SmallVec<[BackgroundSize; 1]>),
  "background-repeat": BackgroundRepeat(SmallVec<[BackgroundRepeat; 1]>),
  "background-attachment": BackgroundAttachment(SmallVec<[BackgroundAttachment; 1]>),
  "background-clip": BackgroundClip(SmallVec<[BackgroundBox; 1]>),
  "background-origin": BackgroundOrigin(SmallVec<[BackgroundBox; 1]>),
  "background": Background(SmallVec<[Background; 1]>),

  "box-shadow": BoxShadow(SmallVec<[BoxShadow; 1]>),
  "opacity": Opacity(AlphaValue),

  "color": Color(CssColor),

  "width": Width(Size),
  "height": Height(Size),
  "min-width": MinWidth(MinMaxSize),
  "min-height": MinHeight(MinMaxSize),
  "max-width": MaxWidth(MinMaxSize),
  "max-height": MaxHeight(MinMaxSize),
  "block-size": BlockSize(Size),
  "inline-size": InlineSize(Size),
  "min-block-size": MinBlockSize(MinMaxSize),
  "min-inline-size": MinInlineSize(MinMaxSize),
  "max-block-size": MaxBlockSize(MinMaxSize),
  "max-inline-size": MaxInlineSize(MinMaxSize),

  "top": Top(LengthPercentageOrAuto),
  "bottom": Bottom(LengthPercentageOrAuto),
  "left": Left(LengthPercentageOrAuto),
  "right": Right(LengthPercentageOrAuto),
  "inset-block-start": InsetBlockStart(LengthPercentageOrAuto),
  "inset-block-end": InsetBlockEnd(LengthPercentageOrAuto),
  "inset-inline-start": InsetInlineStart(LengthPercentageOrAuto),
  "inset-inline-end": InsetInlineEnd(LengthPercentageOrAuto),

  "border-top-color": BorderTopColor(CssColor),
  "border-bottom-color": BorderBottomColor(CssColor),
  "border-left-color": BorderLeftColor(CssColor),
  "border-right-color": BorderRightColor(CssColor),
  "border-block-start-color": BorderBlockStartColor(CssColor),
  "border-block-end-color": BorderBlockEndColor(CssColor),
  "border-inline-start-color": BorderInlineStartColor(CssColor),
  "border-inline-end-color": BorderInlineEndColor(CssColor),

  "border-top-style": BorderTopStyle(BorderStyle),
  "border-bottom-style": BorderBottomStyle(BorderStyle),
  "border-left-style": BorderLeftStyle(BorderStyle),
  "border-right-style": BorderRightStyle(BorderStyle),
  "border-block-start-style": BorderBlockStartStyle(BorderStyle),
  "border-block-end-style": BorderBlockEndStyle(BorderStyle),
  "border-inline-start-style": BorderInlineStartStyle(BorderStyle),
  "border-inline-end-style": BorderInlineEndStyle(BorderStyle),

  "border-top-width": BorderTopWidth(BorderSideWidth),
  "border-bottom-width": BorderBottomWidth(BorderSideWidth),
  "border-left-width": BorderLeftWidth(BorderSideWidth),
  "border-right-width": BorderRightWidth(BorderSideWidth),
  "border-block-start-width": BorderBlockStartWidth(BorderSideWidth),
  "border-block-end-width": BorderBlockEndWidth(BorderSideWidth),
  "border-inline-start-width": BorderInlineStartWidth(BorderSideWidth),
  "border-inline-end-width": BorderInlineEndWidth(BorderSideWidth),

  "border-top-left-radius": BorderTopLeftRadius(Size2D<LengthPercentage>),
  "border-top-right-radius": BorderTopRightRadius(Size2D<LengthPercentage>),
  "border-bottom-left-radius": BorderBottomLeftRadius(Size2D<LengthPercentage>),
  "border-bottom-right-radius": BorderBottomRightRadius(Size2D<LengthPercentage>),
  "border-start-start-radius": BorderStartStartRadius(Size2D<LengthPercentage>),
  "border-start-end-radius": BorderStartEndRadius(Size2D<LengthPercentage>),
  "border-end-start-radius": BorderEndStartRadius(Size2D<LengthPercentage>),
  "border-end-end-radius": BorderEndEndRadius(Size2D<LengthPercentage>),
  "border-radius": BorderRadius(BorderRadius),

  "border-image-source": BorderImageSource(Image),
  "border-image-outset": BorderImageOutset(Rect<LengthOrNumber>),
  "border-image-repeat": BorderImageRepeat(BorderImageRepeat),
  "border-image-width": BorderImageWidth(Rect<BorderImageSideWidth>),
  "border-image-slice": BorderImageSlice(BorderImageSlice),
  "border-image": BorderImage(BorderImage),

  "border-color": BorderColor(Rect<CssColor>),
  "border-style": BorderStyle(Rect<BorderStyle>),
  "border-width": BorderWidth(Rect<BorderSideWidth>),

  "border-block-color": BorderBlockColor(CssColor),
  "border-block-style": BorderBlockStyle(BorderStyle),
  "border-block-width": BorderBlockWidth(BorderSideWidth),

  "border-inline-color": BorderInlineColor(CssColor),
  "border-inline-style": BorderInlineStyle(BorderStyle),
  "border-inline-width": BorderInlineWidth(BorderSideWidth),

  "border": Border(Border),
  "border-top": BorderTop(Border),
  "border-bottom": BorderBottom(Border),
  "border-left": BorderLeft(Border),
  "border-right": BorderRight(Border),
  "border-block": BorderBlock(Border),
  "border-block-start": BorderBlockStart(Border),
  "border-block-end": BorderBlockEnd(Border),
  "border-inline": BorderInline(Border),
  "border-inline-start": BorderInlineStart(Border),
  "border-inline-end": BorderInlineEnd(Border),

  "outline": Outline(Outline),
  "outline-color": OutlineColor(CssColor),
  "outline-style": OutlineStyle(OutlineStyle),
  "outline-width": OutlineWidth(BorderSideWidth),

  // Flex properties: https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119
  "flex-direction": FlexDirection(FlexDirection),
  "flex-wrap": FlexWrap(FlexWrap),
  "flex-flow": FlexFlow(FlexFlow),
  "flex-grow": FlexGrow(f32),
  "flex-shrink": FlexShrink(f32),
  "flex-basis": FlexBasis(LengthPercentageOrAuto),
  "flex": Flex(Flex),

  // Align properties: https://www.w3.org/TR/2020/WD-css-align-3-20200421
  "align-content": AlignContent(AlignContent),
  "justify-content": JustifyContent(JustifyContent),
  "place-content": PlaceContent(PlaceContent),
  "align-self": AlignSelf(AlignSelf),
  "justify-self": JustifySelf(JustifySelf),
  "place-self": PlaceSelf(PlaceSelf),
  "align-items": AlignItems(AlignItems),
  "justify-items": JustifyItems(JustifyItems),
  "place-items": PlaceItems(PlaceItems),
  "row-gap": RowGap(GapValue),
  "column-gap": ColumnGap(GapValue),
  "gap": Gap(Gap),

  "margin-top": MarginTop(LengthPercentageOrAuto),
  "margin-bottom": MarginBottom(LengthPercentageOrAuto),
  "margin-left": MarginLeft(LengthPercentageOrAuto),
  "margin-right": MarginRight(LengthPercentageOrAuto),
  "margin-block-start": MarginBlockStart(LengthPercentageOrAuto),
  "margin-block-end": MarginBlockEnd(LengthPercentageOrAuto),
  "margin-inline-start": MarginInlineStart(LengthPercentageOrAuto),
  "margin-inline-end": MarginInlineEnd(LengthPercentageOrAuto),
  "margin-block": MarginBlock(Size2D<LengthPercentageOrAuto>),
  "margin-inline": MarginInline(Size2D<LengthPercentageOrAuto>),
  "margin": Margin(Rect<LengthPercentageOrAuto>),

  "padding-top": PaddingTop(LengthPercentageOrAuto),
  "padding-bottom": PaddingBottom(LengthPercentageOrAuto),
  "padding-left": PaddingLeft(LengthPercentageOrAuto),
  "padding-right": PaddingRight(LengthPercentageOrAuto),
  "padding-block-start": PaddingBlockStart(LengthPercentageOrAuto),
  "padding-block-end": PaddingBlockEnd(LengthPercentageOrAuto),
  "padding-inline-start": PaddingInlineStart(LengthPercentageOrAuto),
  "padding-inline-end": PaddingInlineEnd(LengthPercentageOrAuto),
  "padding-block": PaddingBlock(Size2D<LengthPercentageOrAuto>),
  "padding-inline": PaddingInline(Size2D<LengthPercentageOrAuto>),
  "padding": Padding(Rect<LengthPercentageOrAuto>),

  "scroll-margin-top": ScrollMarginTop(LengthPercentageOrAuto),
  "scroll-margin-bottom": ScrollMarginBottom(LengthPercentageOrAuto),
  "scroll-margin-left": ScrollMarginLeft(LengthPercentageOrAuto),
  "scroll-margin-right": ScrollMarginRight(LengthPercentageOrAuto),
  "scroll-margin-block-start": ScrollMarginBlockStart(LengthPercentageOrAuto),
  "scroll-margin-block-end": ScrollMarginBlockEnd(LengthPercentageOrAuto),
  "scroll-margin-inline-start": ScrollMarginInlineStart(LengthPercentageOrAuto),
  "scroll-margin-inline-end": ScrollMarginInlineEnd(LengthPercentageOrAuto),
  "scroll-margin-block": ScrollMarginBlock(Size2D<LengthPercentageOrAuto>),
  "scroll-margin-inline": ScrollMarginInline(Size2D<LengthPercentageOrAuto>),
  "scroll-margin": ScrollMargin(Rect<LengthPercentageOrAuto>),

  "scroll-padding-top": ScrollPaddingTop(LengthPercentageOrAuto),
  "scroll-padding-bottom": ScrollPaddingBottom(LengthPercentageOrAuto),
  "scroll-padding-left": ScrollPaddingLeft(LengthPercentageOrAuto),
  "scroll-padding-right": ScrollPaddingRight(LengthPercentageOrAuto),
  "scroll-padding-block-start": ScrollPaddingBlockStart(LengthPercentageOrAuto),
  "scroll-padding-block-end": ScrollPaddingBlockEnd(LengthPercentageOrAuto),
  "scroll-padding-inline-start": ScrollPaddingInlineStart(LengthPercentageOrAuto),
  "scroll-padding-inline-end": ScrollPaddingInlineEnd(LengthPercentageOrAuto),
  "scroll-padding-block": ScrollPaddingBlock(Size2D<LengthPercentageOrAuto>),
  "scroll-padding-inline": ScrollPaddingInline(Size2D<LengthPercentageOrAuto>),
  "scroll-padding": ScrollPadding(Rect<LengthPercentageOrAuto>),

  // shorthands: columns, list-style
  // grid, inset

  "font-weight": FontWeight(FontWeight),
  "font-size": FontSize(FontSize),
  "font-stretch": FontStretch(FontStretch),
  "font-family": FontFamily(Vec<FontFamily>),
  "font-style": FontStyle(FontStyle),
  "font-variant-caps": FontVariantCaps(FontVariantCaps),
  "line-height": LineHeight(LineHeight),
  "font": Font(Font),

  "transition-property": TransitionProperty(SmallVec<[CustomIdent; 1]>),
  "transition-duration": TransitionDuration(SmallVec<[Time; 1]>),
  "transition-delay": TransitionDelay(SmallVec<[Time; 1]>),
  "transition-timing-function": TransitionTimingFunction(SmallVec<[EasingFunction; 1]>),
  "transition": Transition(SmallVec<[Transition; 1]>),

  "animation-name": AnimationName(SmallVec<[AnimationName; 1]>),
  "animation-duration": AnimationDuration(SmallVec<[Time; 1]>),
  "animation-timing-function": AnimationTimingFunction(SmallVec<[EasingFunction; 1]>),
  "animation-iteration-count": AnimationIterationCount(SmallVec<[AnimationIterationCount; 1]>),
  "animation-direction": AnimationDirection(SmallVec<[AnimationDirection; 1]>),
  "animation-play-state": AnimationPlayState(SmallVec<[AnimationPlayState; 1]>),
  "animation-delay": AnimationDelay(SmallVec<[Time; 1]>),
  "animation-fill-mode": AnimationFillMode(SmallVec<[AnimationFillMode; 1]>),
  "animation": Animation(SmallVec<[Animation; 1]>),

  "transform": Transform(TransformList),
}

impl<T: smallvec::Array<Item = V>, V: Parse> Parse for SmallVec<T> {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    // Copied from cssparser `parse_comma_separated` but using SmallVec instead of Vec.
    let mut values = smallvec![];
    loop {
      input.skip_whitespace(); // Unnecessary for correctness, but may help try() in parse_one rewind less.
      match input.parse_until_before(Delimiter::Comma, &mut V::parse) {
        Ok(v) => values.push(v),
        Err(_) => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
      }
      match input.next() {
        Err(_) => return Ok(values),
        Ok(&Token::Comma) => continue,
        Ok(_) => unreachable!(),
      }
    }
  }
}

impl<T: smallvec::Array<Item = V>, V: ToCss> ToCss for SmallVec<T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let len = self.len();
    for (idx, val) in self.iter().enumerate() {
      val.to_css(dest)?;
      if idx < len - 1 {
        dest.delim(',', false)?;
      }
    }
    Ok(())
  }
}

impl<T: Parse> Parse for Vec<T> {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    input.parse_comma_separated(|input| T::parse(input))
  }
}

impl <T: ToCss> ToCss for Vec<T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let len = self.len();
    for (idx, val) in self.iter().enumerate() {
      val.to_css(dest)?;
      if idx < len - 1 {
        dest.delim(',', false)?;
      }
    }
    Ok(())
  }
}
