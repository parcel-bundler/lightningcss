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
pub mod prefixes;
pub mod prefix_handler;
pub mod display;
pub mod text;

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
use display::*;
use text::*;
use crate::values::{image::*, length::*, position::*, alpha::*, size::*, rect::*, color::*, time::Time, ident::CustomIdent, easing::EasingFunction};
use crate::traits::{Parse, ToCss};
use crate::printer::Printer;
use smallvec::{SmallVec, smallvec};
use bitflags::bitflags;
use std::fmt::Write;

bitflags! {
  pub struct VendorPrefix: u8 {
    const None   = 0b00000001;
    const WebKit = 0b00000010;
    const Moz    = 0b00000100;
    const Ms     = 0b00001000;
    const O      = 0b00010000;
  }
}

impl Default for VendorPrefix {
  fn default() -> VendorPrefix {
    VendorPrefix::None
  }
}

impl VendorPrefix {
  pub fn from_str(s: &str) -> VendorPrefix {
    match s {
      "webkit" => VendorPrefix::WebKit,
      "moz" => VendorPrefix::Moz,
      "ms" => VendorPrefix::Ms,
      "o" => VendorPrefix::O,
      _ => unreachable!()
    }
  }
}

impl ToCss for VendorPrefix {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match *self {
      VendorPrefix::WebKit => dest.write_str("-webkit-"),
      VendorPrefix::Moz => dest.write_str("-moz-"),
      VendorPrefix::Ms => dest.write_str("-ms-"),
      VendorPrefix::O => dest.write_str("-o-"),
      _ => Ok(())
    }
  }
}

macro_rules! define_properties {
  (
    $( $name: tt: $property: ident($type: ty $(, $vp: ident)?) $( / $prefix: tt )*, )+
  ) => {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Property {
      $(
        $property($type, $($vp)?),
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
                return Ok(Property::$property(c, $(<$vp>::None)?))
              }
            }
          )+
          $(
            $(
              concat!("-", $prefix, "-", $name) => {
                if let Ok(c) = <$type>::parse(input) {
                  return Ok(Property::$property(c, VendorPrefix::from_str($prefix)))
                }
              }
            )*
          )?
          _ => {}
        }

        input.reset(&state);
        return Ok(Property::Custom(CustomProperty::parse(name, input)?))
      }

      pub fn to_css<W>(&self, dest: &mut Printer<W>, important: bool) -> std::fmt::Result where W: std::fmt::Write {
        use Property::*;

        match self {
          $(
            $property(val, $($vp)?) => {
              // If there are multiple vendor prefixes set, this expands them.
              let mut first = true;
              macro_rules! start {
                () => {
                  if first {
                    first = false;
                  } else {
                    dest.write_char(';')?;
                    if !dest.minify {
                      dest.write_str("\n  ")?;
                    }
                  }
                };
              }

              macro_rules! write {
                () => {
                  dest.write_str($name)?;
                  dest.delim(':', false)?;
                  val.to_css(dest)?;
                  if important {
                    dest.whitespace()?;
                    dest.write_str("!important")?;
                  }
                };
                ($p: expr) => {
                  start!();
                  write!();
                };
                ($v: ident, $p: expr) => {
                  if $v.contains($p) {
                    start!();
                    $p.to_css(dest)?;
                    write!();
                  }
                };
              }
              
              $(
                write!($vp, VendorPrefix::WebKit);
                write!($vp, VendorPrefix::Moz);
                write!($vp, VendorPrefix::Ms);
                write!($vp, VendorPrefix::O);
              )?

              write!($($vp,)? VendorPrefix::None);
            }
          )+
          Custom(custom) => {
            dest.write_str(custom.name.as_ref())?;
            dest.delim(':', false)?;
            dest.write_str(custom.value.as_ref())?;
            if important {
              dest.whitespace()?;
              dest.write_str("!important")?;
            }
          }
        }
        Ok(())
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
  "background-clip": BackgroundClip(SmallVec<[BackgroundClip; 1]>, VendorPrefix) / "webkit" / "moz",
  "background-origin": BackgroundOrigin(SmallVec<[BackgroundBox; 1]>),
  "background": Background(SmallVec<[Background; 1]>),

  "box-shadow": BoxShadow(SmallVec<[BoxShadow; 1]>, VendorPrefix) / "webkit" / "moz",
  "opacity": Opacity(AlphaValue),
  "color": Color(CssColor),
  "display": Display(Display),
  "visibility": Visibility(Visibility),

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
  "box-sizing": BoxSizing(BoxSizing, VendorPrefix) / "webkit" / "moz",

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

  "border-top-left-radius": BorderTopLeftRadius(Size2D<LengthPercentage>, VendorPrefix) / "webkit" / "moz",
  "border-top-right-radius": BorderTopRightRadius(Size2D<LengthPercentage>, VendorPrefix) / "webkit" / "moz",
  "border-bottom-left-radius": BorderBottomLeftRadius(Size2D<LengthPercentage>, VendorPrefix) / "webkit" / "moz",
  "border-bottom-right-radius": BorderBottomRightRadius(Size2D<LengthPercentage>, VendorPrefix) / "webkit" / "moz",
  "border-start-start-radius": BorderStartStartRadius(Size2D<LengthPercentage>),
  "border-start-end-radius": BorderStartEndRadius(Size2D<LengthPercentage>),
  "border-end-start-radius": BorderEndStartRadius(Size2D<LengthPercentage>),
  "border-end-end-radius": BorderEndEndRadius(Size2D<LengthPercentage>),
  "border-radius": BorderRadius(BorderRadius, VendorPrefix) / "webkit" / "moz",

  "border-image-source": BorderImageSource(Image),
  "border-image-outset": BorderImageOutset(Rect<LengthOrNumber>),
  "border-image-repeat": BorderImageRepeat(BorderImageRepeat),
  "border-image-width": BorderImageWidth(Rect<BorderImageSideWidth>),
  "border-image-slice": BorderImageSlice(BorderImageSlice),
  "border-image": BorderImage(BorderImage, VendorPrefix) / "webkit" / "moz" / "o",

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
  "flex-direction": FlexDirection(FlexDirection, VendorPrefix) / "webkit" / "ms",
  "flex-wrap": FlexWrap(FlexWrap, VendorPrefix) / "webkit" / "ms",
  "flex-flow": FlexFlow(FlexFlow, VendorPrefix) / "webkit" / "ms",
  "flex-grow": FlexGrow(f32, VendorPrefix) / "webkit",
  "flex-shrink": FlexShrink(f32, VendorPrefix) / "webkit",
  "flex-basis": FlexBasis(LengthPercentageOrAuto, VendorPrefix) / "webkit",
  "flex": Flex(Flex, VendorPrefix) / "webkit" / "ms",
  "order": Order(f32, VendorPrefix) / "webkit",

  // Align properties: https://www.w3.org/TR/2020/WD-css-align-3-20200421
  "align-content": AlignContent(AlignContent, VendorPrefix) / "webkit",
  "justify-content": JustifyContent(JustifyContent, VendorPrefix) / "webkit",
  "place-content": PlaceContent(PlaceContent),
  "align-self": AlignSelf(AlignSelf, VendorPrefix) / "webkit",
  "justify-self": JustifySelf(JustifySelf),
  "place-self": PlaceSelf(PlaceSelf),
  "align-items": AlignItems(AlignItems, VendorPrefix) / "webkit",
  "justify-items": JustifyItems(JustifyItems),
  "place-items": PlaceItems(PlaceItems),
  "row-gap": RowGap(GapValue),
  "column-gap": ColumnGap(GapValue),
  "gap": Gap(Gap),

  // Old flex (2009): https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/
  "box-orient": BoxOrient(BoxOrient, VendorPrefix) / "webkit" / "moz",
  "box-direction": BoxDirection(BoxDirection, VendorPrefix) / "webkit" / "moz",
  "box-ordinal-group": BoxOrdinalGroup(f32, VendorPrefix) / "webkit" / "moz",
  "box-align": BoxAlign(BoxAlign, VendorPrefix) / "webkit" / "moz",
  "box-flex": BoxFlex(f32, VendorPrefix) / "webkit" / "moz",
  "box-flex-group": BoxFlexGroup(f32, VendorPrefix) / "webkit",
  "box-pack": BoxPack(BoxPack, VendorPrefix) / "webkit" / "moz",
  "box-lines": BoxLines(BoxLines, VendorPrefix) / "webkit" / "moz",

  // Old flex (2012): https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/
  "flex-pack": FlexPack(FlexPack, VendorPrefix) / "ms",
  "flex-order": FlexOrder(f32, VendorPrefix) / "ms",
  "flex-align": FlexAlign(BoxAlign, VendorPrefix) / "ms",
  "flex-item-align": FlexItemAlign(FlexItemAlign, VendorPrefix) / "ms",
  "flex-line-pack": FlexLinePack(FlexLinePack, VendorPrefix) / "ms",

  // Microsoft extensions
  "flex-positive": FlexPositive(f32, VendorPrefix) / "ms",
  "flex-negative": FlexNegative(f32, VendorPrefix) / "ms",
  "flex-preferred-size": FlexPreferredSize(LengthPercentageOrAuto, VendorPrefix) / "ms",

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

  "transition-property": TransitionProperty(SmallVec<[CustomIdent; 1]>, VendorPrefix) / "webkit" / "moz" / "ms",
  "transition-duration": TransitionDuration(SmallVec<[Time; 1]>, VendorPrefix) / "webkit" / "moz" / "ms",
  "transition-delay": TransitionDelay(SmallVec<[Time; 1]>, VendorPrefix) / "webkit" / "moz" / "ms",
  "transition-timing-function": TransitionTimingFunction(SmallVec<[EasingFunction; 1]>, VendorPrefix) / "webkit" / "moz" / "ms",
  "transition": Transition(SmallVec<[Transition; 1]>, VendorPrefix) / "webkit" / "moz" / "ms",

  "animation-name": AnimationName(SmallVec<[AnimationName; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-duration": AnimationDuration(SmallVec<[Time; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-timing-function": AnimationTimingFunction(SmallVec<[EasingFunction; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-iteration-count": AnimationIterationCount(SmallVec<[AnimationIterationCount; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-direction": AnimationDirection(SmallVec<[AnimationDirection; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-play-state": AnimationPlayState(SmallVec<[AnimationPlayState; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-delay": AnimationDelay(SmallVec<[Time; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation-fill-mode": AnimationFillMode(SmallVec<[AnimationFillMode; 1]>, VendorPrefix) / "webkit" / "moz" / "o",
  "animation": Animation(SmallVec<[Animation; 1]>, VendorPrefix) / "webkit" / "moz" / "o",

  // https://drafts.csswg.org/css-transforms-2/
  "transform": Transform(TransformList, VendorPrefix) / "webkit" / "moz" / "ms" / "o",
  "transform-origin": TransformOrigin(Position, VendorPrefix) / "webkit" / "moz" / "ms" / "o", // TODO: handle z offset syntax
  "transform-style": TransformStyle(TransformStyle, VendorPrefix) / "webkit" / "moz",
  "transform-box": TransformBox(TransformBox),
  "backface-visibility": BackfaceVisibility(BackfaceVisibility, VendorPrefix) / "webkit" / "moz",
  "perspective": Perspective(Perspective, VendorPrefix) / "webkit" / "moz",
  "perspective-origin": PerspectiveOrigin(Position, VendorPrefix) / "webkit" / "moz",
  "translate": Translate(Translate),
  "rotate": Rotate(Rotate),
  "scale": Scale(Scale),

  // https://www.w3.org/TR/2021/CRD-css-text-3-20210422
  "text-transform": TextTransform(TextTransform),
  "white-space": WhiteSpace(WhiteSpace),
  "tab-size": TabSize(LengthOrNumber, VendorPrefix) / "moz" / "o",
  "word-break": WordBreak(WordBreak),
  "line-break": LineBreak(LineBreak),
  "hyphens": Hyphens(Hyphens, VendorPrefix) / "webkit" / "moz" / "ms",
  "overflow-wrap": OverflowWrap(OverflowWrap),
  "word-wrap": WordWrap(OverflowWrap),
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
