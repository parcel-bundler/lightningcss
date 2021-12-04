use crate::values::length::*;
use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::color::CssColor;
use crate::properties::Property;
use crate::declaration::DeclarationList;
use crate::values::rect::Rect;
use crate::macros::*;
use super::border_image::*;
use super::border_radius::*;
use crate::targets::Browsers;
use crate::printer::Printer;

#[derive(Debug, Clone, PartialEq)]
pub enum BorderSideWidth {
  /// `thin`
  Thin,
  /// `medium`
  Medium,
  /// `thick`
  Thick,
  /// `<length>`
  Length(Length),
}

impl Default for BorderSideWidth {
  fn default() -> BorderSideWidth {
    BorderSideWidth::Medium
  }
}

impl Parse for BorderSideWidth {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(length) = input.try_parse(|i| Length::parse(i)) {
      return Ok(BorderSideWidth::Length(length));
    }
    let ident = input.expect_ident_cloned()?;
    match_ignore_ascii_case! { &ident,
      "thin" => Ok(BorderSideWidth::Thin),
      "medium" => Ok(BorderSideWidth::Medium),
      "thick" => Ok(BorderSideWidth::Thick),
      _ => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }
}

impl ToCss for BorderSideWidth {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    use BorderSideWidth::*;
    match self {
      Thin => dest.write_str("thin"),
      Medium => dest.write_str("medium"),
      Thick => dest.write_str("thick"),
      Length(length) => length.to_css(dest)
    }
  }
}

enum_property!(BorderStyle,
  Hidden,
  None,
  Inset,
  Groove,
  Outset,
  Ridge,
  Dotted,
  Dashed,
  Solid,
  Double
);

impl Default for BorderStyle {
  fn default() -> BorderStyle {
    BorderStyle::None
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericBorder<S> {
  pub width: BorderSideWidth,
  pub style: S,
  pub color: CssColor
}

impl<S: Default> Default for GenericBorder<S> {
  fn default() -> GenericBorder<S> {
    GenericBorder {
      width: BorderSideWidth::Medium,
      style: S::default(),
      color: CssColor::current_color()
    }
  }
}

impl<S: Parse + Default> Parse for GenericBorder<S> {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    // Order doesn't matter...
    let mut color = None;
    let mut style = None;
    let mut width = None;
    let mut any = false;
    loop {
        if width.is_none() {
            if let Ok(value) = input.try_parse(|i| BorderSideWidth::parse(i)) {
                width = Some(value);
                any = true;
            }
        }
        if style.is_none() {
            if let Ok(value) = input.try_parse(S::parse) {
                style = Some(value);
                any = true;
                continue
            }
        }
        if color.is_none() {
            if let Ok(value) = input.try_parse(|i| CssColor::parse(i)) {
                color = Some(value);
                any = true;
                continue
            }
        }
        break
    }
    if any {
      Ok(GenericBorder {
        width: width.unwrap_or(BorderSideWidth::Medium),
        style: style.unwrap_or_default(),
        color: color.unwrap_or_else(|| CssColor::current_color())
      })
    } else {
      Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }
}

impl<S: ToCss + Default + PartialEq> ToCss for GenericBorder<S> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    // Assume the default is 'none'
    if self.style == S::default() {
      if dest.minify {
        dest.write_char('0')?;
      } else {
        self.style.to_css(dest)?;
      }
      return Ok(())
    }

    if self.width != BorderSideWidth::default() {
      self.width.to_css(dest)?;
      dest.write_str(" ")?;
    }
    if self.style != S::default() {
      self.style.to_css(dest)?;
      dest.write_str(" ")?;
    }
    if self.color != CssColor::current_color() {
      self.color.to_css(dest)?;
    }
    Ok(())
  }
}

pub type Border = GenericBorder<BorderStyle>;

#[derive(Default, Debug, PartialEq)]
pub struct BorderShorthand {
  pub width: Option<BorderSideWidth>,
  pub style: Option<BorderStyle>,
  pub color: Option<CssColor>
}

impl BorderShorthand {
  pub fn set_border(&mut self, border: &Border) {
    self.width = Some(border.width.clone());
    self.style = Some(border.style.clone());
    self.color = Some(border.color.clone());
  }

  pub fn is_valid(&self) -> bool {
    self.width.is_some() && self.style.is_some() && self.color.is_some()
  }

  pub fn reset(&mut self) {
    self.width = None;
    self.style = None;
    self.color = None;
  }

  pub fn to_border(&self) -> Border {
    Border {
      width: self.width.clone().unwrap(),
      style: self.style.clone().unwrap(),
      color: self.color.clone().unwrap()
    }
  }
}

#[derive(Debug, PartialEq)]
enum BorderCategory {
  Logical,
  Physical
}

impl Default for BorderCategory {
  fn default() -> BorderCategory {
    BorderCategory::Physical
  }
}

#[derive(Default, Debug)]
pub(crate) struct BorderHandler {
  border_top: BorderShorthand,
  border_bottom: BorderShorthand,
  border_left: BorderShorthand,
  border_right: BorderShorthand,
  border_block_start: BorderShorthand,
  border_block_end: BorderShorthand,
  border_inline_start: BorderShorthand,
  border_inline_end: BorderShorthand,
  category: BorderCategory,
  border_image_handler: BorderImageHandler,
  border_radius_handler: BorderRadiusHandler
}

impl BorderHandler {
  pub fn new(targets: Option<Browsers>) -> BorderHandler {
    BorderHandler {
      border_image_handler: BorderImageHandler::new(targets),
      border_radius_handler: BorderRadiusHandler::new(targets),
      ..BorderHandler::default()
    }
  }
}

impl PropertyHandler for BorderHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;
    use BorderCategory::*;

    macro_rules! property {
      ($key: ident, $prop: ident, $val: ident, $category: ident) => {{
        if $category != self.category {
          self.flush(dest);
        }
        self.$key.$prop = Some($val.clone());
        self.category = $category;
      }};
    }

    macro_rules! set_border {
      ($key: ident, $val: ident, $category: ident) => {{
        if $category != self.category {
          self.flush(dest);
        }
        self.$key.set_border($val);
        self.category = $category;
      }};
    }

    match &property {
      BorderTopColor(val) => property!(border_top, color, val, Physical),
      BorderBottomColor(val) => property!(border_bottom, color, val, Physical),
      BorderLeftColor(val) => property!(border_left, color, val, Physical),
      BorderRightColor(val) => property!(border_right, color, val, Physical),
      BorderBlockStartColor(val) => property!(border_block_start, color, val, Logical),
      BorderBlockEndColor(val) => property!(border_block_end, color, val, Logical),
      BorderBlockColor(val) => {
        property!(border_block_start, color, val, Logical);
        property!(border_block_end, color, val, Logical);
      }
      BorderInlineStartColor(val) => property!(border_inline_start, color, val, Logical),
      BorderInlineEndColor(val) => property!(border_inline_end, color, val, Logical),
      BorderInlineColor(val) => {
        property!(border_inline_start, color, val, Logical);
        property!(border_inline_end, color, val, Logical);
      }
      BorderTopWidth(val) => property!(border_top, width, val, Physical),
      BorderBottomWidth(val) => property!(border_bottom, width, val, Physical),
      BorderLeftWidth(val) => property!(border_left, width, val, Physical),
      BorderRightWidth(val) => property!(border_right, width, val, Physical),
      BorderBlockStartWidth(val) => property!(border_block_start, width, val, Logical),
      BorderBlockEndWidth(val) => property!(border_block_end, width, val, Logical),
      BorderBlockWidth(val) => {
        property!(border_block_start, width, val, Logical);
        property!(border_block_end, width, val, Logical);
      }
      BorderInlineStartWidth(val) => property!(border_inline_start, width, val, Logical),
      BorderInlineEndWidth(val) => property!(border_inline_end, width, val, Logical),
      BorderInlineWidth(val) => {
        property!(border_inline_start, width, val, Logical);
        property!(border_inline_end, width, val, Logical);
      }
      BorderTopStyle(val) => property!(border_top, style, val, Physical),
      BorderBottomStyle(val) => property!(border_bottom, style, val, Physical),
      BorderLeftStyle(val) => property!(border_left, style, val, Physical),
      BorderRightStyle(val) => property!(border_right, style, val, Physical),
      BorderBlockStartStyle(val) => property!(border_block_start, style, val, Logical),
      BorderBlockEndStyle(val) => property!(border_block_end, style, val, Logical),
      BorderBlockStyle(val) => {
        property!(border_block_start, style, val, Logical);
        property!(border_block_end, style, val, Logical);
      }
      BorderInlineStartStyle(val) => property!(border_inline_start, style, val, Logical),
      BorderInlineEndStyle(val) => property!(border_inline_end, style, val, Logical),
      BorderInlineStyle(val) => {
        property!(border_inline_start, style, val, Logical);
        property!(border_inline_end, style, val, Logical);
      }
      BorderTop(val) => set_border!(border_top, val, Physical),
      BorderBottom(val) => set_border!(border_bottom, val, Physical),
      BorderLeft(val) => set_border!(border_left, val, Physical),
      BorderRight(val) => set_border!(border_right, val, Physical),
      BorderBlockStart(val) => set_border!(border_block_start, val, Logical),
      BorderBlockEnd(val) => set_border!(border_block_end, val, Logical),
      BorderInlineStart(val) => set_border!(border_inline_start, val, Logical),
      BorderInlineEnd(val) => set_border!(border_inline_end, val, Logical),
      BorderBlock(val) => {
        set_border!(border_block_start, val, Logical);
        set_border!(border_block_end, val, Logical);
      },
      BorderInline(val) => {
        set_border!(border_inline_start, val, Logical);
        set_border!(border_inline_end, val, Logical);
      },
      BorderWidth(val) => {
        self.border_top.width = Some(val.0.clone());
        self.border_right.width = Some(val.1.clone());
        self.border_bottom.width = Some(val.2.clone());
        self.border_left.width = Some(val.3.clone());
        self.border_block_start.width = None;
        self.border_block_end.width = None;
        self.border_inline_start.width = None;
        self.border_inline_end.width = None;
      }
      BorderStyle(val) => {
        self.border_top.style = Some(val.0.clone());
        self.border_right.style = Some(val.1.clone());
        self.border_bottom.style = Some(val.2.clone());
        self.border_left.style = Some(val.3.clone());
        self.border_block_start.style = None;
        self.border_block_end.style = None;
        self.border_inline_start.style = None;
        self.border_inline_end.style = None;
      }
      BorderColor(val) => {
        self.border_top.color = Some(val.0.clone());
        self.border_right.color = Some(val.1.clone());
        self.border_bottom.color = Some(val.2.clone());
        self.border_left.color = Some(val.3.clone());
        self.border_block_start.color = None;
        self.border_block_end.color = None;
        self.border_inline_start.color = None;
        self.border_inline_end.color = None;
      }
      Border(val) => {
        // dest.clear();
        self.border_top.set_border(val);
        self.border_bottom.set_border(val);
        self.border_left.set_border(val);
        self.border_right.set_border(val);
        self.border_block_start.reset();
        self.border_block_end.reset();
        self.border_inline_start.reset();
        self.border_inline_end.reset();

        // Setting the `border` property resets `border-image`.
        self.border_image_handler.reset();
      }
      _ => {
        return self.border_image_handler.handle_property(property, dest) || self.border_radius_handler.handle_property(property, dest)
      }
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    self.border_image_handler.finalize(dest);
    self.border_radius_handler.finalize(dest);
    self.flush(dest);
  }
}

impl BorderHandler {
  fn flush(&mut self, dest: &mut DeclarationList) {
    use Property::*;

    macro_rules! flush_category {
      (
        $block_start_prop: ident,
        $block_start_width: ident,
        $block_start_style: ident,
        $block_start_color: ident,
        $block_start: expr,
        $block_end_prop: ident,
        $block_end_width: ident,
        $block_end_style: ident,
        $block_end_color: ident,
        $block_end: expr, 
        $inline_start_prop: ident, 
        $inline_start_width: ident,
        $inline_start_style: ident,
        $inline_start_color: ident,
        $inline_start: expr, 
        $inline_end_prop: ident, 
        $inline_end_width: ident,
        $inline_end_style: ident,
        $inline_end_color: ident,
        $inline_end: expr,
        $is_logical: expr
      ) => {
        if $block_start.is_valid() && $block_end.is_valid() && $inline_start.is_valid() && $inline_end.is_valid() {
          let top_eq_bottom = $block_start == $block_end;
          let left_eq_right = $inline_start == $inline_end;
          let top_eq_left = $block_start == $inline_start;
          let top_eq_right = $block_start == $inline_end;
          let bottom_eq_left = $block_end == $inline_start;
          let bottom_eq_right = $block_end == $inline_end;

          macro_rules! side_diff {
            ($border: expr, $other: expr, $prop: ident, $width: ident, $style: ident, $color: ident) => {
              let eq_width = $border.width == $other.width;
              let eq_style = $border.style == $other.style;
              let eq_color = $border.color == $other.color;
      
              // If only one of the sub-properties is different, only emit that.
              // Otherwise, emit the full border value.
              if eq_width && eq_style {
                dest.push($color($other.color.clone().unwrap()))
              } else if eq_width && eq_color {
                dest.push($style($other.style.clone().unwrap()))
              } else if eq_style && eq_color {
                dest.push($width($other.width.clone().unwrap()))
              } else {
                dest.push($prop($other.to_border()))
              }
            };
          }      

          if top_eq_bottom && top_eq_left && top_eq_right {
            dest.push(Property::Border($block_start.to_border()));
          } else if top_eq_bottom && top_eq_left {
            dest.push(Property::Border($block_start.to_border()));
            side_diff!($block_start, $inline_end, $inline_end_prop, $inline_end_width, $inline_end_style, $inline_end_color);
          } else if top_eq_bottom && top_eq_right {
            dest.push(Property::Border($block_start.to_border()));
            side_diff!($block_start, $inline_start, $inline_start_prop, $inline_start_width, $inline_start_style, $inline_start_color);
          } else if left_eq_right && bottom_eq_left {
            dest.push(Property::Border($inline_start.to_border()));
            side_diff!($inline_start, $block_start, $block_start_prop, $block_start_width, $block_start_style, $block_start_color);
          } else if left_eq_right && top_eq_left {
            dest.push(Property::Border($inline_start.to_border()));
            side_diff!($inline_start, $block_end, $block_end_prop, $block_end_width, $block_end_style, $block_end_color);
          } else if top_eq_bottom {
            dest.push(Property::Border($block_start.to_border()));
            side_diff!($block_start, $inline_start, $inline_start_prop, $inline_start_width, $inline_start_style, $inline_start_color);
            side_diff!($block_start, $inline_end, $inline_end_prop, $inline_end_width, $inline_end_style, $inline_end_color);
          } else if left_eq_right {
            dest.push(Property::Border($inline_start.to_border()));
            side_diff!($inline_start, $block_start, $block_start_prop, $block_start_width, $block_start_style, $block_start_color);
            side_diff!($inline_start, $block_end, $block_end_prop, $block_end_width, $block_end_style, $block_end_color);
          } else if bottom_eq_right {
            dest.push(Property::Border($block_end.to_border()));
            side_diff!($block_end, $block_start, $block_start_prop, $block_start_width, $block_start_style, $block_start_color);
            side_diff!($block_end, $inline_start, $inline_start_prop, $inline_start_width, $inline_start_style, $inline_start_color);
          } else {
            dest.push(Property::$block_start_prop($block_start.to_border()));
            dest.push(Property::$block_end_prop($block_end.to_border()));
            dest.push(Property::$inline_start_prop($inline_start.to_border()));
            dest.push(Property::$inline_end_prop($inline_end.to_border()));
          }
        } else {
          macro_rules! shorthand {
            ($prop: expr, $key: ident) => {{
              let has_prop = $block_start.$key.is_some() && $block_end.$key.is_some() && $inline_start.$key.is_some() && $inline_end.$key.is_some();
              if has_prop {
                if !$is_logical || ($block_start.$key == $block_end.$key && $block_end.$key == $inline_start.$key && $inline_start.$key == $inline_end.$key) {
                  let rect = Rect::new(
                    std::mem::take(&mut $block_start.$key).unwrap(),
                    std::mem::take(&mut $inline_end.$key).unwrap(),
                    std::mem::take(&mut $block_end.$key).unwrap(),
                    std::mem::take(&mut $inline_start.$key).unwrap()
                  );
                  dest.push($prop(rect));
                }
              }
            }};
          }

          macro_rules! logical_shorthand {
            ($prop: expr, $key: ident, $start: expr, $end: expr) => {{
              let has_prop = $start.$key.is_some() && $start.$key == $end.$key;
              if has_prop {
                dest.push($prop(std::mem::take(&mut $start.$key).unwrap()));
                $end.$key = None;
              }
              has_prop
            }};
          }
    
          shorthand!(BorderStyle, style);
          shorthand!(BorderWidth, width);
          shorthand!(BorderColor, color);
    
          macro_rules! side {
            ($val: expr, $shorthand: ident, $width: ident, $style: ident, $color: ident) => {
              if $val.is_valid() {
                dest.push(Property::$shorthand($val.to_border()));
              } else {
                if let Some(style) = &$val.style {
                  dest.push($style(style.clone()));
                }
        
                if let Some(width) = &$val.width {
                  dest.push($width(width.clone()));
                }
        
                if let Some(color) = &$val.color {
                  dest.push($color(color.clone()));
                }
              }
            };
          }

          if $is_logical && $block_start == $block_end && $block_start.is_valid() {
            dest.push(BorderBlock($block_start.to_border()))
          } else {
            if $is_logical && !$block_start.is_valid() && !$block_end.is_valid() {
              logical_shorthand!(BorderBlockStyle, style, $block_start, $block_end);
              logical_shorthand!(BorderBlockWidth, width, $block_start, $block_end);
              logical_shorthand!(BorderBlockColor, color, $block_start, $block_end);
            }

            side!($block_start, $block_start_prop, $block_start_width, $block_start_style, $block_start_color);
            side!($block_end, $block_end_prop, $block_end_width, $block_end_style, $block_end_color);
          }

          if $is_logical && $inline_start == $inline_end && $inline_start.is_valid() {
            dest.push(BorderBlock($block_start.to_border()))
          } else {
            if $is_logical && !$inline_start.is_valid() && !$inline_end.is_valid() {
              logical_shorthand!(BorderInlineStyle, style, $inline_start, $inline_end);
              logical_shorthand!(BorderInlineWidth, width, $inline_start, $inline_end);
              logical_shorthand!(BorderInlineColor, color, $inline_start, $inline_end);
            }

            side!($inline_start, $inline_start_prop, $inline_start_width, $inline_start_style, $inline_start_color);
            side!($inline_end, $inline_end_prop, $inline_end_width, $inline_end_style, $inline_end_color);
          }
        }
      };
    }

    flush_category!(
      BorderTop,
      BorderTopWidth,
      BorderTopStyle,
      BorderTopColor,
      self.border_top,
      BorderBottom,
      BorderBottomWidth,
      BorderBottomStyle,
      BorderBottomColor,
      self.border_bottom,
      BorderLeft,
      BorderLeftWidth,
      BorderLeftStyle,
      BorderLeftColor,
      self.border_left,
      BorderRight,
      BorderRightWidth,
      BorderRightStyle,
      BorderRightColor,
      self.border_right,
      false
    );

    flush_category!(
      BorderBlockStart,
      BorderBlockStartWidth,
      BorderBlockStartStyle,
      BorderBlockStartColor,
      self.border_block_start,
      BorderBlockEnd,
      BorderBlockEndWidth,
      BorderBlockEndStyle,
      BorderBlockEndColor,
      self.border_block_end,
      BorderInlineStart,
      BorderInlineStartWidth,
      BorderInlineStartStyle,
      BorderInlineStartColor,
      self.border_inline_start,
      BorderInlineEnd,
      BorderInlineEndWidth,
      BorderInlineEndStyle,
      BorderInlineEndColor,
      self.border_inline_end,
      true
    );

    self.border_top.reset();
    self.border_bottom.reset();
    self.border_left.reset();
    self.border_right.reset();
    self.border_block_start.reset();
    self.border_block_end.reset();
    self.border_inline_start.reset();
    self.border_inline_end.reset();
  }
}
