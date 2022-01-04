#![allow(non_upper_case_globals)]
use crate::values::length::*;
use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::color::CssColor;
use crate::properties::{Property, PropertyId};
use crate::declaration::DeclarationList;
use crate::logical::{LogicalProperties, LogicalProperty};
use crate::values::rect::Rect;
use crate::macros::*;
use super::border_image::*;
use super::border_radius::*;
use crate::targets::Browsers;
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};
use crate::compat::Feature;
use bitflags::bitflags;
use crate::properties::custom::UnparsedProperty;

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
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(length) = input.try_parse(|i| Length::parse(i)) {
      return Ok(BorderSideWidth::Length(length));
    }
    let location = input.current_source_location();
    let ident = input.expect_ident_cloned()?;
    match_ignore_ascii_case! { &ident,
      "thin" => Ok(BorderSideWidth::Thin),
      "medium" => Ok(BorderSideWidth::Medium),
      "thick" => Ok(BorderSideWidth::Thick),
      _ => return Err(location.new_unexpected_token_error(Token::Ident(ident)))
    }
  }
}

impl ToCss for BorderSideWidth {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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
      Err(input.new_custom_error(ParserError::InvalidDeclaration))
    }
  }
}

impl<S: ToCss + Default + PartialEq> ToCss for GenericBorder<S> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

bitflags! {
  /// Tracks which physical properties have already been emitted.
  struct PhysicalProperties: u16 {
    const BorderTop = 1 << 0;
    const BorderTopColor = 1 << 1;
    const BorderTopStyle = 1 << 2;
    const BorderTopWidth = 1 << 3;
    const BorderBottom = 1 << 4;
    const BorderBottomColor = 1 << 5;
    const BorderBottomStyle = 1 << 6;
    const BorderBottomWidth = 1 << 7;
    const BorderLeft = 1 << 8;
    const BorderLeftColor = 1 << 9;
    const BorderLeftStyle = 1 << 10;
    const BorderLeftWidth = 1 << 11;
    const BorderRight = 1 << 12;
    const BorderRightColor = 1 << 13;
    const BorderRightStyle = 1 << 14;
    const BorderRightWidth = 1 << 15;
  }
}

#[derive(Default, Debug)]
struct PhysicalToLogical {
  border_left: Option<usize>,
  border_left_color: Option<usize>,
  border_left_style: Option<usize>,
  border_left_width: Option<usize>,
  border_right: Option<usize>,
  border_right_color: Option<usize>,
  border_right_style: Option<usize>,
  border_right_width: Option<usize>,
}

macro_rules! get_physical {
  ($physical_to_logical: expr, $key: ident, $dest: ident) => {
    if let Some(index) = $physical_to_logical.$key {
      $dest.get_mut(index)
    } else {
      None
    }
  };
}

#[derive(Debug)]
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
  border_radius_handler: BorderRadiusHandler,
  has_any: bool,
  physical_properties: PhysicalProperties,
  physical_to_logical: PhysicalToLogical
}

impl<'a> BorderHandler {
  pub fn new(targets: Option<Browsers>) -> BorderHandler {
    BorderHandler {
      border_top: BorderShorthand::default(),
      border_bottom: BorderShorthand::default(),
      border_left: BorderShorthand::default(),
      border_right: BorderShorthand::default(),
      border_block_start: BorderShorthand::default(),
      border_block_end: BorderShorthand::default(),
      border_inline_start: BorderShorthand::default(),
      border_inline_end: BorderShorthand::default(),
      category: BorderCategory::default(),
      border_image_handler: BorderImageHandler::new(targets),
      border_radius_handler: BorderRadiusHandler::new(targets),
      has_any: false,
      physical_properties: PhysicalProperties::empty(),
      physical_to_logical: PhysicalToLogical::default()
    }
  }
}

impl PropertyHandler for BorderHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList, logical: &mut LogicalProperties) -> bool {
    use Property::*;

    macro_rules! property {
      ($key: ident, $prop: ident, $val: ident, $category: ident) => {{
        if BorderCategory::$category != self.category {
          self.flush(dest, logical);
        }
        self.$key.$prop = Some($val.clone());
        self.category = BorderCategory::$category;
        self.has_any = true;
      }};
    }

    macro_rules! set_border {
      ($key: ident, $val: ident, $category: ident) => {{
        if BorderCategory::$category != self.category {
          self.flush(dest, logical);
        }
        self.$key.set_border($val);
        self.category = BorderCategory::$category;
        self.has_any = true;
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
        self.has_any = true;
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
        self.has_any = true;
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
        self.has_any = true;
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
        self.has_any = true;
      }
      Unparsed(val) if is_border_property(&val.property_id) => {
        self.flush(dest, logical);
        self.flush_unparsed(&val, dest, logical);
      }
      _ => {
        return self.border_image_handler.handle_property(property, dest, logical) || self.border_radius_handler.handle_property(property, dest, logical)
      }
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList, logical: &mut LogicalProperties) {
    self.border_image_handler.finalize(dest, logical);
    self.border_radius_handler.finalize(dest, logical);
    self.flush(dest, logical);
    self.physical_properties = PhysicalProperties::empty();
  }
}

impl BorderHandler {
  fn flush(&mut self, dest: &mut DeclarationList, logical_properties: &mut LogicalProperties) {
    if !self.has_any {
      return
    }

    self.has_any = false;

    use Property::*;

    let logical_supported = logical_properties.is_supported(Feature::LogicalBorders);
    macro_rules! logical_prop {
      ($ltr: ident, $ltr_key: ident, $rtl: ident, $rtl_key: ident, $val: expr) => {{
        logical_properties.used = true;
        if let Some(Property::Logical(property)) = get_physical!(self.physical_to_logical, $ltr_key, dest) {
          property.ltr = Some(Box::new(Property::$ltr($val.clone())));
        } else {
          self.physical_to_logical.$ltr_key = Some(dest.len());
          dest.push(Property::Logical(LogicalProperty {
            property_id: PropertyId::$ltr,
            ltr: Some(Box::new(Property::$ltr($val.clone()))),
            rtl: None
          }));
        }

        if let Some(Property::Logical(property)) = get_physical!(self.physical_to_logical, $rtl_key, dest) {
          property.rtl = Some(Box::new(Property::$rtl($val.clone())));
        } else {
          self.physical_to_logical.$rtl_key = Some(dest.len());
          dest.push(Property::Logical(LogicalProperty {
            property_id: PropertyId::$rtl,
            ltr: None,
            rtl: Some(Box::new(Property::$rtl($val.clone())))
          }));
        }
      }};
    }

    macro_rules! prop {
      (BorderInlineStart => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineStart($val));
        } else {
          logical_prop!(BorderLeft, border_left, BorderRight, border_right, $val);
        }
      };
      (BorderInlineStartWidth => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineStartWidth($val));
        } else {
          logical_prop!(BorderLeftWidth, border_left_width, BorderRightWidth, border_right_width, $val);
        }
      };
      (BorderInlineStartColor => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineStartColor($val));
        } else {
          logical_prop!(BorderLeftColor, border_left_color, BorderRightColor, border_right_color, $val);
        }
      };
      (BorderInlineStartStyle => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineStartStyle($val));
        } else {
          logical_prop!(BorderLeftStyle, border_left_style, BorderRightStyle, border_right_style, $val);
        }
      };
      (BorderInlineEnd => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineEnd($val));
        } else {
          logical_prop!(BorderRight, border_right, BorderLeft, border_left, $val);
        }
      };
      (BorderInlineEndWidth => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineEndWidth($val));
        } else {
          logical_prop!(BorderRightWidth, border_right_width, BorderLeftWidth, border_left_width, $val);
        }
      };
      (BorderInlineEndColor => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineEndColor($val));
        } else {
          logical_prop!(BorderRightColor, border_right_color, BorderLeftColor, border_left_color, $val);
        }
      };
      (BorderInlineEndStyle => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderInlineEndStyle($val));
        } else {
          logical_prop!(BorderRightStyle, border_right_style, BorderLeftStyle, border_left_style, $val);
        }
      };
      (BorderBlockStart => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockStart($val));
        } else {
          dest.push(Property::BorderTop($val));
        }
      };
      (BorderBlockStartWidth => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockStartWidth($val));
        } else {
          dest.push(Property::BorderTopWidth($val));
        }
      };
      (BorderBlockStartColor => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockStartColor($val));
        } else {
          dest.push(Property::BorderTopColor($val));
        }
      };
      (BorderBlockStartStyle => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockStartStyle($val));
        } else {
          dest.push(Property::BorderTopStyle($val));
        }
      };
      (BorderBlockEnd => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockEnd($val));
        } else {
          dest.push(Property::BorderBottom($val));
        }
      };
      (BorderBlockEndWidth => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockEndWidth($val));
        } else {
          dest.push(Property::BorderBottomWidth($val));
        }
      };
      (BorderBlockEndColor => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockEndColor($val));
        } else {
          dest.push(Property::BorderBottomColor($val));
        }
      };
      (BorderBlockEndStyle => $val: expr) => {
        if logical_supported {
          dest.push(Property::BorderBlockEndStyle($val));
        } else {
          dest.push(Property::BorderBottomStyle($val));
        }
      };
      ($prop: ident => $val: expr) => {
        dest.push(Property::$prop($val))
      };
    }

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
                prop!($color => $other.color.clone().unwrap());
              } else if eq_width && eq_color {
                prop!($style => $other.style.clone().unwrap());
              } else if eq_style && eq_color {
                prop!($width => $other.width.clone().unwrap());
              } else {
                prop!($prop => $other.to_border());
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
            prop!($block_start_prop => $block_start.to_border());
            prop!($block_end_prop => $block_end.to_border());
            prop!($inline_start_prop => $inline_start.to_border());
            prop!($inline_end_prop => $inline_end.to_border());
          }
        } else {
          macro_rules! shorthand {
            ($prop: ident, $key: ident) => {{
              let has_prop = $block_start.$key.is_some() && $block_end.$key.is_some() && $inline_start.$key.is_some() && $inline_end.$key.is_some();
              if has_prop {
                if !$is_logical || ($block_start.$key == $block_end.$key && $block_end.$key == $inline_start.$key && $inline_start.$key == $inline_end.$key) {
                  let rect = Rect::new(
                    std::mem::take(&mut $block_start.$key).unwrap(),
                    std::mem::take(&mut $inline_end.$key).unwrap(),
                    std::mem::take(&mut $block_end.$key).unwrap(),
                    std::mem::take(&mut $inline_start.$key).unwrap()
                  );
                  prop!($prop => rect);
                }
              }
            }};
          }

          macro_rules! logical_shorthand {
            ($prop: ident, $key: ident, $start: expr, $end: expr) => {{
              let has_prop = $start.$key.is_some() && $start.$key == $end.$key;
              if has_prop {
                prop!($prop => std::mem::take(&mut $start.$key).unwrap());
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
                prop!($shorthand => $val.to_border());
              } else {
                if let Some(style) = &$val.style {
                  prop!($style => style.clone());
                }
        
                if let Some(width) = &$val.width {
                  prop!($width => width.clone());
                }
        
                if let Some(color) = &$val.color {
                  prop!($color => color.clone());
                }
              }
            };
          }

          if $is_logical && $block_start == $block_end && $block_start.is_valid() {
            if logical_supported {
              dest.push(BorderBlock($block_start.to_border()));
            } else {
              dest.push(BorderTop($block_start.to_border()));
              dest.push(BorderBottom($block_start.to_border()));
            }
          } else {
            if $is_logical && logical_supported && !$block_start.is_valid() && !$block_end.is_valid() {
              logical_shorthand!(BorderBlockStyle, style, $block_start, $block_end);
              logical_shorthand!(BorderBlockWidth, width, $block_start, $block_end);
              logical_shorthand!(BorderBlockColor, color, $block_start, $block_end);
            }

            side!($block_start, $block_start_prop, $block_start_width, $block_start_style, $block_start_color);
            side!($block_end, $block_end_prop, $block_end_width, $block_end_style, $block_end_color);
          }

          if $is_logical && $inline_start == $inline_end && $inline_start.is_valid() {
            if logical_supported {
              dest.push(BorderInline($inline_start.to_border()));
            } else {
              dest.push(BorderLeft($inline_start.to_border()));
              dest.push(BorderRight($inline_start.to_border()));
            }
          } else {
            if $is_logical && logical_supported && !$inline_start.is_valid() && !$inline_end.is_valid() {
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

  fn flush_unparsed(&mut self, unparsed: &UnparsedProperty, dest: &mut DeclarationList, logical_properties: &mut LogicalProperties) {
    let logical_supported = logical_properties.is_supported(Feature::LogicalBorders);
    if logical_supported {
      dest.push(Property::Unparsed(unparsed.clone()));
      return
    }

    macro_rules! prop {
      ($id: ident) => {{
        dest.push(Property::Unparsed(unparsed.with_property_id(PropertyId::$id)));
      }};
    }

    macro_rules! logical_prop {
      ($ltr: ident, $ltr_key: ident, $rtl: ident, $rtl_key: ident) => {{
        let ltr = Some(Box::new(Property::Unparsed(unparsed.with_property_id(PropertyId::$ltr))));
        let rtl = Some(Box::new(Property::Unparsed(unparsed.with_property_id(PropertyId::$rtl))));
        logical_properties.used = true;
        if let Some(Property::Logical(property)) = get_physical!(self.physical_to_logical, $ltr_key, dest) {
          property.ltr = ltr;
        } else {
          self.physical_to_logical.$ltr_key = Some(dest.len());
          dest.push(Property::Logical(LogicalProperty {
            property_id: PropertyId::$ltr,
            ltr,
            rtl: None
          }));
        }

        if let Some(Property::Logical(property)) = get_physical!(self.physical_to_logical, $rtl_key, dest) {
          property.rtl = rtl;
        } else {
          self.physical_to_logical.$rtl_key = Some(dest.len());
          dest.push(Property::Logical(LogicalProperty {
            property_id: PropertyId::$rtl,
            ltr: None,
            rtl
          }));
        }
      }};
    }

    use PropertyId::*;
    match &unparsed.property_id {
      BorderInlineStart => logical_prop!(BorderLeft, border_left, BorderRight, border_right),
      BorderInlineStartWidth => logical_prop!(BorderLeftWidth, border_left_width, BorderRightWidth, border_right_width),
      BorderInlineStartColor => logical_prop!(BorderLeftColor, border_left_color, BorderRightColor, border_right_color),
      BorderInlineStartStyle => logical_prop!(BorderLeftStyle, border_left_style, BorderRightStyle, border_right_style),
      BorderInlineEnd => logical_prop!(BorderRight, border_right, BorderLeft, border_left),
      BorderInlineEndWidth => logical_prop!(BorderRightWidth, border_right_width, BorderLeftWidth, border_left_width),
      BorderInlineEndColor => logical_prop!(BorderRightColor, border_right_color, BorderLeftColor, border_left_color),
      BorderInlineEndStyle => logical_prop!(BorderRightStyle, border_right_style, BorderLeftStyle, border_left_style),
      BorderBlockStart => prop!(BorderTop),
      BorderBlockStartWidth => prop!(BorderTopWidth),
      BorderBlockStartColor => prop!(BorderTopColor),
      BorderBlockStartStyle => prop!(BorderTopStyle),
      BorderBlockEnd => prop!(BorderBottom),
      BorderBlockEndWidth => prop!(BorderBottomWidth),
      BorderBlockEndColor => prop!(BorderBottomColor),
      BorderBlockEndStyle => prop!(BorderBottomStyle),
      _ => {
        dest.push(Property::Unparsed(unparsed.clone()));
      }
    }
  }
}

fn is_border_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BorderTopColor |
    PropertyId::BorderBottomColor |
    PropertyId::BorderLeftColor |
    PropertyId::BorderRightColor |
    PropertyId::BorderBlockStartColor |
    PropertyId::BorderBlockEndColor |
    PropertyId::BorderBlockColor |
    PropertyId::BorderInlineStartColor |
    PropertyId::BorderInlineEndColor |
    PropertyId::BorderInlineColor |
    PropertyId::BorderTopWidth |
    PropertyId::BorderBottomWidth |
    PropertyId::BorderLeftWidth |
    PropertyId::BorderRightWidth |
    PropertyId::BorderBlockStartWidth |
    PropertyId::BorderBlockEndWidth |
    PropertyId::BorderBlockWidth |
    PropertyId::BorderInlineStartWidth |
    PropertyId::BorderInlineEndWidth |
    PropertyId::BorderInlineWidth |
    PropertyId::BorderTopStyle |
    PropertyId::BorderBottomStyle |
    PropertyId::BorderLeftStyle |
    PropertyId::BorderRightStyle |
    PropertyId::BorderBlockStartStyle |
    PropertyId::BorderBlockEndStyle |
    PropertyId::BorderBlockStyle |
    PropertyId::BorderInlineStartStyle |
    PropertyId::BorderInlineEndStyle |
    PropertyId::BorderInlineStyle |
    PropertyId::BorderTop |
    PropertyId::BorderBottom |
    PropertyId::BorderLeft |
    PropertyId::BorderRight |
    PropertyId::BorderBlockStart |
    PropertyId::BorderBlockEnd |
    PropertyId::BorderInlineStart |
    PropertyId::BorderInlineEnd |
    PropertyId::BorderBlock |
    PropertyId::BorderInline |
    PropertyId::BorderWidth |
    PropertyId::BorderStyle |
    PropertyId::BorderColor |
    PropertyId::Border => true,
    _ => false
  }
}