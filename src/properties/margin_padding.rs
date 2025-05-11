use crate::compat::Feature;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::logical::PropertyCategory;
use crate::macros::{define_shorthand, rect_shorthand, size_shorthand};
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::traits::{IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::{length::LengthPercentageOrAuto, rect::Rect, size::Size2D};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

rect_shorthand! {
  /// A value for the [margin](https://drafts.csswg.org/css-box-4/#propdef-margin) shorthand property.
  pub struct Margin<LengthPercentageOrAuto> {
    MarginTop,
    MarginRight,
    MarginBottom,
    MarginLeft
  }
}

rect_shorthand! {
  /// A value for the [padding](https://drafts.csswg.org/css-box-4/#propdef-padding) shorthand property.
  pub struct Padding<LengthPercentageOrAuto> {
    PaddingTop,
    PaddingRight,
    PaddingBottom,
    PaddingLeft
  }
}

rect_shorthand! {
  /// A value for the [scroll-margin](https://drafts.csswg.org/css-scroll-snap/#scroll-margin) shorthand property.
  pub struct ScrollMargin<LengthPercentageOrAuto> {
    ScrollMarginTop,
    ScrollMarginRight,
    ScrollMarginBottom,
    ScrollMarginLeft
  }
}

rect_shorthand! {
  /// A value for the [scroll-padding](https://drafts.csswg.org/css-scroll-snap/#scroll-padding) shorthand property.
  pub struct ScrollPadding<LengthPercentageOrAuto> {
    ScrollPaddingTop,
    ScrollPaddingRight,
    ScrollPaddingBottom,
    ScrollPaddingLeft
  }
}

rect_shorthand! {
  /// A value for the [inset](https://drafts.csswg.org/css-logical/#propdef-inset) shorthand property.
  pub struct Inset<LengthPercentageOrAuto> {
    Top,
    Right,
    Bottom,
    Left
  }
}

size_shorthand! {
  /// A value for the [margin-block](https://drafts.csswg.org/css-logical/#propdef-margin-block) shorthand property.
  pub struct MarginBlock<LengthPercentageOrAuto> {
    /// The block start value.
    block_start: MarginBlockStart,
    /// The block end value.
    block_end: MarginBlockEnd,
  }
}

size_shorthand! {
  /// A value for the [margin-inline](https://drafts.csswg.org/css-logical/#propdef-margin-inline) shorthand property.
  pub struct MarginInline<LengthPercentageOrAuto> {
    /// The inline start value.
    inline_start: MarginInlineStart,
    /// The inline end value.
    inline_end: MarginInlineEnd,
  }
}

size_shorthand! {
  /// A value for the [padding-block](https://drafts.csswg.org/css-logical/#propdef-padding-block) shorthand property.
  pub struct PaddingBlock<LengthPercentageOrAuto> {
     /// The block start value.
    block_start: PaddingBlockStart,
    /// The block end value.
    block_end: PaddingBlockEnd,
  }
}

size_shorthand! {
  /// A value for the [padding-inline](https://drafts.csswg.org/css-logical/#propdef-padding-inline) shorthand property.
  pub struct PaddingInline<LengthPercentageOrAuto> {
    /// The inline start value.
    inline_start: PaddingInlineStart,
    /// The inline end value.
    inline_end: PaddingInlineEnd,
  }
}

size_shorthand! {
  /// A value for the [scroll-margin-block](https://drafts.csswg.org/css-scroll-snap/#propdef-scroll-margin-block) shorthand property.
  pub struct ScrollMarginBlock<LengthPercentageOrAuto> {
     /// The block start value.
    block_start: ScrollMarginBlockStart,
    /// The block end value.
    block_end: ScrollMarginBlockEnd,
  }
}

size_shorthand! {
  /// A value for the [scroll-margin-inline](https://drafts.csswg.org/css-scroll-snap/#propdef-scroll-margin-inline) shorthand property.
  pub struct ScrollMarginInline<LengthPercentageOrAuto> {
    /// The inline start value.
    inline_start: ScrollMarginInlineStart,
    /// The inline end value.
    inline_end: ScrollMarginInlineEnd,
  }
}

size_shorthand! {
  /// A value for the [scroll-padding-block](https://drafts.csswg.org/css-scroll-snap/#propdef-scroll-padding-block) shorthand property.
  pub struct ScrollPaddingBlock<LengthPercentageOrAuto> {
     /// The block start value.
    block_start: ScrollPaddingBlockStart,
    /// The block end value.
    block_end: ScrollPaddingBlockEnd,
  }
}

size_shorthand! {
  /// A value for the [scroll-padding-inline](https://drafts.csswg.org/css-scroll-snap/#propdef-scroll-padding-inline) shorthand property.
  pub struct ScrollPaddingInline<LengthPercentageOrAuto> {
    /// The inline start value.
    inline_start: ScrollPaddingInlineStart,
    /// The inline end value.
    inline_end: ScrollPaddingInlineEnd,
  }
}

size_shorthand! {
  /// A value for the [inset-block](https://drafts.csswg.org/css-logical/#propdef-inset-block) shorthand property.
  pub struct InsetBlock<LengthPercentageOrAuto> {
     /// The block start value.
    block_start: InsetBlockStart,
    /// The block end value.
    block_end: InsetBlockEnd,
  }
}

size_shorthand! {
  /// A value for the [inset-inline](https://drafts.csswg.org/css-logical/#propdef-inset-inline) shorthand property.
  pub struct InsetInline<LengthPercentageOrAuto> {
    /// The inline start value.
    inline_start: InsetInlineStart,
    /// The inline end value.
    inline_end: InsetInlineEnd,
  }
}

macro_rules! side_handler {
  ($name: ident, $top: ident, $bottom: ident, $left: ident, $right: ident, $block_start: ident, $block_end: ident, $inline_start: ident, $inline_end: ident, $shorthand: ident, $block_shorthand: ident, $inline_shorthand: ident, $shorthand_category: ident $(, $feature: ident, $shorthand_feature: ident)?) => {
    #[derive(Debug, Default)]
    pub(crate) struct $name<'i> {
      top: Option<LengthPercentageOrAuto>,
      bottom: Option<LengthPercentageOrAuto>,
      left: Option<LengthPercentageOrAuto>,
      right: Option<LengthPercentageOrAuto>,
      block_start: Option<Property<'i>>,
      block_end: Option<Property<'i>>,
      inline_start: Option<Property<'i>>,
      inline_end: Option<Property<'i>>,
      has_any: bool,
      category: PropertyCategory
    }

    impl<'i> PropertyHandler<'i> for $name<'i> {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) -> bool {
        use Property::*;

        macro_rules! flush {
          ($key: ident, $val: expr, $category: ident) => {{
            // If the category changes betweet logical and physical,
            // or if the value contains syntax that isn't supported across all targets,
            // preserve the previous value as a fallback.
            if PropertyCategory::$category != self.category || (self.$key.is_some() && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets))) {
              self.flush(dest, context);
            }
          }}
        }

        macro_rules! property {
          ($key: ident, $val: ident, $category: ident) => {{
            flush!($key, $val, $category);
            self.$key = Some($val.clone());
            self.category = PropertyCategory::$category;
            self.has_any = true;
          }};
        }

        macro_rules! logical_property {
          ($prop: ident, $val: expr) => {{
            // Assume unparsed properties might contain unsupported syntax that we must preserve as a fallback.
            if self.category != PropertyCategory::Logical || (self.$prop.is_some() && matches!($val, Property::Unparsed(_))) {
              self.flush(dest, context);
            }

            self.$prop = Some($val);
            self.category = PropertyCategory::Logical;
            self.has_any = true;
          }};
        }

        match &property {
          $top(val) => property!(top, val, Physical),
          $bottom(val) => property!(bottom, val, Physical),
          $left(val) => property!(left, val, Physical),
          $right(val) => property!(right, val, Physical),
          $block_start(val) => {
            flush!(block_start, val, Logical);
            logical_property!(block_start, property.clone());
          },
          $block_end(val) => {
            flush!(block_end, val, Logical);
            logical_property!(block_end, property.clone());
          },
          $inline_start(val) => {
            flush!(inline_start, val, Logical);
            logical_property!(inline_start, property.clone())
          },
          $inline_end(val) => {
            flush!(inline_end, val, Logical);
            logical_property!(inline_end, property.clone());
          },
          $block_shorthand(val) => {
            flush!(block_start, val.block_start, Logical);
            flush!(block_end, val.block_end, Logical);
            logical_property!(block_start, Property::$block_start(val.block_start.clone()));
            logical_property!(block_end, Property::$block_end(val.block_end.clone()));
          },
          $inline_shorthand(val) => {
            flush!(inline_start, val.inline_start, Logical);
            flush!(inline_end, val.inline_end, Logical);
            logical_property!(inline_start, Property::$inline_start(val.inline_start.clone()));
            logical_property!(inline_end, Property::$inline_end(val.inline_end.clone()));
          },
          $shorthand(val) => {
            flush!(top, val.top, $shorthand_category);
            flush!(right, val.right, $shorthand_category);
            flush!(bottom, val.bottom, $shorthand_category);
            flush!(left, val.left, $shorthand_category);
            self.top = Some(val.top.clone());
            self.right = Some(val.right.clone());
            self.bottom = Some(val.bottom.clone());
            self.left = Some(val.left.clone());
            self.block_start = None;
            self.block_end = None;
            self.inline_start = None;
            self.inline_end = None;
            self.has_any = true;
          }
          Unparsed(val) if matches!(val.property_id, PropertyId::$top | PropertyId::$bottom | PropertyId::$left | PropertyId::$right | PropertyId::$block_start | PropertyId::$block_end | PropertyId::$inline_start | PropertyId::$inline_end | PropertyId::$block_shorthand | PropertyId::$inline_shorthand | PropertyId::$shorthand) => {
            // Even if we weren't able to parse the value (e.g. due to var() references),
            // we can still add vendor prefixes to the property itself.
            match &val.property_id {
              PropertyId::$block_start => logical_property!(block_start, property.clone()),
              PropertyId::$block_end => logical_property!(block_end, property.clone()),
              PropertyId::$inline_start => logical_property!(inline_start, property.clone()),
              PropertyId::$inline_end => logical_property!(inline_end, property.clone()),
              _ => {
                self.flush(dest, context);
                dest.push(property.clone());
              }
            }
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
        self.flush(dest, context);
      }
    }

    impl<'i> $name<'i> {
      fn flush(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
        if !self.has_any {
          return
        }

        self.has_any = false;

        let top = std::mem::take(&mut self.top);
        let bottom = std::mem::take(&mut self.bottom);
        let left = std::mem::take(&mut self.left);
        let right = std::mem::take(&mut self.right);
        let logical_supported = true $(&& !context.should_compile_logical(Feature::$feature))?;

        if (PropertyCategory::$shorthand_category != PropertyCategory::Logical || logical_supported) && top.is_some() && bottom.is_some() && left.is_some() && right.is_some() {
          dest.push(Property::$shorthand($shorthand {
            top: top.unwrap(),
            right: right.unwrap(),
            bottom: bottom.unwrap(),
            left: left.unwrap()
          }));
        } else {
          if let Some(val) = top {
            dest.push(Property::$top(val));
          }

          if let Some(val) = bottom {
            dest.push(Property::$bottom(val));
          }

          if let Some(val) = left {
            dest.push(Property::$left(val));
          }

          if let Some(val) = right {
            dest.push(Property::$right(val));
          }
        }

        let block_start = std::mem::take(&mut self.block_start);
        let block_end = std::mem::take(&mut self.block_end);
        let inline_start = std::mem::take(&mut self.inline_start);
        let inline_end = std::mem::take(&mut self.inline_end);

        macro_rules! logical_side {
          ($start: ident, $end: ident, $shorthand_prop: ident, $start_prop: ident, $end_prop: ident) => {
            let shorthand_supported = logical_supported $(&& !context.should_compile_logical(Feature::$shorthand_feature))?;
            if let (Some(Property::$start_prop(start)), Some(Property::$end_prop(end)), true) = (&$start, &$end, shorthand_supported) {
              dest.push(Property::$shorthand_prop($shorthand_prop {
                $start: start.clone(),
                $end: end.clone()
              }));
            } else {
              if let Some(val) = $start {
                dest.push(val);
              }

              if let Some(val) = $end {
                dest.push(val);
              }
            }
          };
        }

        macro_rules! prop {
          ($val: ident, $logical: ident, $physical: ident) => {
            match $val {
              Some(Property::$logical(val)) => {
                dest.push(Property::$physical(val));
              }
              Some(Property::Unparsed(val)) => {
                dest.push(Property::Unparsed(val.with_property_id(PropertyId::$physical)));
              }
              _ => {}
            }
          }
        }

        if logical_supported {
          logical_side!(block_start, block_end, $block_shorthand, $block_start, $block_end);
        } else {
          prop!(block_start, $block_start, $top);
          prop!(block_end, $block_end, $bottom);
        }

        if logical_supported {
          logical_side!(inline_start, inline_end, $inline_shorthand, $inline_start, $inline_end);
        } else if inline_start.is_some() || inline_end.is_some() {
          if matches!((&inline_start, &inline_end), (Some(Property::$inline_start(start)), Some(Property::$inline_end(end))) if start == end) {
            prop!(inline_start, $inline_start, $left);
            prop!(inline_end, $inline_end, $right);
          } else {
            macro_rules! logical_prop {
              ($val: ident, $logical: ident, $ltr: ident, $rtl: ident) => {
                match $val {
                  Some(Property::$logical(val)) => {
                    context.add_logical_rule(
                      Property::$ltr(val.clone()),
                      Property::$rtl(val)
                    );
                  }
                  Some(Property::Unparsed(val)) => {
                    context.add_logical_rule(
                      Property::Unparsed(val.with_property_id(PropertyId::$ltr)),
                      Property::Unparsed(val.with_property_id(PropertyId::$rtl))
                    );
                  }
                  _ => {}
                }
              }
            }

            logical_prop!(inline_start, $inline_start, $left, $right);
            logical_prop!(inline_end, $inline_end, $right, $left);
          }
        }
      }
    }
  };
}

side_handler!(
  MarginHandler,
  MarginTop,
  MarginBottom,
  MarginLeft,
  MarginRight,
  MarginBlockStart,
  MarginBlockEnd,
  MarginInlineStart,
  MarginInlineEnd,
  Margin,
  MarginBlock,
  MarginInline,
  Physical,
  LogicalMargin,
  LogicalMarginShorthand
);

side_handler!(
  PaddingHandler,
  PaddingTop,
  PaddingBottom,
  PaddingLeft,
  PaddingRight,
  PaddingBlockStart,
  PaddingBlockEnd,
  PaddingInlineStart,
  PaddingInlineEnd,
  Padding,
  PaddingBlock,
  PaddingInline,
  Physical,
  LogicalPadding,
  LogicalPaddingShorthand
);

side_handler!(
  ScrollMarginHandler,
  ScrollMarginTop,
  ScrollMarginBottom,
  ScrollMarginLeft,
  ScrollMarginRight,
  ScrollMarginBlockStart,
  ScrollMarginBlockEnd,
  ScrollMarginInlineStart,
  ScrollMarginInlineEnd,
  ScrollMargin,
  ScrollMarginBlock,
  ScrollMarginInline,
  Physical
);

side_handler!(
  ScrollPaddingHandler,
  ScrollPaddingTop,
  ScrollPaddingBottom,
  ScrollPaddingLeft,
  ScrollPaddingRight,
  ScrollPaddingBlockStart,
  ScrollPaddingBlockEnd,
  ScrollPaddingInlineStart,
  ScrollPaddingInlineEnd,
  ScrollPadding,
  ScrollPaddingBlock,
  ScrollPaddingInline,
  Physical
);

side_handler!(
  InsetHandler,
  Top,
  Bottom,
  Left,
  Right,
  InsetBlockStart,
  InsetBlockEnd,
  InsetInlineStart,
  InsetInlineEnd,
  Inset,
  InsetBlock,
  InsetInline,
  Logical,
  LogicalInset,
  LogicalInset
);
