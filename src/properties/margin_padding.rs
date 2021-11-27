use crate::values::{
  size::Size2D,
  length::LengthPercentageOrAuto,
  rect::Rect
};
use crate::properties::Property;
use crate::declaration::DeclarationList;
use crate::traits::PropertyHandler;

#[derive(Debug, PartialEq)]
enum SideCategory {
  Logical,
  Physical
}

impl Default for SideCategory {
  fn default() -> SideCategory {
    SideCategory::Physical
  }
}

macro_rules! side_handler {
  ($name: ident, $top: ident, $bottom: ident, $left: ident, $right: ident, $block_start: ident, $block_end: ident, $inline_start: ident, $inline_end: ident, $shorthand: ident, $block_shorthand: ident, $inline_shorthand: ident) => {
    #[derive(Debug, Default)]
    pub(crate) struct $name {
      top: Option<LengthPercentageOrAuto>,
      bottom: Option<LengthPercentageOrAuto>,
      left: Option<LengthPercentageOrAuto>,
      right: Option<LengthPercentageOrAuto>,
      block_start: Option<LengthPercentageOrAuto>,
      block_end: Option<LengthPercentageOrAuto>,
      inline_start: Option<LengthPercentageOrAuto>,
      inline_end: Option<LengthPercentageOrAuto>,
      category: SideCategory
    }

    impl PropertyHandler for $name {
      fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
        use Property::*;
        use SideCategory::*;

        macro_rules! property {
          ($key: ident, $val: ident, $category: ident) => {{
            if $category != self.category {
              self.flush(dest);
            }
            self.$key = Some($val.clone());
            self.category = $category;
          }};
        }

        macro_rules! set_shorthand {
          ($start: ident, $end: ident, $val: ident) => {{
            if self.category != Logical {
              self.flush(dest);
            }
            self.$start = Some($val.0.clone());
            self.$end = Some($val.1.clone());
            self.category = Logical;
          }};
        }

        match &property {
          $top(val) => property!(top, val, Physical),
          $bottom(val) => property!(bottom, val, Physical),
          $left(val) => property!(left, val, Physical),
          $right(val) => property!(right, val, Physical),
          $block_start(val) => property!(block_start, val, Logical),
          $block_end(val) => property!(block_end, val, Logical),
          $inline_start(val) => property!(inline_start, val, Logical),
          $inline_end(val) => property!(inline_end, val, Logical),
          $block_shorthand(val) => set_shorthand!(block_start, block_end, val),
          $inline_shorthand(val) => set_shorthand!(inline_start, inline_end, val),
          $shorthand(val) => {
            // dest.clear();
            self.top = Some(val.0.clone());
            self.right = Some(val.1.clone());
            self.bottom = Some(val.2.clone());
            self.left = Some(val.3.clone());
            self.block_start = None;
            self.block_end = None;
            self.inline_start = None;
            self.inline_end = None;
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, dest: &mut DeclarationList) {
        self.flush(dest);
      }
    }

    impl $name {
      fn flush(&mut self, dest: &mut DeclarationList) {
        use Property::*;

        let top = std::mem::take(&mut self.top);
        let bottom = std::mem::take(&mut self.bottom);
        let left = std::mem::take(&mut self.left);
        let right = std::mem::take(&mut self.right);

        if top.is_some() && bottom.is_some() && left.is_some() && right.is_some() {
          let rect = Rect::new(top.unwrap(), right.unwrap(), bottom.unwrap(), left.unwrap());
          dest.push($shorthand(rect));
        } else {
          if let Some(val) = top {
            dest.push($top(val));
          }

          if let Some(val) = bottom {
            dest.push($bottom(val));
          }

          if let Some(val) = left {
            dest.push($left(val));
          }

          if let Some(val) = right {
            dest.push($right(val));
          }
        }

        let block_start = std::mem::take(&mut self.block_start);
        let block_end = std::mem::take(&mut self.block_end);
        let inline_start = std::mem::take(&mut self.inline_start);
        let inline_end = std::mem::take(&mut self.inline_end);

        macro_rules! logical_side {
          ($start: ident, $end: ident, $shorthand_prop: ident, $start_prop: ident, $end_prop: ident) => {
            if ($start.is_some() && $end.is_some()) {
              let size = Size2D($start.unwrap(), $end.unwrap());
              dest.push($shorthand_prop(size));
            } else {
              if let Some(val) = $start {
                dest.push($start_prop(val));
              }
    
              if let Some(val) = $end {
                dest.push($end_prop(val));
              }
            }    
          };
        }

        logical_side!(block_start, block_end, $block_shorthand, $block_start, $block_end);
        logical_side!(inline_start, inline_end, $inline_shorthand, $inline_start, $inline_end);
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
  MarginInline
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
  PaddingInline
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
  ScrollMarginInline
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
  ScrollPaddingInline
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
  InsetInline
);
