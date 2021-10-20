use crate::values::{length::{Size2D, LengthPercentageOrAuto}, rect::Rect};
use crate::properties::Property;

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
    pub struct $name {
      top: Option<LengthPercentageOrAuto>,
      bottom: Option<LengthPercentageOrAuto>,
      left: Option<LengthPercentageOrAuto>,
      right: Option<LengthPercentageOrAuto>,
      block_start: Option<LengthPercentageOrAuto>,
      block_end: Option<LengthPercentageOrAuto>,
      inline_start: Option<LengthPercentageOrAuto>,
      inline_end: Option<LengthPercentageOrAuto>,
      category: SideCategory,
      decls: Vec<Property>
    }

    impl $name {
      pub fn handle_property(&mut self, property: &Property) -> bool {
        use Property::*;
        use SideCategory::*;

        macro_rules! property {
          ($key: ident, $val: ident, $category: ident) => {{
            if $category != self.category {
              self.flush();
            }
            self.$key = Some($val.clone());
            self.category = $category;
          }};
        }

        macro_rules! set_shorthand {
          ($start: ident, $end: ident, $val: ident) => {{
            if self.category != Logical {
              self.flush();
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
            self.decls.clear();
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

      fn flush(&mut self) {
        use Property::*;

        let top = std::mem::take(&mut self.top);
        let bottom = std::mem::take(&mut self.bottom);
        let left = std::mem::take(&mut self.left);
        let right = std::mem::take(&mut self.right);

        if top.is_some() && bottom.is_some() && left.is_some() && right.is_some() {
          let rect = Rect::new(top.unwrap(), right.unwrap(), bottom.unwrap(), left.unwrap());
          self.decls.push($shorthand(rect));
        } else {
          if let Some(val) = top {
            self.decls.push($top(val));
          }

          if let Some(val) = bottom {
            self.decls.push($bottom(val));
          }

          if let Some(val) = left {
            self.decls.push($left(val));
          }

          if let Some(val) = right {
            self.decls.push($right(val));
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
              self.decls.push($shorthand_prop(size));
            } else {
              if let Some(val) = $start {
                self.decls.push($start_prop(val));
              }
    
              if let Some(val) = $end {
                self.decls.push($end_prop(val));
              }
            }    
          };
        }

        logical_side!(block_start, block_end, $block_shorthand, $block_start, $block_end);
        logical_side!(inline_start, inline_end, $inline_shorthand, $inline_start, $inline_end);
      }

      pub fn finalize(&mut self) -> Vec<Property> {
        self.flush();
        std::mem::take(&mut self.decls)
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
