#[derive(Debug, PartialEq)]
pub enum PropertyCategory {
  Logical,
  Physical,
}

impl Default for PropertyCategory {
  fn default() -> PropertyCategory {
    PropertyCategory::Physical
  }
}

#[derive(PartialEq)]
pub enum LogicalGroup {
  BorderColor,
  BorderStyle,
  BorderWidth,
  BorderRadius,
  Margin,
  ScrollMargin,
  Padding,
  ScrollPadding,
  Inset,
  Size,
  MinSize,
  MaxSize,
}
