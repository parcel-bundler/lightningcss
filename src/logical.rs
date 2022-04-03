#[derive(Debug, PartialEq)]
pub(crate) enum PropertyCategory {
  Logical,
  Physical
}

impl Default for PropertyCategory {
  fn default() -> PropertyCategory {
    PropertyCategory::Physical
  }
}
