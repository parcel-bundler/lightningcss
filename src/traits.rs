use cssparser::*;
use crate::properties::Property;
use crate::declaration::DeclarationList;
use crate::printer::Printer;

pub trait Parse: Sized {
  /// Parse a value of this type.
  ///
  /// Returns an error on failure.
  fn parse<'i, 't>(
      input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ()>>;
}

/// Trait for things the can serialize themselves in CSS syntax.
pub(crate) trait ToCss {
  /// Serialize `self` in CSS syntax, writing to `dest`.
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write;

  /// Serialize `self` in CSS syntax and return a string.
  ///
  /// (This is a convenience wrapper for `to_css` and probably should not be overridden.)
  #[inline]
  fn to_css_string(&self) -> String {
      let mut s = String::new();
      let mut printer = Printer::new("", &mut s, None, false, None);
      self.to_css(&mut printer).unwrap();
      s
  }
}

impl<'a, T> ToCss for &'a T
where
    T: ToCss + ?Sized,
{
    fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
      (*self).to_css(dest)
    }
}

pub(crate) trait PropertyHandler: Sized {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool;
  fn finalize(&mut self, dest: &mut DeclarationList);
}

pub trait TryAdd<T> {
  fn try_add(&self, other: &T) -> Option<T>;
}

pub trait FromStandard<T>: Sized {
  fn from_standard(val: &T) -> Option<Self>;
}
