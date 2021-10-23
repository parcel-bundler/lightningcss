use std::fmt::*;

pub struct Printer<'a, W> {
  dest: &'a mut W
}

impl<'a, W: Write + Sized> Printer<'a, W> {
  pub fn new(dest: &mut W) -> Printer<W> {
    Printer { dest }
  }

  pub fn write_str(&mut self, s: &str) -> Result {
    self.dest.write_str(s)
  }
}

impl<'a, W: Write + Sized> Write for Printer<'a, W> {
  fn write_str(&mut self, s: &str) -> Result {
    self.dest.write_str(s)
  }
}
