use std::fmt::*;

pub struct Printer<'a, W> {
  dest: &'a mut W,
  pub minify: bool
}

impl<'a, W: Write + Sized> Printer<'a, W> {
  pub fn new(dest: &mut W, minify: bool) -> Printer<W> {
    Printer { dest, minify }
  }

  pub fn write_str(&mut self, s: &str) -> Result {
    self.dest.write_str(s)
  }

  pub fn whitespace(&mut self) -> Result {
    if self.minify {
      return Ok(())
    }

    self.write_char(' ')
  }

  pub fn delim(&mut self, delim: char, ws_before: bool) -> Result {
    if ws_before {
      self.whitespace()?;
    }
    self.write_char(delim)?;
    self.whitespace()
  }

  pub fn newline(&mut self) -> Result {
    if self.minify {
      return Ok(())
    }

    self.write_char('\n')
  }
}

impl<'a, W: Write + Sized> Write for Printer<'a, W> {
  fn write_str(&mut self, s: &str) -> Result {
    self.dest.write_str(s)
  }
}
