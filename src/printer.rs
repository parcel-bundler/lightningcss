use std::fmt::*;
use cssparser::SourceLocation;
use parcel_sourcemap::{SourceMap, OriginalLocation};
use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;

pub struct Printer<'a, W> {
  dest: &'a mut W,
  source_map: Option<&'a mut SourceMap>,
  indent: u8,
  line: u32,
  col: u32,
  pub minify: bool,
  pub targets: Option<Browsers>,
  /// Vendor prefix override. When non-empty, it overrides 
  /// the vendor prefix of whatever is being printed.
  pub vendor_prefix: VendorPrefix
}

impl<'a, W: Write + Sized> Printer<'a, W> {
  pub fn new(
    dest: &'a mut W,
    source_map: Option<&'a mut SourceMap>,
    minify: bool,
    targets: Option<Browsers>
  ) -> Printer<'a, W> {
    Printer {
      dest,
      source_map,
      indent: 0,
      line: 0,
      col: 0,
      minify,
      targets,
      vendor_prefix: VendorPrefix::empty()
    }
  }

  pub fn write_str(&mut self, s: &str) -> Result {
    self.col += s.len() as u32;
    self.dest.write_str(s)
  }

  pub fn write_char(&mut self, c: char) -> Result {
    if c == '\n' {
      self.line += 1;
      self.col = 0;
    } else {
      self.col += 1;
    }
    self.dest.write_char(c)
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

    self.write_char('\n')?;
    if self.indent > 0 {
      self.write_str(&" ".repeat(self.indent as usize))?;
    }

    Ok(())
  }

  pub fn indent(&mut self) {
    self.indent += 2;
  }

  pub fn dedent(&mut self) {
    self.indent -= 2;
  }

  pub fn add_mapping(&mut self, loc: SourceLocation) {
    if let Some(map) = &mut self.source_map {
      map.add_mapping(self.line, self.col, Some(OriginalLocation {
        original_line: loc.line,
        original_column: loc.column - 1,
        source: 0,
        name: None
      }))
    }
  }
}

impl<'a, W: Write + Sized> Write for Printer<'a, W> {
  fn write_str(&mut self, s: &str) -> Result {
    self.col += s.len() as u32;
    self.dest.write_str(s)
  }
}
