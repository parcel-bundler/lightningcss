//! CSS serialization and source map generation.

use crate::css_modules::CssModule;
use crate::dependencies::{Dependency, DependencyOptions};
use crate::error::{Error, ErrorLocation, PrinterError, PrinterErrorKind};
use crate::rules::{Location, StyleContext};
use crate::selector::SelectorList;
use crate::targets::{Targets, TargetsWithSupportsScope};
use crate::vendor_prefix::VendorPrefix;
use cssparser::{serialize_identifier, serialize_name};
#[cfg(feature = "sourcemap")]
use parcel_sourcemap::{OriginalLocation, SourceMap};

/// Options that control how CSS is serialized to a string.
#[derive(Default)]
pub struct PrinterOptions<'a> {
  /// Whether to minify the CSS, i.e. remove white space.
  pub minify: bool,
  /// An optional reference to a source map to write mappings into.
  #[cfg(feature = "sourcemap")]
  #[cfg_attr(docsrs, doc(cfg(feature = "sourcemap")))]
  pub source_map: Option<&'a mut SourceMap>,
  /// An optional project root path, used to generate relative paths for sources used in CSS module hashes.
  pub project_root: Option<&'a str>,
  /// Targets to output the CSS for.
  pub targets: Targets,
  /// Whether to analyze dependencies (i.e. `@import` and `url()`).
  /// If true, the dependencies are returned as part of the
  /// [ToCssResult](super::stylesheet::ToCssResult).
  ///
  /// When enabled, `@import` and `url()` dependencies
  /// are replaced with hashed placeholders that can be replaced with the final
  /// urls later (after bundling).
  pub analyze_dependencies: Option<DependencyOptions>,
  /// A mapping of pseudo classes to replace with class names that can be applied
  /// from JavaScript. Useful for polyfills, for example.
  pub pseudo_classes: Option<PseudoClasses<'a>>,
}

/// A mapping of user action pseudo classes to replace with class names.
///
/// See [PrinterOptions](PrinterOptions).
#[derive(Default, Debug)]
pub struct PseudoClasses<'a> {
  /// The class name to replace `:hover` with.
  pub hover: Option<&'a str>,
  /// The class name to replace `:active` with.
  pub active: Option<&'a str>,
  /// The class name to replace `:focus` with.
  pub focus: Option<&'a str>,
  /// The class name to replace `:focus-visible` with.
  pub focus_visible: Option<&'a str>,
  /// The class name to replace `:focus-within` with.
  pub focus_within: Option<&'a str>,
}

/// A `Printer` represents a destination to output serialized CSS, as used in
/// the [ToCss](super::traits::ToCss) trait. It can wrap any destination that
/// implements [std::fmt::Write](std::fmt::Write), such as a [String](String).
///
/// A `Printer` keeps track of the current line and column position, and uses
/// this to generate a source map if provided in the options.
///
/// `Printer` also includes helper functions that assist with writing output
/// that respects options such as `minify`, and `css_modules`.
pub struct Printer<'a, 'c, W> {
  pub(crate) sources: Option<&'c Vec<String>>,
  dest: &'a mut W,
  #[cfg(feature = "sourcemap")]
  #[cfg_attr(docsrs, doc(cfg(feature = "sourcemap")))]
  pub(crate) source_map: Option<&'a mut SourceMap>,
  #[cfg(feature = "sourcemap")]
  #[cfg_attr(docsrs, doc(cfg(feature = "sourcemap")))]
  pub(crate) source_maps: Vec<Option<SourceMap>>,
  pub(crate) loc: Location,
  indent: u8,
  line: u32,
  col: u32,
  pub(crate) minify: bool,
  pub(crate) targets: TargetsWithSupportsScope,
  /// Vendor prefix override. When non-empty, it overrides
  /// the vendor prefix of whatever is being printed.
  pub(crate) vendor_prefix: VendorPrefix,
  pub(crate) in_calc: bool,
  pub(crate) css_module: Option<CssModule<'a, 'c>>,
  pub(crate) dependencies: Option<Vec<Dependency>>,
  pub(crate) remove_imports: bool,
  pub(crate) pseudo_classes: Option<PseudoClasses<'a>>,
  context: Option<&'a StyleContext<'a, 'c>>,
}

impl<'a, 'c, W: std::fmt::Write + Sized> Printer<'a, 'c, W> {
  /// Create a new Printer wrapping the given destination.
  pub fn new(dest: &'a mut W, options: PrinterOptions<'a>) -> Self {
    Printer {
      sources: None,
      dest,
      #[cfg(feature = "sourcemap")]
      source_map: options.source_map,
      #[cfg(feature = "sourcemap")]
      source_maps: Vec::new(),
      loc: Location {
        source_index: 0,
        line: 0,
        column: 1,
      },
      indent: 0,
      line: 0,
      col: 0,
      minify: options.minify,
      targets: TargetsWithSupportsScope::new(options.targets),
      vendor_prefix: VendorPrefix::empty(),
      in_calc: false,
      css_module: None,
      dependencies: if options.analyze_dependencies.is_some() {
        Some(Vec::new())
      } else {
        None
      },
      remove_imports: matches!(&options.analyze_dependencies, Some(d) if d.remove_imports),
      pseudo_classes: options.pseudo_classes,
      context: None,
    }
  }

  /// Returns the current source filename that is being printed.
  pub fn filename(&self) -> &'c str {
    if let Some(sources) = self.sources {
      if let Some(f) = sources.get(self.loc.source_index as usize) {
        f
      } else {
        "unknown.css"
      }
    } else {
      "unknown.css"
    }
  }

  /// Writes a raw string to the underlying destination.
  ///
  /// NOTE: Is is assumed that the string does not contain any newline characters.
  /// If such a string is written, it will break source maps.
  pub fn write_str(&mut self, s: &str) -> Result<(), PrinterError> {
    self.col += utf16_len(s) as u32;
    self.dest.write_str(s)?;
    Ok(())
  }

  /// Writes a raw string which may contain newlines to the underlying destination.
  pub fn write_str_with_newlines(&mut self, s: &str) -> Result<(), PrinterError> {
    let mut last_line_start: usize = 0;

    for (idx, n) in s.char_indices() {
      if n == '\n' {
        self.line += 1;
        self.col = 0;

        // Keep track of where the *next* line starts
        last_line_start = idx + 1;
      }
    }

    self.col += utf16_len(&s[last_line_start..]) as u32;
    self.dest.write_str(s)?;
    Ok(())
  }

  /// Write a single character to the underlying destination.
  pub fn write_char(&mut self, c: char) -> Result<(), PrinterError> {
    if c == '\n' {
      self.line += 1;
      self.col = 0;
    } else {
      self.col += c.len_utf16() as u32;
    }
    self.dest.write_char(c)?;
    Ok(())
  }

  /// Writes a single whitespace character, unless the `minify` option is enabled.
  ///
  /// Use `write_char` instead if you wish to force a space character to be written,
  /// regardless of the `minify` option.
  pub fn whitespace(&mut self) -> Result<(), PrinterError> {
    if self.minify {
      return Ok(());
    }

    self.write_char(' ')
  }

  /// Writes a delimiter character, followed by whitespace (depending on the `minify` option).
  /// If `ws_before` is true, then whitespace is also written before the delimiter.
  pub fn delim(&mut self, delim: char, ws_before: bool) -> Result<(), PrinterError> {
    if ws_before {
      self.whitespace()?;
    }
    self.write_char(delim)?;
    self.whitespace()
  }

  /// Writes a newline character followed by indentation.
  /// If the `minify` option is enabled, then nothing is printed.
  pub fn newline(&mut self) -> Result<(), PrinterError> {
    if self.minify {
      return Ok(());
    }

    self.write_char('\n')?;
    if self.indent > 0 {
      self.write_str(&" ".repeat(self.indent as usize))?;
    }

    Ok(())
  }

  /// Increases the current indent level.
  pub fn indent(&mut self) {
    self.indent += 2;
  }

  /// Decreases the current indent level.
  pub fn dedent(&mut self) {
    self.indent -= 2;
  }

  /// Increases the current indent level by the given number of characters.
  pub fn indent_by(&mut self, amt: u8) {
    self.indent += amt;
  }

  /// Decreases the current indent level by the given number of characters.
  pub fn dedent_by(&mut self, amt: u8) {
    self.indent -= amt;
  }

  /// Returns whether the indent level is greater than one.
  pub fn is_nested(&self) -> bool {
    self.indent > 2
  }

  /// Adds a mapping to the source map, if any.
  #[cfg(feature = "sourcemap")]
  #[cfg_attr(docsrs, doc(cfg(feature = "sourcemap")))]
  pub fn add_mapping(&mut self, loc: Location) {
    self.loc = loc;

    if let Some(map) = &mut self.source_map {
      let mut original = OriginalLocation {
        original_line: loc.line,
        original_column: loc.column - 1,
        source: loc.source_index,
        name: None,
      };

      // Remap using input source map if possible.
      if let Some(Some(sm)) = self.source_maps.get_mut(loc.source_index as usize) {
        let mut found_mapping = false;
        if let Some(mapping) = sm.find_closest_mapping(loc.line, loc.column - 1) {
          if let Some(orig) = mapping.original {
            let sources_len = map.get_sources().len();
            let source_index = map.add_source(sm.get_source(orig.source).unwrap());
            let name = orig.name.map(|name| map.add_name(sm.get_name(name).unwrap()));
            original.original_line = orig.original_line;
            original.original_column = orig.original_column;
            original.source = source_index;
            original.name = name;

            if map.get_sources().len() > sources_len {
              let content = sm.get_source_content(orig.source).unwrap().to_owned();
              let _ = map.set_source_content(source_index as usize, &content);
            }

            found_mapping = true;
          }
        }

        if !found_mapping {
          return;
        }
      }

      map.add_mapping(self.line, self.col, Some(original))
    }
  }

  /// Writes a CSS identifier to the underlying destination, escaping it
  /// as appropriate. If the `css_modules` option was enabled, then a hash
  /// is added, and the mapping is added to the CSS module.
  pub fn write_ident(&mut self, ident: &str, handle_css_module: bool) -> Result<(), PrinterError> {
    if handle_css_module {
      if let Some(css_module) = &mut self.css_module {
        let mut dest = IdentifierWriter {
          dest: &mut self.dest,
          col: &mut self.col,
        };
        let mut first = true;
        css_module.config.pattern.write(
          &css_module.hashes[self.loc.source_index as usize],
          &css_module.sources[self.loc.source_index as usize],
          ident,
          if let Some(content_hashes) = &css_module.content_hashes {
            &content_hashes[self.loc.source_index as usize]
          } else {
            ""
          },
          |s| {
            if first {
              first = false;
              serialize_identifier(s, &mut dest)
            } else {
              serialize_name(s, &mut dest)
            }
          },
        )?;

        css_module.add_local(&ident, &ident, self.loc.source_index);
        return Ok(());
      }
    }

    serialize_identifier(ident, self)?;
    Ok(())
  }

  pub(crate) fn write_dashed_ident(&mut self, ident: &str, is_declaration: bool) -> Result<(), PrinterError> {
    self.write_str("--")?;

    match &mut self.css_module {
      Some(css_module) if css_module.config.dashed_idents => {
        let mut dest = IdentifierWriter {
          dest: &mut self.dest,
          col: &mut self.col,
        };
        css_module.config.pattern.write(
          &css_module.hashes[self.loc.source_index as usize],
          &css_module.sources[self.loc.source_index as usize],
          &ident[2..],
          if let Some(content_hashes) = &css_module.content_hashes {
            &content_hashes[self.loc.source_index as usize]
          } else {
            ""
          },
          |s| serialize_name(s, &mut dest),
        )?;

        if is_declaration {
          css_module.add_dashed(ident, self.loc.source_index);
        }
      }
      _ => {
        serialize_name(&ident[2..], self)?;
      }
    }

    Ok(())
  }

  /// Returns an error of the given kind at the provided location in the current source file.
  pub fn error(&self, kind: PrinterErrorKind, loc: crate::dependencies::Location) -> Error<PrinterErrorKind> {
    Error {
      kind,
      loc: Some(ErrorLocation {
        filename: self.filename().into(),
        line: loc.line - 1,
        column: loc.column,
      }),
    }
  }

  pub(crate) fn with_context<T, U, F: FnOnce(&mut Printer<'a, 'c, W>) -> Result<T, U>>(
    &mut self,
    selectors: &SelectorList,
    f: F,
  ) -> Result<T, U> {
    let parent = std::mem::take(&mut self.context);
    let ctx = StyleContext {
      selectors: unsafe { std::mem::transmute(selectors) },
      parent,
    };

    // I can't figure out what lifetime to use here to convince the compiler that
    // the reference doesn't live beyond the function call.
    self.context = Some(unsafe { std::mem::transmute(&ctx) });
    let res = f(self);
    self.context = parent;
    res
  }

  pub(crate) fn with_cleared_context<T, U, F: FnOnce(&mut Printer<'a, 'c, W>) -> Result<T, U>>(
    &mut self,
    f: F,
  ) -> Result<T, U> {
    let parent = std::mem::take(&mut self.context);
    let res = f(self);
    self.context = parent;
    res
  }

  pub(crate) fn with_parent_context<T, U, F: FnOnce(&mut Printer<'a, 'c, W>) -> Result<T, U>>(
    &mut self,
    f: F,
  ) -> Result<T, U> {
    let parent = std::mem::take(&mut self.context);
    if let Some(parent) = parent {
      self.context = parent.parent;
    }
    let res = f(self);
    self.context = parent;
    res
  }

  pub(crate) fn context(&self) -> Option<&'a StyleContext<'a, 'c>> {
    self.context.clone()
  }
}

impl<'a, 'c, W: std::fmt::Write + Sized> std::fmt::Write for Printer<'a, 'c, W> {
  fn write_str(&mut self, s: &str) -> std::fmt::Result {
    self.col += utf16_len(s) as u32;
    self.dest.write_str(s)
  }
}

// CSS module pattern segments must be counted after escaping, which can expand identifiers.
struct IdentifierWriter<'a, W> {
  dest: &'a mut W,
  col: &'a mut u32,
}

impl<W: std::fmt::Write> std::fmt::Write for IdentifierWriter<'_, W> {
  fn write_str(&mut self, s: &str) -> std::fmt::Result {
    *self.col += utf16_len(s) as u32;
    self.dest.write_str(s)
  }
}

fn utf16_len(s: &str) -> usize {
  // Each non-continuation byte starts one scalar. Four-byte scalars need a second UTF-16 code unit.
  s.bytes().map(|b| usize::from(b & 0xc0 != 0x80) + usize::from(b >= 0xf0)).sum()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_utf16_len_counts_code_units() {
    for (s, expected) in [("", 0), ("ascii", 5), ("é", 1), ("❯", 1), ("😀", 2), ("aé❯😀", 5)] {
      assert_eq!(utf16_len(s), expected, "{s:?}");
    }
  }

  #[test]
  fn test_writes_advance_utf16_columns_and_reset_at_newlines() {
    let mut output = String::new();
    let mut printer = Printer::new(&mut output, PrinterOptions::default());
    printer.write_str("é❯😀").unwrap();
    assert_eq!((printer.line, printer.col), (0, 4));
    printer.write_char('😀').unwrap();
    assert_eq!((printer.line, printer.col), (0, 6));
    std::fmt::Write::write_str(&mut printer, "é😀").unwrap();
    assert_eq!((printer.line, printer.col), (0, 9));
    printer.write_str_with_newlines("é😀").unwrap();
    assert_eq!((printer.line, printer.col), (0, 12));
    printer.write_str_with_newlines("é\n😀\né😀").unwrap();
    assert_eq!((printer.line, printer.col), (2, 3));
    printer.write_str_with_newlines("😀\n").unwrap();
    assert_eq!((printer.line, printer.col), (3, 0));
    printer.write_char('😀').unwrap();
    printer.write_char('\n').unwrap();
    assert_eq!((printer.line, printer.col), (4, 0));
  }

  #[test]
  #[cfg(feature = "sourcemap")]
  fn test_source_map_matches_columns_after_unicode_and_identifier_escapes() {
    use crate::stylesheet::{MinifyOptions, ParserOptions, StyleSheet};

    for (source, css_modules, expected) in [
      (
        ".a { content: 'é❯😀'; }\n.b { color: red; }",
        false,
        ".a{content:\"é❯😀\"}.b{color:red}",
      ),
      (
        ".\\31 é😀 { --\\+❯😀: 1; }\n.b { color: red; }",
        true,
        ".\\31 é😀{--\\+❯😀:1}.b{color:red}",
      ),
    ] {
      let mut stylesheet = StyleSheet::parse(
        source,
        ParserOptions {
          css_modules: css_modules.then(|| crate::css_modules::Config {
            pattern: crate::css_modules::Pattern::parse("[local]").unwrap(),
            dashed_idents: true,
            ..Default::default()
          }),
          ..Default::default()
        },
      )
      .unwrap();
      stylesheet.minify(MinifyOptions::default()).unwrap();
      let mut map = SourceMap::new("/");
      let output = stylesheet
        .to_css(PrinterOptions {
          minify: true,
          source_map: Some(&mut map),
          ..Default::default()
        })
        .unwrap()
        .code;
      assert_eq!(output, expected);

      let column = expected[..expected.find(".b").unwrap()].encode_utf16().count() as u32;
      let mapping = map.find_closest_mapping(0, column).unwrap();
      assert_eq!((mapping.generated_line, mapping.generated_column), (0, column));
      let original = mapping.original.unwrap();
      assert_eq!((original.original_line, original.original_column), (1, 0));
    }
  }
}
