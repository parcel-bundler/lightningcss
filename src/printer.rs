//! CSS serialization and source map generation.

use crate::css_modules::CssModule;
use crate::dependencies::{Dependency, DependencyOptions};
use crate::error::{Error, ErrorLocation, PrinterError, PrinterErrorKind};
use crate::rules::{Location, StyleContextPtr};
use crate::targets::{Targets, TargetsWithSupportsScope};
use crate::vendor_prefix::VendorPrefix;
use cssparser::{serialize_identifier, serialize_name};
#[cfg(feature = "sourcemap")]
use parcel_sourcemap::SourceMap as ParcelSourceMap;
#[cfg(not(feature = "custom_sourcemap"))]
use std::marker::PhantomData;

/// An original source location referenced by a generated source map mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OriginalLocation {
  /// The zero-based line number in the original source.
  pub original_line: u32,
  /// The zero-based column number in the original source.
  pub original_column: u32,
  /// The source index in the source map.
  pub source: u32,
  /// The optional name index in the source map.
  pub name: Option<u32>,
}

/// A source map implementation that can receive generated mappings and
/// provide input mappings for remapping.
#[cfg(feature = "custom_sourcemap")]
pub trait SourceMap {
  /// Adds a source path and returns its source index.
  fn add_source(&mut self, source: &str) -> u32;

  /// Adds a symbol name and returns its name index.
  fn add_name(&mut self, name: &str) -> u32;

  /// Sets the original content for a source index.
  fn set_source_content(&mut self, source_index: u32, source_content: &str);

  /// Adds a mapping from a generated position to an optional original location.
  fn add_mapping(&mut self, generated_line: u32, generated_column: u32, original: Option<OriginalLocation>);

  /// Reads an inline source map data URL.
  fn from_data_url(source_root: &str, data_url: &str) -> Option<Self>
  where
    Self: Sized;

  /// Finds the closest original location for a generated position.
  fn find_closest_mapping(&mut self, line: u32, column: u32) -> Option<OriginalLocation>;

  /// Returns the source path for a source index.
  fn get_source(&self, source_index: u32) -> Option<&str>;

  /// Returns the symbol name for a name index.
  fn get_name(&self, name_index: u32) -> Option<&str>;

  /// Returns the source content for a source index.
  fn get_source_content(&self, source_index: u32) -> Option<&str>;
}

/// A source map placeholder used when custom source maps are disabled.
#[cfg(not(feature = "custom_sourcemap"))]
pub trait SourceMap {}

#[cfg(not(feature = "custom_sourcemap"))]
impl SourceMap for () {}

#[cfg(feature = "custom_sourcemap")]
impl SourceMap for () {
  fn add_source(&mut self, _source: &str) -> u32 {
    unreachable!()
  }

  fn add_name(&mut self, _name: &str) -> u32 {
    unreachable!()
  }

  fn set_source_content(&mut self, _source_index: u32, _source_content: &str) {
    unreachable!()
  }

  fn add_mapping(&mut self, _generated_line: u32, _generated_column: u32, _original: Option<OriginalLocation>) {
    unreachable!()
  }

  fn from_data_url(_source_root: &str, _data_url: &str) -> Option<Self> {
    unreachable!()
  }

  fn find_closest_mapping(&mut self, _line: u32, _column: u32) -> Option<OriginalLocation> {
    unreachable!()
  }

  fn get_source(&self, _source_index: u32) -> Option<&str> {
    unreachable!()
  }

  fn get_name(&self, _name_index: u32) -> Option<&str> {
    unreachable!()
  }

  fn get_source_content(&self, _source_index: u32) -> Option<&str> {
    unreachable!()
  }
}

#[cfg(feature = "custom_sourcemap")]
impl<T: SourceMap + ?Sized> SourceMap for &mut T {
  fn add_source(&mut self, source: &str) -> u32 {
    (**self).add_source(source)
  }

  fn add_name(&mut self, name: &str) -> u32 {
    (**self).add_name(name)
  }

  fn set_source_content(&mut self, source_index: u32, source_content: &str) {
    (**self).set_source_content(source_index, source_content)
  }

  fn add_mapping(&mut self, generated_line: u32, generated_column: u32, original: Option<OriginalLocation>) {
    (**self).add_mapping(generated_line, generated_column, original)
  }

  fn from_data_url(source_root: &str, data_url: &str) -> Option<Self>
  where
    Self: Sized,
  {
    let _ = (source_root, data_url);
    None
  }

  fn find_closest_mapping(&mut self, line: u32, column: u32) -> Option<OriginalLocation> {
    (**self).find_closest_mapping(line, column)
  }

  fn get_source(&self, source_index: u32) -> Option<&str> {
    (**self).get_source(source_index)
  }

  fn get_name(&self, name_index: u32) -> Option<&str> {
    (**self).get_name(name_index)
  }

  fn get_source_content(&self, source_index: u32) -> Option<&str> {
    (**self).get_source_content(source_index)
  }
}

#[cfg(feature = "sourcemap")]
#[cfg_attr(docsrs, doc(cfg(feature = "sourcemap")))]
impl SourceMap for ParcelSourceMap {
  fn add_source(&mut self, source: &str) -> u32 {
    ParcelSourceMap::add_source(self, source)
  }

  fn add_name(&mut self, name: &str) -> u32 {
    ParcelSourceMap::add_name(self, name)
  }

  fn set_source_content(&mut self, source_index: u32, source_content: &str) {
    let _ = ParcelSourceMap::set_source_content(self, source_index as usize, source_content);
  }

  fn add_mapping(&mut self, generated_line: u32, generated_column: u32, original: Option<OriginalLocation>) {
    ParcelSourceMap::add_mapping(
      self,
      generated_line,
      generated_column,
      original.map(|original| parcel_sourcemap::OriginalLocation {
        original_line: original.original_line,
        original_column: original.original_column,
        source: original.source,
        name: original.name,
      }),
    );
  }

  fn from_data_url(source_root: &str, data_url: &str) -> Option<Self> {
    ParcelSourceMap::from_data_url(source_root, data_url).ok()
  }

  fn find_closest_mapping(&mut self, line: u32, column: u32) -> Option<OriginalLocation> {
    let mapping = ParcelSourceMap::find_closest_mapping(self, line, column)?;
    mapping.original.map(|original| OriginalLocation {
      original_line: original.original_line,
      original_column: original.original_column,
      source: original.source,
      name: original.name,
    })
  }

  fn get_source(&self, source_index: u32) -> Option<&str> {
    ParcelSourceMap::get_source(self, source_index).ok()
  }

  fn get_name(&self, name_index: u32) -> Option<&str> {
    ParcelSourceMap::get_name(self, name_index).ok()
  }

  fn get_source_content(&self, source_index: u32) -> Option<&str> {
    ParcelSourceMap::get_source_content(self, source_index).ok()
  }
}

/// Options that control how CSS is serialized to a string.
pub struct PrinterOptions<'a> {
  /// Whether to minify the CSS, i.e. remove white space.
  pub minify: bool,
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

impl<'a> Default for PrinterOptions<'a> {
  fn default() -> Self {
    PrinterOptions {
      minify: false,
      project_root: None,
      targets: Targets::default(),
      analyze_dependencies: None,
      pseudo_classes: None,
    }
  }
}

#[allow(missing_docs)]
pub struct PrinterState<'a, 'c> {
  pub(crate) loc: Location,
  pub(crate) generated_line: u32,
  pub(crate) generated_col: u32,
  pub(crate) indent: u8,
  pub(crate) targets: TargetsWithSupportsScope,
  pub(crate) vendor_prefix: VendorPrefix,
  pub(crate) in_calc: bool,
  pub(crate) css_module: Option<CssModule<'a, 'c>>,
  pub(crate) dependencies: Option<Vec<Dependency>>,
  pub(crate) remove_imports: bool,
  pub(crate) context: Option<StyleContextPtr>,
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
/// this to generate a source map if one is provided.
///
/// `Printer` also includes helper functions that assist with writing output
/// that respects options such as `minify`, and `css_modules`.
pub struct Printer<'a, 'c, W, S: SourceMap = ()> {
  pub(crate) sources: Option<&'c Vec<String>>,
  dest: &'a mut W,
  pub(crate) options: PrinterOptions<'a>,
  pub(crate) state: PrinterState<'a, 'c>,
  #[cfg(feature = "custom_sourcemap")]
  pub(crate) source_map: Option<&'a mut S>,
  #[cfg(feature = "custom_sourcemap")]
  pub(crate) source_maps: Vec<Option<S>>,
  #[cfg(not(feature = "custom_sourcemap"))]
  source_map: PhantomData<fn() -> S>,
}

impl<'a, 'c, W: std::fmt::Write + Sized> Printer<'a, 'c, W, ()> {
  /// Create a new Printer wrapping the given destination.
  pub fn new(dest: &'a mut W, options: PrinterOptions<'a>) -> Self {
    Self::new_impl(dest, options)
  }
}

impl<'a, 'c, W: std::fmt::Write + Sized, S: SourceMap> Printer<'a, 'c, W, S> {
  fn new_impl(dest: &'a mut W, options: PrinterOptions<'a>) -> Self {
    let dependencies = if options.analyze_dependencies.is_some() {
      Some(Vec::new())
    } else {
      None
    };
    let remove_imports = matches!(&options.analyze_dependencies, Some(d) if d.remove_imports);
    Printer {
      sources: None,
      dest,
      #[cfg(feature = "custom_sourcemap")]
      source_map: None,
      state: PrinterState {
        loc: Location {
          source_index: 0,
          line: 0,
          column: 1,
        },
        generated_line: 0,
        generated_col: 0,
        indent: 0,
        targets: TargetsWithSupportsScope::new(options.targets),
        vendor_prefix: VendorPrefix::empty(),
        in_calc: false,
        css_module: None,
        dependencies,
        remove_imports,
        context: None,
      },
      options,
      #[cfg(feature = "custom_sourcemap")]
      source_maps: Vec::new(),
      #[cfg(not(feature = "custom_sourcemap"))]
      source_map: PhantomData,
    }
  }

  #[cfg(feature = "custom_sourcemap")]
  /// Attaches a source map writer to this printer.
  pub fn with_source_map<T: SourceMap>(self, source_map: Option<&'a mut T>) -> Printer<'a, 'c, W, T> {
    Printer {
      sources: self.sources,
      dest: self.dest,
      options: self.options,
      state: self.state,
      source_map,
      source_maps: Vec::new(),
    }
  }

  /// Returns the current source filename that is being printed.
  pub fn filename(&self) -> &'c str {
    if let Some(sources) = self.sources {
      if let Some(f) = sources.get(self.state.loc.source_index as usize) {
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
    self.state.generated_col += s.len() as u32;
    self.dest.write_str(s)?;
    Ok(())
  }

  /// Writes a raw string which may contain newlines to the underlying destination.
  pub fn write_str_with_newlines(&mut self, s: &str) -> Result<(), PrinterError> {
    let mut last_line_start: usize = 0;

    for (idx, n) in s.char_indices() {
      if n == '\n' {
        self.state.generated_line += 1;
        self.state.generated_col = 0;

        // Keep track of where the *next* line starts
        last_line_start = idx + 1;
      }
    }

    self.state.generated_col += (s.len() - last_line_start) as u32;
    self.dest.write_str(s)?;
    Ok(())
  }

  /// Write a single character to the underlying destination.
  pub fn write_char(&mut self, c: char) -> Result<(), PrinterError> {
    if c == '\n' {
      self.state.generated_line += 1;
      self.state.generated_col = 0;
    } else {
      self.state.generated_col += 1;
    }
    self.dest.write_char(c)?;
    Ok(())
  }

  /// Writes a single whitespace character, unless the `minify` option is enabled.
  ///
  /// Use `write_char` instead if you wish to force a space character to be written,
  /// regardless of the `minify` option.
  pub fn whitespace(&mut self) -> Result<(), PrinterError> {
    if self.options.minify {
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
    if self.options.minify {
      return Ok(());
    }

    self.write_char('\n')?;
    if self.state.indent > 0 {
      self.write_str(&" ".repeat(self.state.indent as usize))?;
    }

    Ok(())
  }

  /// Increases the current indent level.
  pub fn indent(&mut self) {
    self.state.indent += 2;
  }

  /// Decreases the current indent level.
  pub fn dedent(&mut self) {
    self.state.indent -= 2;
  }

  /// Increases the current indent level by the given number of characters.
  pub fn indent_by(&mut self, amt: u8) {
    self.state.indent += amt;
  }

  /// Decreases the current indent level by the given number of characters.
  pub fn dedent_by(&mut self, amt: u8) {
    self.state.indent -= amt;
  }

  /// Returns whether the indent level is greater than one.
  pub fn is_nested(&self) -> bool {
    self.state.indent > 2
  }

  /// Writes a CSS identifier to the underlying destination, escaping it
  /// as appropriate. If the `css_modules` option was enabled, then a hash
  /// is added, and the mapping is added to the CSS module.
  pub fn write_ident(&mut self, ident: &str, handle_css_module: bool) -> Result<(), PrinterError> {
    if handle_css_module {
      if let Some(css_module) = &mut self.state.css_module {
        let dest = &mut self.dest;
        let mut first = true;
        css_module.config.pattern.write(
          &css_module.hashes[self.state.loc.source_index as usize],
          &css_module.sources[self.state.loc.source_index as usize],
          ident,
          if let Some(content_hashes) = &css_module.content_hashes {
            &content_hashes[self.state.loc.source_index as usize]
          } else {
            ""
          },
          |s| {
            self.state.generated_col += s.len() as u32;
            if first {
              first = false;
              serialize_identifier(s, dest)
            } else {
              serialize_name(s, dest)
            }
          },
        )?;

        css_module.add_local(&ident, &ident, self.state.loc.source_index);
        return Ok(());
      }
    }

    serialize_identifier(ident, self)?;
    Ok(())
  }

  pub(crate) fn write_dashed_ident(&mut self, ident: &str, is_declaration: bool) -> Result<(), PrinterError> {
    self.write_str("--")?;

    match &mut self.state.css_module {
      Some(css_module) if css_module.config.dashed_idents => {
        let dest = &mut self.dest;
        css_module.config.pattern.write(
          &css_module.hashes[self.state.loc.source_index as usize],
          &css_module.sources[self.state.loc.source_index as usize],
          &ident[2..],
          if let Some(content_hashes) = &css_module.content_hashes {
            &content_hashes[self.state.loc.source_index as usize]
          } else {
            ""
          },
          |s| {
            self.state.generated_col += s.len() as u32;
            serialize_name(s, dest)
          },
        )?;

        if is_declaration {
          css_module.add_dashed(ident, self.state.loc.source_index);
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

  pub(crate) fn take_dependencies(&mut self) -> Option<Vec<Dependency>> {
    self.state.dependencies.take()
  }

  /// Adds a mapping to the source map, if any.
  pub fn add_mapping(&mut self, loc: Location) {
    self.state.loc = loc;

    #[cfg(feature = "custom_sourcemap")]
    if let Some(map) = &mut self.source_map {
      #[cfg_attr(not(feature = "sourcemap"), allow(unused_mut))]
      let mut original = OriginalLocation {
        original_line: loc.line,
        original_column: loc.column - 1,
        source: loc.source_index,
        name: None,
      };

      // Remap using input source map if possible.
      if let Some(Some(sm)) = self.source_maps.get_mut(loc.source_index as usize) {
        let mut found_mapping = false;
        if let Some(orig) = sm.find_closest_mapping(loc.line, loc.column - 1) {
          let source_index = map.add_source(sm.get_source(orig.source).unwrap());
          let name = orig.name.map(|name| map.add_name(sm.get_name(name).unwrap()));
          original.original_line = orig.original_line;
          original.original_column = orig.original_column;
          original.source = source_index;
          original.name = name;

          let content = sm.get_source_content(orig.source).unwrap().to_owned();
          map.set_source_content(source_index, &content);

          found_mapping = true;
        }

        if !found_mapping {
          return;
        }
      }

      map.add_mapping(self.state.generated_line, self.state.generated_col, Some(original))
    }
  }
}

impl<'a, 'c, W: std::fmt::Write + Sized, S: SourceMap> std::fmt::Write for Printer<'a, 'c, W, S> {
  fn write_str(&mut self, s: &str) -> std::fmt::Result {
    self.state.generated_col += s.len() as u32;
    self.dest.write_str(s)
  }
}

mod private {
  pub trait Sealed {}
}

/// A printer abstraction used by CSS serialization implementations.
#[allow(missing_docs)]
pub trait PrinterTrait: std::fmt::Write + private::Sealed {
  fn options(&self) -> &PrinterOptions<'_>;
  fn state(&self) -> &PrinterState<'_, '_>;
  fn state_mut(&mut self) -> &mut PrinterState<'_, '_>;
  fn filename(&self) -> &str;
  fn error(&self, kind: PrinterErrorKind, loc: crate::dependencies::Location) -> Error<PrinterErrorKind> {
    Error {
      kind,
      loc: Some(ErrorLocation {
        filename: self.filename().into(),
        line: loc.line - 1,
        column: loc.column,
      }),
    }
  }

  fn whitespace(&mut self) -> Result<(), PrinterError>;
  fn delim(&mut self, c: char, ws_before: bool) -> Result<(), PrinterError>;
  fn newline(&mut self) -> Result<(), PrinterError>;
  fn write_str_with_newlines(&mut self, s: &str) -> Result<(), PrinterError>;
  fn add_mapping(&mut self, loc: Location);
  fn indent(&mut self);
  fn dedent(&mut self);
  fn indent_by(&mut self, amt: u8);
  fn dedent_by(&mut self, amt: u8);
  fn write_ident(&mut self, ident: &str, handle_css_module: bool) -> Result<(), PrinterError>;
  fn write_dashed_ident(&mut self, ident: &str, is_declaration: bool) -> Result<(), PrinterError>;

  fn without_css_module<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T;
}

impl<'a, 'c, W: std::fmt::Write + Sized, S: SourceMap> private::Sealed for Printer<'a, 'c, W, S> {}

impl<'a, 'c, W: std::fmt::Write + Sized, S: SourceMap> PrinterTrait for Printer<'a, 'c, W, S> {
  fn options(&self) -> &PrinterOptions<'_> {
    unsafe { std::mem::transmute::<&PrinterOptions<'a>, &PrinterOptions<'_>>(&self.options) }
  }

  fn state(&self) -> &PrinterState<'_, '_> {
    unsafe { std::mem::transmute::<&PrinterState<'a, 'c>, &PrinterState<'_, '_>>(&self.state) }
  }

  fn state_mut(&mut self) -> &mut PrinterState<'_, '_> {
    unsafe { std::mem::transmute::<&mut PrinterState<'a, 'c>, &mut PrinterState<'_, '_>>(&mut self.state) }
  }

  fn filename(&self) -> &str {
    Printer::filename(self)
  }

  fn whitespace(&mut self) -> Result<(), PrinterError> {
    Printer::whitespace(self)
  }

  fn delim(&mut self, c: char, ws_before: bool) -> Result<(), PrinterError> {
    Printer::delim(self, c, ws_before)
  }

  fn newline(&mut self) -> Result<(), PrinterError> {
    Printer::newline(self)
  }

  fn write_str_with_newlines(&mut self, s: &str) -> Result<(), PrinterError> {
    Printer::write_str_with_newlines(self, s)
  }

  fn add_mapping(&mut self, loc: Location) {
    Printer::add_mapping(self, loc)
  }

  fn indent(&mut self) {
    Printer::indent(self)
  }

  fn dedent(&mut self) {
    Printer::dedent(self)
  }

  fn indent_by(&mut self, amt: u8) {
    Printer::indent_by(self, amt)
  }

  fn dedent_by(&mut self, amt: u8) {
    Printer::dedent_by(self, amt)
  }

  fn write_ident(&mut self, ident: &str, handle_css_module: bool) -> Result<(), PrinterError> {
    Printer::write_ident(self, ident, handle_css_module)
  }

  fn write_dashed_ident(&mut self, ident: &str, is_declaration: bool) -> Result<(), PrinterError> {
    Printer::write_dashed_ident(self, ident, is_declaration)
  }

  fn without_css_module<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
    let css_module = self.state.css_module.take();
    let res = f(self);
    self.state.css_module = css_module;
    res
  }
}
