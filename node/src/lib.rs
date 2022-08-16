#[cfg(target_os = "macos")]
#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

use parcel_css::bundler::{BundleErrorKind, Bundler, FileProvider, SourceProvider};
use parcel_css::css_modules::{CssModuleExports, CssModuleReferences, PatternParseError};
use parcel_css::dependencies::Dependency;
use parcel_css::error::{Error, ErrorLocation, MinifyErrorKind, ParserError, PrinterErrorKind};
use parcel_css::stylesheet::{
  MinifyOptions, ParserOptions, PrinterOptions, PseudoClasses, StyleAttribute, StyleSheet,
};
use parcel_css::targets::Browsers;
use parcel_sourcemap::SourceMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::path::Path;
use std::sync::{Arc, RwLock};

// ---------------------------------------------

#[cfg(target_arch = "wasm32")]
use serde_wasm_bindgen::{from_value, Serializer};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn transform(config_val: JsValue) -> Result<JsValue, JsValue> {
  let config: Config = from_value(config_val).map_err(JsValue::from)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };
  let res = compile(code, &config)?;
  let serializer = Serializer::new().serialize_maps_as_objects(true);
  res.serialize(&serializer).map_err(JsValue::from)
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(js_name = "transformStyleAttribute")]
pub fn transform_style_attribute(config_val: JsValue) -> Result<JsValue, JsValue> {
  let config: AttrConfig = from_value(config_val).map_err(JsValue::from)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };
  let res = compile_attr(code, &config)?;
  let serializer = Serializer::new().serialize_maps_as_objects(true);
  res.serialize(&serializer).map_err(JsValue::from)
}

// ---------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
use napi::{CallContext, JsObject, JsUnknown};
#[cfg(not(target_arch = "wasm32"))]
use napi_derive::{js_function, module_exports};

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct TransformResult<'i> {
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
  #[serde(with = "serde_bytes")]
  map: Option<Vec<u8>>,
  exports: Option<CssModuleExports>,
  references: Option<CssModuleReferences>,
  dependencies: Option<Vec<Dependency>>,
  warnings: Vec<Warning<'i>>,
}

#[cfg(not(target_arch = "wasm32"))]
impl<'i> TransformResult<'i> {
  fn into_js(self, ctx: CallContext) -> napi::Result<JsUnknown> {
    // Manually construct buffers so we avoid a copy and work around
    // https://github.com/napi-rs/napi-rs/issues/1124.
    let mut obj = ctx.env.create_object()?;
    let buf = ctx.env.create_buffer_with_data(self.code)?;
    obj.set_named_property("code", buf.into_raw())?;
    obj.set_named_property(
      "map",
      if let Some(map) = self.map {
        let buf = ctx.env.create_buffer_with_data(map)?;
        buf.into_raw().into_unknown()
      } else {
        ctx.env.get_null()?.into_unknown()
      },
    )?;
    obj.set_named_property("exports", ctx.env.to_js_value(&self.exports)?)?;
    obj.set_named_property("references", ctx.env.to_js_value(&self.references)?)?;
    obj.set_named_property("dependencies", ctx.env.to_js_value(&self.dependencies)?)?;
    obj.set_named_property("warnings", ctx.env.to_js_value(&self.warnings)?)?;
    Ok(obj.into_unknown())
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[js_function(1)]
fn transform(ctx: CallContext) -> napi::Result<JsUnknown> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: Config = ctx.env.from_js_value(opts)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };
  let res = compile(code, &config);

  match res {
    Ok(res) => res.into_js(ctx),
    Err(err) => err.throw(ctx, Some(code)),
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[js_function(1)]
fn transform_style_attribute(ctx: CallContext) -> napi::Result<JsUnknown> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: AttrConfig = ctx.env.from_js_value(opts)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };
  let res = compile_attr(code, &config);

  match res {
    Ok(res) => res.into_js(ctx),
    Err(err) => err.throw(ctx, Some(code)),
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[js_function(1)]
fn bundle(ctx: CallContext) -> napi::Result<JsUnknown> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: BundleConfig = ctx.env.from_js_value(opts)?;
  let fs = FileProvider::new();
  let res = compile_bundle(&fs, &config);

  match res {
    Ok(res) => res.into_js(ctx),
    Err(err) => {
      let code = match &err {
        CompileError::ParseError(Error {
          loc: Some(ErrorLocation { filename, .. }),
          ..
        })
        | CompileError::PrinterError(Error {
          loc: Some(ErrorLocation { filename, .. }),
          ..
        })
        | CompileError::MinifyError(Error {
          loc: Some(ErrorLocation { filename, .. }),
          ..
        })
        | CompileError::BundleError(Error {
          loc: Some(ErrorLocation { filename, .. }),
          ..
        }) => Some(fs.read(Path::new(filename))?),
        _ => None,
      };
      err.throw(ctx, code)
    }
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[module_exports]
fn init(mut exports: JsObject) -> napi::Result<()> {
  exports.create_named_method("transform", transform)?;
  exports.create_named_method("transformStyleAttribute", transform_style_attribute)?;
  exports.create_named_method("bundle", bundle)?;

  Ok(())
}

// ---------------------------------------------

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Config {
  pub filename: Option<String>,
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>,
  pub source_map: Option<bool>,
  pub input_source_map: Option<String>,
  pub drafts: Option<Drafts>,
  pub css_modules: Option<CssModulesOption>,
  pub analyze_dependencies: Option<bool>,
  pub pseudo_classes: Option<OwnedPseudoClasses>,
  pub unused_symbols: Option<HashSet<String>>,
  pub error_recovery: Option<bool>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum CssModulesOption {
  Bool(bool),
  Config(CssModulesConfig),
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct CssModulesConfig {
  pattern: Option<String>,
  dashed_idents: Option<bool>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BundleConfig {
  pub filename: String,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>,
  pub source_map: Option<bool>,
  pub drafts: Option<Drafts>,
  pub css_modules: Option<CssModulesOption>,
  pub analyze_dependencies: Option<bool>,
  pub pseudo_classes: Option<OwnedPseudoClasses>,
  pub unused_symbols: Option<HashSet<String>>,
  pub error_recovery: Option<bool>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct OwnedPseudoClasses {
  pub hover: Option<String>,
  pub active: Option<String>,
  pub focus: Option<String>,
  pub focus_visible: Option<String>,
  pub focus_within: Option<String>,
}

impl<'a> Into<PseudoClasses<'a>> for &'a OwnedPseudoClasses {
  fn into(self) -> PseudoClasses<'a> {
    PseudoClasses {
      hover: self.hover.as_deref(),
      active: self.active.as_deref(),
      focus: self.focus.as_deref(),
      focus_visible: self.focus_visible.as_deref(),
      focus_within: self.focus_within.as_deref(),
    }
  }
}

#[derive(Serialize, Debug, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct Drafts {
  #[serde(default)]
  nesting: bool,
  #[serde(default)]
  custom_media: bool,
}

fn compile<'i>(code: &'i str, config: &Config) -> Result<TransformResult<'i>, CompileError<'i>> {
  let drafts = config.drafts.as_ref();
  let warnings = Some(Arc::new(RwLock::new(Vec::new())));

  let filename = config.filename.clone().unwrap_or_default();
  let mut source_map = if config.source_map.unwrap_or_default() {
    let mut sm = SourceMap::new("/");
    sm.add_source(&filename);
    sm.set_source_content(0, code)?;
    Some(sm)
  } else {
    None
  };

  let res = {
    let mut stylesheet = StyleSheet::parse(
      &code,
      ParserOptions {
        filename: filename.clone(),
        nesting: matches!(drafts, Some(d) if d.nesting),
        custom_media: matches!(drafts, Some(d) if d.custom_media),
        css_modules: if let Some(css_modules) = &config.css_modules {
          match css_modules {
            CssModulesOption::Bool(true) => Some(parcel_css::css_modules::Config::default()),
            CssModulesOption::Bool(false) => None,
            CssModulesOption::Config(c) => Some(parcel_css::css_modules::Config {
              pattern: if let Some(pattern) = c.pattern.as_ref() {
                match parcel_css::css_modules::Pattern::parse(pattern) {
                  Ok(p) => p,
                  Err(e) => return Err(CompileError::PatternError(e)),
                }
              } else {
                Default::default()
              },
              dashed_idents: c.dashed_idents.unwrap_or_default(),
            }),
          }
        } else {
          None
        },
        source_index: 0,
        error_recovery: config.error_recovery.unwrap_or_default(),
        warnings: warnings.clone(),
      },
    )?;
    stylesheet.minify(MinifyOptions {
      targets: config.targets,
      unused_symbols: config.unused_symbols.clone().unwrap_or_default(),
    })?;

    stylesheet.to_css(PrinterOptions {
      minify: config.minify.unwrap_or_default(),
      source_map: source_map.as_mut(),
      targets: config.targets,
      analyze_dependencies: config.analyze_dependencies.unwrap_or_default(),
      pseudo_classes: config.pseudo_classes.as_ref().map(|p| p.into()),
    })?
  };

  let map = if let Some(mut source_map) = source_map {
    if let Some(input_source_map) = &config.input_source_map {
      if let Ok(mut sm) = SourceMap::from_json("/", input_source_map) {
        let _ = source_map.extends(&mut sm);
      }
    }

    source_map.to_json(None).ok()
  } else {
    None
  };

  Ok(TransformResult {
    code: res.code.into_bytes(),
    map: map.map(|m| m.into_bytes()),
    exports: res.exports,
    references: res.references,
    dependencies: res.dependencies,
    warnings: warnings.map_or(Vec::new(), |w| {
      Arc::try_unwrap(w)
        .unwrap()
        .into_inner()
        .unwrap()
        .into_iter()
        .map(|w| w.into())
        .collect()
    }),
  })
}

fn compile_bundle<'i>(
  fs: &'i FileProvider,
  config: &BundleConfig,
) -> Result<TransformResult<'i>, CompileError<'i>> {
  let mut source_map = if config.source_map.unwrap_or_default() {
    Some(SourceMap::new("/"))
  } else {
    None
  };
  let warnings = Some(Arc::new(RwLock::new(Vec::new())));
  let res = {
    let drafts = config.drafts.as_ref();
    let parser_options = ParserOptions {
      nesting: matches!(drafts, Some(d) if d.nesting),
      custom_media: matches!(drafts, Some(d) if d.custom_media),
      css_modules: if let Some(css_modules) = &config.css_modules {
        match css_modules {
          CssModulesOption::Bool(true) => Some(parcel_css::css_modules::Config::default()),
          CssModulesOption::Bool(false) => None,
          CssModulesOption::Config(c) => Some(parcel_css::css_modules::Config {
            pattern: if let Some(pattern) = c.pattern.as_ref() {
              match parcel_css::css_modules::Pattern::parse(pattern) {
                Ok(p) => p,
                Err(e) => return Err(CompileError::PatternError(e)),
              }
            } else {
              Default::default()
            },
            dashed_idents: c.dashed_idents.unwrap_or_default(),
          }),
        }
      } else {
        None
      },
      error_recovery: config.error_recovery.unwrap_or_default(),
      warnings: warnings.clone(),
      ..ParserOptions::default()
    };

    let mut bundler = Bundler::new(fs, source_map.as_mut(), parser_options);
    let mut stylesheet = bundler.bundle(Path::new(&config.filename))?;

    stylesheet.minify(MinifyOptions {
      targets: config.targets,
      unused_symbols: config.unused_symbols.clone().unwrap_or_default(),
    })?;

    stylesheet.to_css(PrinterOptions {
      minify: config.minify.unwrap_or_default(),
      source_map: source_map.as_mut(),
      targets: config.targets,
      analyze_dependencies: config.analyze_dependencies.unwrap_or_default(),
      pseudo_classes: config.pseudo_classes.as_ref().map(|p| p.into()),
    })?
  };

  let map = if let Some(source_map) = &mut source_map {
    source_map.to_json(None).ok()
  } else {
    None
  };

  Ok(TransformResult {
    code: res.code.into_bytes(),
    map: map.map(|m| m.into_bytes()),
    exports: res.exports,
    references: res.references,
    dependencies: res.dependencies,
    warnings: warnings.map_or(Vec::new(), |w| {
      Arc::try_unwrap(w)
        .unwrap()
        .into_inner()
        .unwrap()
        .into_iter()
        .map(|w| w.into())
        .collect()
    }),
  })
}

#[derive(Serialize, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AttrConfig {
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  #[serde(default)]
  pub minify: bool,
  #[serde(default)]
  pub analyze_dependencies: bool,
  #[serde(default)]
  pub error_recovery: bool,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AttrResult<'i> {
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
  dependencies: Option<Vec<Dependency>>,
  warnings: Vec<Warning<'i>>,
}

#[cfg(not(target_arch = "wasm32"))]
impl<'i> AttrResult<'i> {
  fn into_js(self, ctx: CallContext) -> napi::Result<JsUnknown> {
    // Manually construct buffers so we avoid a copy and work around
    // https://github.com/napi-rs/napi-rs/issues/1124.
    let mut obj = ctx.env.create_object()?;
    let buf = ctx.env.create_buffer_with_data(self.code)?;
    obj.set_named_property("code", buf.into_raw())?;
    obj.set_named_property("dependencies", ctx.env.to_js_value(&self.dependencies)?)?;
    obj.set_named_property("warnings", ctx.env.to_js_value(&self.warnings)?)?;
    Ok(obj.into_unknown())
  }
}

fn compile_attr<'i>(code: &'i str, config: &AttrConfig) -> Result<AttrResult<'i>, CompileError<'i>> {
  let warnings = if config.error_recovery {
    Some(Arc::new(RwLock::new(Vec::new())))
  } else {
    None
  };
  let res = {
    let mut attr = StyleAttribute::parse(
      &code,
      ParserOptions {
        error_recovery: config.error_recovery,
        warnings: warnings.clone(),
        ..ParserOptions::default()
      },
    )?;
    attr.minify(MinifyOptions {
      targets: config.targets,
      ..MinifyOptions::default()
    });
    attr.to_css(PrinterOptions {
      minify: config.minify,
      source_map: None,
      targets: config.targets,
      analyze_dependencies: config.analyze_dependencies,
      pseudo_classes: None,
    })?
  };
  Ok(AttrResult {
    code: res.code.into_bytes(),
    dependencies: res.dependencies,
    warnings: warnings.map_or(Vec::new(), |w| {
      Arc::try_unwrap(w)
        .unwrap()
        .into_inner()
        .unwrap()
        .into_iter()
        .map(|w| w.into())
        .collect()
    }),
  })
}

enum CompileError<'i> {
  ParseError(Error<ParserError<'i>>),
  MinifyError(Error<MinifyErrorKind>),
  PrinterError(Error<PrinterErrorKind>),
  SourceMapError(parcel_sourcemap::SourceMapError),
  BundleError(Error<BundleErrorKind<'i>>),
  PatternError(PatternParseError),
}

impl<'i> std::fmt::Display for CompileError<'i> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      CompileError::ParseError(err) => err.kind.fmt(f),
      CompileError::MinifyError(err) => err.kind.fmt(f),
      CompileError::PrinterError(err) => err.kind.fmt(f),
      CompileError::BundleError(err) => err.kind.fmt(f),
      CompileError::PatternError(err) => err.fmt(f),
      CompileError::SourceMapError(err) => write!(f, "{}", err.to_string()), // TODO: switch to `fmt::Display` once parcel_sourcemap supports this
    }
  }
}

impl<'i> CompileError<'i> {
  #[cfg(not(target_arch = "wasm32"))]
  fn throw(self, ctx: CallContext, code: Option<&str>) -> napi::Result<JsUnknown> {
    let reason = self.to_string();
    let data = match &self {
      CompileError::ParseError(Error { kind, .. }) => ctx.env.to_js_value(kind)?,
      CompileError::PrinterError(Error { kind, .. }) => ctx.env.to_js_value(kind)?,
      CompileError::MinifyError(Error { kind, .. }) => ctx.env.to_js_value(kind)?,
      CompileError::BundleError(Error { kind, .. }) => ctx.env.to_js_value(kind)?,
      _ => ctx.env.get_null()?.into_unknown(),
    };

    match self {
      CompileError::ParseError(Error { loc, .. })
      | CompileError::PrinterError(Error { loc, .. })
      | CompileError::MinifyError(Error { loc, .. })
      | CompileError::BundleError(Error { loc, .. }) => {
        // Generate an error with location information.
        let syntax_error = ctx.env.get_global()?.get_named_property::<napi::JsFunction>("SyntaxError")?;
        let reason = ctx.env.create_string_from_std(reason)?;
        let mut obj = syntax_error.new_instance(&[reason])?;
        if let Some(loc) = loc {
          let line = ctx.env.create_int32((loc.line + 1) as i32)?;
          let col = ctx.env.create_int32(loc.column as i32)?;
          let filename = ctx.env.create_string_from_std(loc.filename)?;
          obj.set_named_property("fileName", filename)?;
          if let Some(code) = code {
            let source = ctx.env.create_string(code)?;
            obj.set_named_property("source", source)?;
          }
          let mut loc = ctx.env.create_object()?;
          loc.set_named_property("line", line)?;
          loc.set_named_property("column", col)?;
          obj.set_named_property("loc", loc)?;
        }
        obj.set_named_property("data", data)?;
        ctx.env.throw(obj)?;
        Ok(ctx.env.get_undefined()?.into_unknown())
      }
      _ => Err(self.into()),
    }
  }
}

impl<'i> From<Error<ParserError<'i>>> for CompileError<'i> {
  fn from(e: Error<ParserError<'i>>) -> CompileError<'i> {
    CompileError::ParseError(e)
  }
}

impl<'i> From<Error<MinifyErrorKind>> for CompileError<'i> {
  fn from(err: Error<MinifyErrorKind>) -> CompileError<'i> {
    CompileError::MinifyError(err)
  }
}

impl<'i> From<Error<PrinterErrorKind>> for CompileError<'i> {
  fn from(err: Error<PrinterErrorKind>) -> CompileError<'i> {
    CompileError::PrinterError(err)
  }
}

impl<'i> From<parcel_sourcemap::SourceMapError> for CompileError<'i> {
  fn from(e: parcel_sourcemap::SourceMapError) -> CompileError<'i> {
    CompileError::SourceMapError(e)
  }
}

impl<'i> From<Error<BundleErrorKind<'i>>> for CompileError<'i> {
  fn from(e: Error<BundleErrorKind<'i>>) -> CompileError<'i> {
    CompileError::BundleError(e)
  }
}

#[cfg(not(target_arch = "wasm32"))]
impl<'i> From<CompileError<'i>> for napi::Error {
  fn from(e: CompileError) -> napi::Error {
    match e {
      CompileError::SourceMapError(e) => napi::Error::from_reason(e.to_string()),
      CompileError::PatternError(e) => napi::Error::from_reason(e.to_string()),
      _ => napi::Error::new(napi::Status::GenericFailure, e.to_string()),
    }
  }
}

#[cfg(target_arch = "wasm32")]
impl<'i> From<CompileError<'i>> for wasm_bindgen::JsValue {
  fn from(e: CompileError) -> wasm_bindgen::JsValue {
    match e {
      CompileError::SourceMapError(e) => js_sys::Error::new(&e.to_string()).into(),
      CompileError::PatternError(e) => js_sys::Error::new(&e.to_string()).into(),
      _ => js_sys::Error::new(&e.to_string()).into(),
    }
  }
}

#[derive(Serialize)]
struct Warning<'i> {
  message: String,
  #[serde(flatten)]
  data: ParserError<'i>,
  loc: Option<ErrorLocation>,
}

impl<'i> From<Error<ParserError<'i>>> for Warning<'i> {
  fn from(mut e: Error<ParserError<'i>>) -> Self {
    // Convert to 1-based line numbers.
    if let Some(loc) = &mut e.loc {
      loc.line += 1;
    }
    Warning {
      message: e.kind.to_string(),
      data: e.kind,
      loc: e.loc,
    }
  }
}
