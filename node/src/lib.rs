#[cfg(target_os = "macos")]
#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

use std::collections::HashSet;
use serde::{Serialize, Deserialize};
use parcel_css::stylesheet::{StyleSheet, StyleAttribute, ParserOptions, PrinterOptions, MinifyOptions, PseudoClasses};
use parcel_css::targets::Browsers;
use parcel_css::css_modules::CssModuleExports;
use parcel_css::dependencies::Dependency;
use parcel_css::error::{ParserError, PrinterError, MinifyError};

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
use napi_derive::{js_function, module_exports};
#[cfg(not(target_arch = "wasm32"))]
use napi::{CallContext, JsObject, JsUnknown};

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SourceMapJson<'a> {
  version: u8,
  mappings: String,
  sources: &'a Vec<String>,
  sources_content: &'a Vec<String>,
  names: &'a Vec<String>
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct TransformResult {
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
  #[serde(with = "serde_bytes")]
  map: Option<Vec<u8>>,
  exports: Option<CssModuleExports>,
  dependencies: Option<Vec<Dependency>>
}

#[cfg(not(target_arch = "wasm32"))]
#[js_function(1)]
fn transform(ctx: CallContext) -> napi::Result<JsUnknown> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: Config = ctx.env.from_js_value(opts)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) }; 
  let res = compile(code, &config);

  match res {
    Ok(res) => ctx.env.to_js_value(&res),
    Err(err) => err.throw(ctx, Some(config.filename), code)
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
    Ok(res) => ctx.env.to_js_value(&res),
    Err(err) => err.throw(ctx, None, code)
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[module_exports]
fn init(mut exports: JsObject) -> napi::Result<()> {
  exports.create_named_method("transform", transform)?;
  exports.create_named_method("transformStyleAttribute", transform_style_attribute)?;

  Ok(())
}

// ---------------------------------------------

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Config {
  pub filename: String,
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>,
  pub source_map: Option<bool>,
  pub drafts: Option<Drafts>,
  pub css_modules: Option<bool>,
  pub analyze_dependencies: Option<bool>,
  pub pseudo_classes: Option<OwnedPseudoClasses>,
  pub unused_symbols: Option<HashSet<String>>
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct OwnedPseudoClasses {
  pub hover: Option<String>,
  pub active: Option<String>,
  pub focus: Option<String>,
  pub focus_visible: Option<String>,
  pub focus_within: Option<String>
}

impl<'a> Into<PseudoClasses<'a>> for &'a OwnedPseudoClasses {
  fn into(self) -> PseudoClasses<'a> {
    PseudoClasses {
      hover: self.hover.as_deref(),
      active: self.active.as_deref(),
      focus: self.focus.as_deref(),
      focus_visible: self.focus_visible.as_deref(),
      focus_within: self.focus_within.as_deref()
    }
  }
}

#[derive(Serialize, Debug, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct Drafts {
  #[serde(default)]
  nesting: bool,
  #[serde(default)]
  custom_media: bool
}

fn compile<'i>(code: &'i str, config: &Config) -> Result<TransformResult, CompileError<'i>> {
  let drafts = config.drafts.as_ref();
  let mut stylesheet = StyleSheet::parse(config.filename.clone(), &code, ParserOptions {
    nesting: matches!(drafts, Some(d) if d.nesting),
    custom_media: matches!(drafts, Some(d) if d.custom_media),
    css_modules: config.css_modules.unwrap_or(false)
  })?;
  stylesheet.minify(MinifyOptions {
    targets: config.targets,
    unused_symbols: config.unused_symbols.clone().unwrap_or_default()
  })?;
  let res = stylesheet.to_css(PrinterOptions {
    minify: config.minify.unwrap_or(false),
    source_map: config.source_map.unwrap_or(false),
    targets: config.targets,
    analyze_dependencies: config.analyze_dependencies.unwrap_or(false),
    pseudo_classes: config.pseudo_classes.as_ref().map(|p| p.into())
  })?;

  let map = if let Some(mut source_map) = res.source_map {
    source_map.set_source_content(0, code)?;
    let mut vlq_output: Vec<u8> = Vec::new();
    source_map.write_vlq(&mut vlq_output)?;

    let sm = SourceMapJson {
      version: 3,
      mappings: unsafe { String::from_utf8_unchecked(vlq_output) },
      sources: source_map.get_sources(),
      sources_content: source_map.get_sources_content(),
      names: source_map.get_names()
    };

    serde_json::to_vec(&sm).ok()
  } else {
    None
  };

  Ok(TransformResult {
    code: res.code.into_bytes(),
    map,
    exports: res.exports,
    dependencies: res.dependencies
  })
}

#[derive(Serialize, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AttrConfig {
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>,
  pub analyze_dependencies: Option<bool>
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct AttrResult {
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
  dependencies: Option<Vec<Dependency>>
}

fn compile_attr<'i>(code: &'i str, config: &AttrConfig) -> Result<AttrResult, CompileError<'i>> {
  let mut attr = StyleAttribute::parse(&code)?;
  attr.minify(MinifyOptions {
    targets: config.targets,
    ..MinifyOptions::default()
  });
  let res = attr.to_css(PrinterOptions {
    minify: config.minify.unwrap_or(false),
    source_map: false,
    targets: config.targets,
    analyze_dependencies: config.analyze_dependencies.unwrap_or(false),
    pseudo_classes: None
  })?;
  Ok(AttrResult {
    code: res.code.into_bytes(),
    dependencies: res.dependencies
  })
}

enum CompileError<'i> {
  ParseError(cssparser::ParseError<'i, ParserError<'i>>),
  MinifyError(MinifyError),
  PrinterError(PrinterError),
  SourceMapError(parcel_sourcemap::SourceMapError)
}

impl<'i> CompileError<'i> {
  fn reason(&self) -> String {
    match self {
      CompileError::ParseError(e) => {
        match &e.kind {
          cssparser::ParseErrorKind::Basic(b) => {
            use cssparser::BasicParseErrorKind::*;
            match b {
              AtRuleBodyInvalid => "Invalid at rule body".into(),
              EndOfInput => "Unexpected end of input".into(),
              AtRuleInvalid(name) => format!("Unknown at rule: @{}", name),
              QualifiedRuleInvalid => "Invalid qualified rule".into(),
              UnexpectedToken(token) => format!("Unexpected token {:?}", token)
            }
          },
          cssparser::ParseErrorKind::Custom(e) => e.reason()
        }
      }
      CompileError::MinifyError(err) => err.reason(),
      CompileError::PrinterError(err) => err.reason(),
      _ => "Unknown error".into()
    }
  }

  #[cfg(not(target_arch = "wasm32"))]
  fn throw(self, ctx: CallContext, filename: Option<String>, code: &str) -> napi::Result<JsUnknown> {
    match &self {
      CompileError::ParseError(e) => {
        throw_syntax_error(ctx, filename, code, self.reason(), &e.location)
      },
      CompileError::PrinterError(PrinterError::InvalidComposesSelector(loc)) |
      CompileError::PrinterError(PrinterError::InvalidComposesNesting(loc)) => {
        throw_syntax_error(ctx, filename, code, self.reason(), loc)
      }
      CompileError::MinifyError(err) => {
        throw_syntax_error(ctx, filename, code, self.reason(), &err.loc())
      }
      _ => Err(self.into())
    }
  }
}

#[cfg(not(target_arch = "wasm32"))]
fn throw_syntax_error(ctx: CallContext, filename: Option<String>, code: &str, message: String, loc: &cssparser::SourceLocation) -> napi::Result<JsUnknown> {
  // Generate an error with location information.
  let syntax_error = ctx.env.get_global()?
    .get_named_property::<napi::JsFunction>("SyntaxError")?;
  let reason = ctx.env.create_string_from_std(message)?;
  let line = ctx.env.create_int32((loc.line + 1) as i32)?;
  let col = ctx.env.create_int32(loc.column as i32)?;
  let mut obj = syntax_error.new(&[reason])?;
  if let Some(filename) = filename {
    let filename = ctx.env.create_string_from_std(filename)?;
    obj.set_named_property("fileName", filename)?;
  }
  let source = ctx.env.create_string(code)?;
  obj.set_named_property("source", source)?;
  let mut loc = ctx.env.create_object()?;
  loc.set_named_property("line", line)?;
  loc.set_named_property("column", col)?;
  obj.set_named_property("loc", loc)?;
  ctx.env.throw(obj)?;
  Ok(ctx.env.get_undefined()?.into_unknown())
}

impl<'i> From<cssparser::ParseError<'i, ParserError<'i>>> for CompileError<'i> {
  fn from(e: cssparser::ParseError<'i, ParserError<'i>>) -> CompileError<'i> {
    CompileError::ParseError(e)
  }
}

impl<'i> From<MinifyError> for CompileError<'i> {
  fn from(err: MinifyError) -> CompileError<'i> {
    CompileError::MinifyError(err)
  }
}

impl<'i> From<PrinterError> for CompileError<'i> {
  fn from(err: PrinterError) -> CompileError<'i> {
    CompileError::PrinterError(err)
  }
}

impl<'i> From<parcel_sourcemap::SourceMapError> for CompileError<'i> {
  fn from(e: parcel_sourcemap::SourceMapError) -> CompileError<'i> {
    CompileError::SourceMapError(e)
  }
}

#[cfg(not(target_arch = "wasm32"))]
impl<'i> From<CompileError<'i>> for napi::Error {
  fn from(e: CompileError) -> napi::Error {
    match e {
      CompileError::SourceMapError(e) => e.into(),
      _ => napi::Error::new(napi::Status::GenericFailure, e.reason())
    }
  }
}

#[cfg(target_arch = "wasm32")]
impl<'i> From<CompileError<'i>> for wasm_bindgen::JsValue {
  fn from(e: CompileError) -> wasm_bindgen::JsValue {
    match e {
      CompileError::SourceMapError(e) => e.into(),
      _ => js_sys::Error::new(&e.reason()).into()
    }
  }
}
