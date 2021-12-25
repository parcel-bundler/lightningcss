#[cfg(target_os = "macos")]
#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

use serde::{Serialize, Deserialize};
use parcel_css::stylesheet::{StyleSheet, StyleAttribute, ParserOptions};
use parcel_css::targets::Browsers;

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
pub fn transform_style_attribute(config_val: JsValue) -> Result<Vec<u8>, JsValue> {
  let config: AttrConfig = from_value(config_val).map_err(JsValue::from)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };
  let res = compile_attr(code, &config)?;
  Ok(res)
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
struct TransformResult {
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
  #[serde(with = "serde_bytes")]
  map: Option<Vec<u8>>
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
    Ok(res) => Ok(ctx.env.create_buffer_with_data(res)?.into_unknown()),
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

#[derive(Serialize, Debug, Deserialize)]
struct Config {
  pub filename: String,
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>,
  pub source_map: Option<bool>,
  pub drafts: Option<Drafts>
}

#[derive(Serialize, Debug, Deserialize, Default)]
struct Drafts {
  nesting: bool
}

fn compile<'i>(code: &'i str, config: &Config) -> Result<TransformResult, CompileError<'i>> {
  let options = config.drafts.as_ref();
  let mut stylesheet = StyleSheet::parse(config.filename.clone(), &code, ParserOptions {
    nesting: match options {
      Some(o) => o.nesting,
      None => false
    }
  })?;
  stylesheet.minify(config.targets); // TODO: should this be conditional?
  let (res, source_map) = stylesheet.to_css(
    config.minify.unwrap_or(false),
    config.source_map.unwrap_or(false),
    config.targets
  )?;

  let map = if let Some(mut source_map) = source_map {
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
    code: res.into_bytes(),
    map
  })
}

#[derive(Serialize, Debug, Deserialize)]
struct AttrConfig {
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>
}

fn compile_attr<'i>(code: &'i str, config: &AttrConfig) -> Result<Vec<u8>, CompileError<'i>> {
  let mut attr = StyleAttribute::parse(&code)?;
  attr.minify(config.targets); // TODO: should this be conditional?
  let res = attr.to_css(config.minify.unwrap_or(false), config.targets)?;
  Ok(res.into_bytes())
}

enum CompileError<'i> {
  ParseError(cssparser::ParseError<'i, ()>),
  PrinterError,
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
          _ => "Unknown error".into()
        }
      }
      CompileError::PrinterError => "Printer error".into(),
      _ => "Unknown error".into()
    }
  }

  #[cfg(not(target_arch = "wasm32"))]
  fn throw(self, ctx: CallContext, filename: Option<String>, code: &str) -> napi::Result<JsUnknown> {
    match &self {
      CompileError::ParseError(e) => {
        // Generate an error with location information.
        let syntax_error = ctx.env.get_global()?
          .get_named_property::<napi::JsFunction>("SyntaxError")?;
        let reason = ctx.env.create_string_from_std(self.reason())?;
        let line = ctx.env.create_int32((e.location.line + 1) as i32)?;
        let col = ctx.env.create_int32(e.location.column as i32)?;
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
      _ => Err(self.into())
    }
  }
}

impl<'i> From<cssparser::ParseError<'i, ()>> for CompileError<'i> {
  fn from(e: cssparser::ParseError<'i, ()>) -> CompileError<'i> {
    CompileError::ParseError(e)
  }
}

impl<'i> From<std::fmt::Error> for CompileError<'i> {
  fn from(_: std::fmt::Error) -> CompileError<'i> {
    CompileError::PrinterError
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
