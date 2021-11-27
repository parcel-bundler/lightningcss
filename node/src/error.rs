pub enum CompileError<'i> {
  ParseError(cssparser::ParseError<'i, ()>),
  PrinterError,
  SourceMapError(parcel_sourcemap::SourceMapError)
}

impl<'i> CompileError<'i> {
  pub fn reason(&self) -> String {
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
