use std::os::raw::c_char;

use parcel_css::error::{Error, MinifyErrorKind, ParserError, PrinterError};
use parcel_css::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};

pub enum CssError<'i> {
  ParserError(Error<ParserError<'i>>),
  MinifyError(Error<MinifyErrorKind>),
  PrinterError(PrinterError),
}

#[no_mangle]
pub extern "C" fn stylesheet_parse(
  source: *const c_char,
  len: usize,
  error: *mut *mut CssError,
) -> *mut StyleSheet {
  let slice = unsafe { std::slice::from_raw_parts(source as *const u8, len) };
  let code = unsafe { std::str::from_utf8_unchecked(slice) };
  match StyleSheet::parse(code, ParserOptions::default()) {
    Ok(stylesheet) => Box::into_raw(Box::new(stylesheet)),
    Err(err) => unsafe {
      *error = Box::into_raw(Box::new(CssError::ParserError(err)));
      std::ptr::null_mut()
    },
  }
}

#[no_mangle]
pub extern "C" fn stylesheet_transform(stylesheet: *mut StyleSheet, error: *mut *mut CssError) -> bool {
  let stylesheet = unsafe { stylesheet.as_mut() }.unwrap();
  match stylesheet.minify(MinifyOptions::default()) {
    Ok(_) => true,
    Err(err) => unsafe {
      *error = Box::into_raw(Box::new(CssError::MinifyError(err)));
      false
    },
  }
}

#[no_mangle]
pub extern "C" fn stylesheet_to_css(stylesheet: *mut StyleSheet, error: *mut *mut CssError) -> RawString {
  let stylesheet = unsafe { stylesheet.as_mut() }.unwrap();
  let opts = PrinterOptions {
    minify: true,
    ..PrinterOptions::default()
  };

  match stylesheet.to_css(opts) {
    Ok(res) => res.code.into(),
    Err(err) => unsafe {
      *error = Box::into_raw(Box::new(CssError::PrinterError(err)));
      RawString::new()
    },
  }
}

#[no_mangle]
pub extern "C" fn stylesheet_free(stylesheet: *mut StyleSheet) {
  if !stylesheet.is_null() {
    drop(unsafe { Box::from_raw(stylesheet) })
  }
}

#[repr(C)]
pub struct RawString {
  text: *mut c_char,
  len: usize,
}

impl RawString {
  fn new() -> Self {
    RawString {
      text: std::ptr::null_mut(),
      len: 0,
    }
  }
}

impl From<String> for RawString {
  fn from(string: String) -> RawString {
    RawString {
      len: string.len(),
      text: Box::into_raw(string.into_boxed_str()) as *mut c_char,
    }
  }
}

impl Drop for RawString {
  fn drop(&mut self) {
    if self.text.is_null() {
      return;
    }
    drop(unsafe { Box::from_raw(self.text) });
    self.text = std::ptr::null_mut();
  }
}

#[no_mangle]
pub extern "C" fn raw_string_free(s: RawString) {
  drop(s)
}

#[no_mangle]
pub extern "C" fn error_message(error: *mut CssError) -> RawString {
  match unsafe { error.as_ref() } {
    Some(err) => match err {
      CssError::ParserError(err) => err.to_string().into(),
      CssError::MinifyError(err) => err.to_string().into(),
      CssError::PrinterError(err) => err.to_string().into(),
    },
    None => String::new().into(),
  }
}

#[no_mangle]
pub extern "C" fn error_free(error: *mut CssError) {
  if !error.is_null() {
    drop(unsafe { Box::from_raw(error) })
  }
}
