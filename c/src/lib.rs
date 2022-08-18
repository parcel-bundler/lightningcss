use std::mem::ManuallyDrop;
use std::os::raw::c_char;

use parcel_css::stylesheet::{ParserOptions, PrinterOptions, StyleSheet};

#[no_mangle]
pub extern "C" fn stylesheet_parse<'i, 'o>(source: *const c_char, len: usize) -> *mut StyleSheet<'i, 'o> {
  let slice = unsafe { std::slice::from_raw_parts(source as *const u8, len) };
  let code = unsafe { std::str::from_utf8_unchecked(slice) };
  // TODO: error handling
  Box::into_raw(Box::new(StyleSheet::parse(code, ParserOptions::default()).unwrap()))
}

#[no_mangle]
pub extern "C" fn stylesheet_to_css(stylesheet: *mut StyleSheet) -> RawString {
  let stylesheet = unsafe { stylesheet.as_mut() }.unwrap();
  let res = stylesheet
    .to_css(PrinterOptions {
      minify: true,
      ..PrinterOptions::default()
    })
    .unwrap(); // TODO: error handling
  res.code.into()
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
  cap: usize,
}

impl From<String> for RawString {
  fn from(string: String) -> RawString {
    let mut string = ManuallyDrop::new(string);
    let ptr = string.as_mut_ptr();
    RawString {
      text: ptr as *mut c_char,
      len: string.len(),
      cap: string.capacity(),
    }
  }
}

impl Drop for RawString {
  fn drop(&mut self) {
    if self.text.is_null() {
      return;
    }
    let s = unsafe { String::from_raw_parts(self.text as *mut u8, self.len, self.cap) };
    self.text = std::ptr::null_mut();
    drop(s)
  }
}

#[no_mangle]
pub extern "C" fn raw_string_free(s: RawString) {
  drop(s)
}
