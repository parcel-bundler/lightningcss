#[cfg(target_os = "macos")]
#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

use napi::{CallContext, JsObject, JsUnknown};
use napi_derive::{js_function, module_exports};

#[js_function(1)]
fn transform(ctx: CallContext) -> napi::Result<JsUnknown> {
  lightningcss_napi::transform(ctx)
}

#[js_function(1)]
fn transform_style_attribute(ctx: CallContext) -> napi::Result<JsUnknown> {
  lightningcss_napi::transform_style_attribute(ctx)
}

#[js_function(1)]
pub fn bundle(ctx: CallContext) -> napi::Result<JsUnknown> {
  lightningcss_napi::bundle(ctx)
}

#[cfg(not(target_arch = "wasm32"))]
#[js_function(1)]
pub fn bundle_async(ctx: CallContext) -> napi::Result<JsObject> {
  lightningcss_napi::bundle_async(ctx)
}

#[cfg_attr(not(target_arch = "wasm32"), module_exports)]
fn init(mut exports: JsObject) -> napi::Result<()> {
  exports.create_named_method("transform", transform)?;
  exports.create_named_method("transformStyleAttribute", transform_style_attribute)?;
  exports.create_named_method("bundle", bundle)?;
  #[cfg(not(target_arch = "wasm32"))]
  {
    exports.create_named_method("bundleAsync", bundle_async)?;
  }

  Ok(())
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub fn register_module() {
  unsafe fn register(raw_env: napi::sys::napi_env, raw_exports: napi::sys::napi_value) -> napi::Result<()> {
    use napi::{Env, JsObject, NapiValue};

    let env = Env::from_raw(raw_env);
    let exports = JsObject::from_raw_unchecked(raw_env, raw_exports);
    init(exports)
  }

  napi::bindgen_prelude::register_module_exports(register)
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn napi_wasm_malloc(size: usize) -> *mut u8 {
  use std::alloc::{alloc, Layout};
  use std::mem;

  let align = mem::align_of::<usize>();
  if let Ok(layout) = Layout::from_size_align(size, align) {
    unsafe {
      if layout.size() > 0 {
        let ptr = alloc(layout);
        if !ptr.is_null() {
          return ptr;
        }
      } else {
        return align as *mut u8;
      }
    }
  }

  std::process::abort();
}
