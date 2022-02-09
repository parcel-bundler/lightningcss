use cssparser::CowRcStr;
use std::borrow::Cow;
use std::marker::PhantomData;
use std::rc::Rc;
use std::slice;
use std::str;

// We need to convert CowRcStr<'i> to Cow<'i, str>, but CowRcStr<'i>
// doesn't provide a method to do this without cloning. Instead, we
// use some unsafe tricks to access the internal fields. This struct
// must be identical to CowRcStr so we can transmutate it.
struct LocalCowRcStr<'a> {
  ptr: &'static (),
  borrowed_len_or_max: usize,
  phantom: PhantomData<Result<&'a str, Rc<String>>>,
}

#[inline]
pub fn to_cow<'i>(s: CowRcStr<'i>) -> Cow<'i, str> {
  let local = unsafe { std::mem::transmute::<&CowRcStr<'i>, &LocalCowRcStr<'i>>(&s) };
  if local.borrowed_len_or_max == usize::MAX {
    let ptr = local.ptr as *const () as *mut String;
    Cow::Owned(std::mem::take(unsafe { &mut *ptr }))
  } else {
    unsafe {
      Cow::Borrowed(str::from_utf8_unchecked(slice::from_raw_parts(
        local.ptr as *const () as *const u8,
        local.borrowed_len_or_max,
      )))
    }
  }
}
