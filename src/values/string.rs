//! Types used to represent strings.

use cssparser::CowRcStr;
#[cfg(feature = "serde")]
use serde::{de::Visitor, Deserialize, Deserializer};
use serde::{Serialize, Serializer};
use std::borrow::Borrow;
use std::cmp;
use std::fmt;
use std::hash;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use std::slice;
use std::str;
use std::sync::Arc;

// We cannot store CowRcStr from cssparser directly because it is not threadsafe (due to Rc).
// CowArcStr is exactly the same, but uses Arc instead of Rc. We could use Cow<str> instead,
// but real-world benchmarks show a performance regression, likely due to the larger memory
// footprint.
//
// In order to convert between CowRcStr and CowArcStr without cloning, we use some unsafe
// tricks to access the internal fields. LocalCowRcStr must be exactly the same as CowRcStr
// so we can transmutate between them.
struct LocalCowRcStr<'a> {
  ptr: &'static (),
  borrowed_len_or_max: usize,
  phantom: PhantomData<Result<&'a str, Rc<String>>>,
}

/// A string that is either shared (heap-allocated and atomically reference-counted)
/// or borrowed from the input CSS source code.
pub struct CowArcStr<'a> {
  ptr: &'static (),
  borrowed_len_or_max: usize,
  phantom: PhantomData<Result<&'a str, Arc<String>>>,
}

impl<'a> From<CowRcStr<'a>> for CowArcStr<'a> {
  #[inline]
  fn from(s: CowRcStr<'a>) -> Self {
    (&s).into()
  }
}

impl<'a> From<&CowRcStr<'a>> for CowArcStr<'a> {
  #[inline]
  fn from(s: &CowRcStr<'a>) -> Self {
    let local = unsafe { std::mem::transmute::<&CowRcStr<'a>, &LocalCowRcStr<'a>>(&s) };
    if local.borrowed_len_or_max == usize::MAX {
      // If the string is owned and not borrowed, we do need to clone.
      // We could possibly use std::mem::take here, but that would mutate the
      // original CowRcStr which we borrowed, so might be unexpected. Owned
      // CowRcStr are very rare in practice though, since most strings are
      // borrowed directly from the input.
      let ptr = local.ptr as *const () as *mut String;
      CowArcStr::from(unsafe { (*ptr).clone() })
    } else {
      let s = unsafe {
        str::from_utf8_unchecked(slice::from_raw_parts(
          local.ptr as *const () as *const u8,
          local.borrowed_len_or_max,
        ))
      };

      CowArcStr::from(s)
    }
  }
}

// The below implementation is copied and modified from cssparser.

impl<'a> From<&'a str> for CowArcStr<'a> {
  #[inline]
  fn from(s: &'a str) -> Self {
    let len = s.len();
    assert!(len < usize::MAX);
    CowArcStr {
      ptr: unsafe { &*(s.as_ptr() as *const ()) },
      borrowed_len_or_max: len,
      phantom: PhantomData,
    }
  }
}

impl<'a> From<String> for CowArcStr<'a> {
  #[inline]
  fn from(s: String) -> Self {
    CowArcStr::from_arc(Arc::new(s))
  }
}

impl<'a> CowArcStr<'a> {
  #[inline]
  fn from_arc(s: Arc<String>) -> Self {
    let ptr = unsafe { &*(Arc::into_raw(s) as *const ()) };
    CowArcStr {
      ptr,
      borrowed_len_or_max: usize::MAX,
      phantom: PhantomData,
    }
  }

  #[inline]
  fn unpack(&self) -> Result<&'a str, *const String> {
    if self.borrowed_len_or_max == usize::MAX {
      Err(self.ptr as *const () as *const String)
    } else {
      unsafe {
        Ok(str::from_utf8_unchecked(slice::from_raw_parts(
          self.ptr as *const () as *const u8,
          self.borrowed_len_or_max,
        )))
      }
    }
  }
}

impl<'a> Clone for CowArcStr<'a> {
  #[inline]
  fn clone(&self) -> Self {
    match self.unpack() {
      Err(ptr) => {
        let rc = unsafe { Arc::from_raw(ptr) };
        let new_rc = rc.clone();
        std::mem::forget(rc); // Don’t actually take ownership of this strong reference
        CowArcStr::from_arc(new_rc)
      }
      Ok(_) => CowArcStr { ..*self },
    }
  }
}

impl<'a> Drop for CowArcStr<'a> {
  #[inline]
  fn drop(&mut self) {
    if let Err(ptr) = self.unpack() {
      std::mem::drop(unsafe { Arc::from_raw(ptr) })
    }
  }
}

impl<'a> Deref for CowArcStr<'a> {
  type Target = str;

  #[inline]
  fn deref(&self) -> &str {
    self.unpack().unwrap_or_else(|ptr| unsafe { &**ptr })
  }
}

// Boilerplate / trivial impls below.

impl<'a> AsRef<str> for CowArcStr<'a> {
  #[inline]
  fn as_ref(&self) -> &str {
    self
  }
}

impl<'a> Borrow<str> for CowArcStr<'a> {
  #[inline]
  fn borrow(&self) -> &str {
    self
  }
}

impl<'a> Default for CowArcStr<'a> {
  #[inline]
  fn default() -> Self {
    Self::from("")
  }
}

impl<'a> hash::Hash for CowArcStr<'a> {
  #[inline]
  fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
    str::hash(self, hasher)
  }
}

impl<'a, T: AsRef<str>> PartialEq<T> for CowArcStr<'a> {
  #[inline]
  fn eq(&self, other: &T) -> bool {
    str::eq(self, other.as_ref())
  }
}

impl<'a, T: AsRef<str>> PartialOrd<T> for CowArcStr<'a> {
  #[inline]
  fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
    str::partial_cmp(self, other.as_ref())
  }
}

impl<'a> Eq for CowArcStr<'a> {}

impl<'a> Ord for CowArcStr<'a> {
  #[inline]
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    str::cmp(self, other)
  }
}

impl<'a> fmt::Display for CowArcStr<'a> {
  #[inline]
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    str::fmt(self, formatter)
  }
}

impl<'a> fmt::Debug for CowArcStr<'a> {
  #[inline]
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    str::fmt(self, formatter)
  }
}

impl<'a> Serialize for CowArcStr<'a> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.as_ref().serialize(serializer)
  }
}

#[cfg(feature = "serde")]
impl<'a, 'de: 'a> Deserialize<'de> for CowArcStr<'a> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    deserializer.deserialize_str(CowArcStrVisitor)
  }
}

#[cfg(feature = "serde")]
struct CowArcStrVisitor;

#[cfg(feature = "serde")]
impl<'de> Visitor<'de> for CowArcStrVisitor {
  type Value = CowArcStr<'de>;

  fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    formatter.write_str("a CowArcStr")
  }

  fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
  where
    E: serde::de::Error,
  {
    Ok(v.into())
  }

  fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
  where
    E: serde::de::Error,
  {
    Ok(v.to_owned().into())
  }

  fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
  where
    E: serde::de::Error,
  {
    Ok(v.into())
  }
}
