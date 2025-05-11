//! Types used to represent strings.

use crate::traits::{Parse, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::{Visit, VisitTypes, Visitor};
use cssparser::{serialize_string, CowRcStr};
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer};
#[cfg(any(feature = "serde", feature = "nodejs"))]
use serde::{Serialize, Serializer};
use std::borrow::{Borrow, Cow};
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

impl<'a> From<Cow<'a, str>> for CowArcStr<'a> {
  #[inline]
  fn from(s: Cow<'a, str>) -> Self {
    match s {
      Cow::Borrowed(s) => s.into(),
      Cow::Owned(s) => s.into(),
    }
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

#[cfg(feature = "into_owned")]
impl<'any> static_self::IntoOwned<'any> for CowArcStr<'_> {
  type Owned = CowArcStr<'any>;

  /// Consumes the value and returns an owned clone.
  fn into_owned(self) -> Self::Owned {
    if self.borrowed_len_or_max != usize::MAX {
      CowArcStr::from(self.as_ref().to_owned())
    } else {
      unsafe { std::mem::transmute(self) }
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

#[cfg(any(feature = "nodejs", feature = "serde"))]
impl<'a> Serialize for CowArcStr<'a> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.as_ref().serialize(serializer)
  }
}

#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
impl<'a, 'de: 'a> Deserialize<'de> for CowArcStr<'a> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    deserializer.deserialize_str(CowArcStrVisitor)
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for CowArcStr<'a> {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    String::json_schema(gen)
  }

  fn schema_name() -> String {
    "String".into()
  }
}

#[cfg(feature = "serde")]
struct CowArcStrVisitor;

#[cfg(feature = "serde")]
impl<'de> serde::de::Visitor<'de> for CowArcStrVisitor {
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

#[cfg(feature = "visitor")]
impl<'i, V: ?Sized + Visitor<'i, T>, T: Visit<'i, T, V>> Visit<'i, T, V> for CowArcStr<'i> {
  const CHILD_TYPES: VisitTypes = VisitTypes::empty();
  fn visit_children(&mut self, _: &mut V) -> Result<(), V::Error> {
    Ok(())
  }
}

/// A quoted CSS string.
#[derive(Clone, Eq, Ord, Hash, Debug)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct CSSString<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub CowArcStr<'i>);

impl<'i> Parse<'i> for CSSString<'i> {
  fn parse<'t>(
    input: &mut cssparser::Parser<'i, 't>,
  ) -> Result<Self, cssparser::ParseError<'i, crate::error::ParserError<'i>>> {
    let s = input.expect_string()?;
    Ok(CSSString(s.into()))
  }
}

impl<'i> ToCss for CSSString<'i> {
  fn to_css<W>(&self, dest: &mut crate::printer::Printer<W>) -> Result<(), crate::error::PrinterError>
  where
    W: std::fmt::Write,
  {
    serialize_string(&self.0, dest)?;
    Ok(())
  }
}

impl<'i> cssparser::ToCss for CSSString<'i> {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result
  where
    W: fmt::Write,
  {
    serialize_string(&self.0, dest)
  }
}

macro_rules! impl_string_type {
  ($t: ident) => {
    impl<'i> From<CowRcStr<'i>> for $t<'i> {
      fn from(s: CowRcStr<'i>) -> Self {
        $t(s.into())
      }
    }

    impl<'a> From<&CowRcStr<'a>> for $t<'a> {
      fn from(s: &CowRcStr<'a>) -> Self {
        $t(s.into())
      }
    }

    impl<'i> From<String> for $t<'i> {
      fn from(s: String) -> Self {
        $t(s.into())
      }
    }

    impl<'i> From<&'i str> for $t<'i> {
      fn from(s: &'i str) -> Self {
        $t(s.into())
      }
    }

    impl<'a> From<std::borrow::Cow<'a, str>> for $t<'a> {
      #[inline]
      fn from(s: std::borrow::Cow<'a, str>) -> Self {
        match s {
          std::borrow::Cow::Borrowed(s) => s.into(),
          std::borrow::Cow::Owned(s) => s.into(),
        }
      }
    }

    impl<'a> Deref for $t<'a> {
      type Target = str;

      #[inline]
      fn deref(&self) -> &str {
        self.0.deref()
      }
    }

    impl<'a> AsRef<str> for $t<'a> {
      #[inline]
      fn as_ref(&self) -> &str {
        self
      }
    }

    impl<'a> Borrow<str> for $t<'a> {
      #[inline]
      fn borrow(&self) -> &str {
        self
      }
    }

    impl<'a> std::fmt::Display for $t<'a> {
      #[inline]
      fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        str::fmt(self, formatter)
      }
    }

    impl<'a, T: AsRef<str>> PartialEq<T> for $t<'a> {
      #[inline]
      fn eq(&self, other: &T) -> bool {
        str::eq(self, other.as_ref())
      }
    }

    impl<'a, T: AsRef<str>> PartialOrd<T> for $t<'a> {
      #[inline]
      fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        str::partial_cmp(self, other.as_ref())
      }
    }
  };
}

impl_string_type!(CSSString);

pub(crate) use impl_string_type;
