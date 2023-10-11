pub use static_self_derive::IntoOwned;

/// A trait for things that can be cloned with a new lifetime.
///
/// `'any` lifeitme means the output should have `'static` lifetime.
pub trait IntoOwned<'any> {
  /// A variant of `Self` with a new lifetime.
  type Owned: 'any;

  /// Make lifetime of `self` `'static`.
  fn into_owned(self) -> Self::Owned;
}

macro_rules! impl_into_owned {
  ($t: ty) => {
    impl<'a> IntoOwned<'a> for $t {
      type Owned = Self;

      #[inline]
      fn into_owned(self) -> Self {
        self
      }
    }
  };
  ($($t:ty),*) => {
    $(impl_into_owned!($t);)*
  };
}

impl_into_owned!(bool, f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, usize, isize);

impl<'any, T> IntoOwned<'any> for Vec<T>
where
  T: IntoOwned<'any>,
{
  type Owned = Vec<<T as IntoOwned<'any>>::Owned>;

  fn into_owned(self) -> Self::Owned {
    self.into_iter().map(|v| v.into_owned()).collect()
  }
}
impl<'any, T> IntoOwned<'any> for Option<T>
where
  T: IntoOwned<'any>,
{
  type Owned = Option<<T as IntoOwned<'any>>::Owned>;

  fn into_owned(self) -> Self::Owned {
    self.map(|v| v.into_owned())
  }
}
impl<'any, T> IntoOwned<'any> for Box<T>
where
  T: for<'aa> IntoOwned<'aa>,
{
  type Owned = Box<<T as IntoOwned<'any>>::Owned>;

  fn into_owned(self) -> Self::Owned {
    Box::new((*self).into_owned())
  }
}

#[cfg(feature = "smallvec")]
impl<'any, T, const N: usize> IntoOwned<'any> for smallvec::SmallVec<[T; N]>
where
  T: for<'aa> IntoOwned<'aa>,
  [T; N]: smallvec::Array<Item = T>,
  [<T as IntoOwned<'any>>::Owned; N]: smallvec::Array<Item = <T as IntoOwned<'any>>::Owned>,
{
  type Owned = smallvec::SmallVec<[<T as IntoOwned<'any>>::Owned; N]>;

  fn into_owned(self) -> Self::Owned {
    self.into_iter().map(|v| v.into_owned()).collect()
  }
}

impl<'any, T, const N: usize> IntoOwned<'any> for [T; N]
where
  T: for<'aa> IntoOwned<'aa>,
{
  type Owned = [<T as IntoOwned<'any>>::Owned; N];

  fn into_owned(self) -> Self::Owned {
    self
      .into_iter()
      .map(|v| v.into_owned())
      .collect::<Vec<_>>()
      .try_into()
      .unwrap_or_else(|_| unreachable!("Vec<T> with N elements should be able to be converted to [T; N]"))
  }
}
