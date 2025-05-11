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

impl_into_owned!(bool, f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, usize, isize, char, String);

macro_rules! impl_tuple {
  (
    $($name:ident),*
  ) =>{
    #[allow(non_snake_case)]
    impl<'any, $($name,)*> IntoOwned<'any> for ($($name,)*)
    where
        $($name: IntoOwned<'any>),*
    {
      type Owned = ($(<$name as IntoOwned<'any>>::Owned,)*);

      #[inline]
      fn into_owned(self) -> Self::Owned {
        let ($($name,)*) = self;
        ($($name.into_owned(),)*)
      }
    }
  };
}

macro_rules! call_impl_tuple {
  () => {};
  ($first:ident) => {
    impl_tuple!($first);
  };
  (
    $first:ident,
    $($name:ident),*
  ) => {
    call_impl_tuple!($($name),*);
    impl_tuple!($first, $($name),*);
  };
}

call_impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);

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
  T: IntoOwned<'any>,
{
  type Owned = Box<<T as IntoOwned<'any>>::Owned>;

  fn into_owned(self) -> Self::Owned {
    Box::new((*self).into_owned())
  }
}

impl<'any, T> IntoOwned<'any> for Box<[T]>
where
  T: IntoOwned<'any>,
{
  type Owned = Box<[<T as IntoOwned<'any>>::Owned]>;

  fn into_owned(self) -> Self::Owned {
    self.into_vec().into_owned().into_boxed_slice()
  }
}

#[cfg(feature = "smallvec")]
impl<'any, T, const N: usize> IntoOwned<'any> for smallvec::SmallVec<[T; N]>
where
  T: IntoOwned<'any>,
  [T; N]: smallvec::Array<Item = T>,
  [<T as IntoOwned<'any>>::Owned; N]: smallvec::Array<Item = <T as IntoOwned<'any>>::Owned>,
{
  type Owned = smallvec::SmallVec<[<T as IntoOwned<'any>>::Owned; N]>;

  fn into_owned(self) -> Self::Owned {
    self.into_iter().map(|v| v.into_owned()).collect()
  }
}

#[cfg(feature = "indexmap")]
impl<'any, K, V> IntoOwned<'any> for indexmap::IndexMap<K, V>
where
  K: IntoOwned<'any>,
  V: IntoOwned<'any>,
  <K as IntoOwned<'any>>::Owned: Eq + std::hash::Hash,
{
  type Owned = indexmap::IndexMap<<K as IntoOwned<'any>>::Owned, <V as IntoOwned<'any>>::Owned>;

  fn into_owned(self) -> Self::Owned {
    self.into_iter().map(|(k, v)| (k.into_owned(), v.into_owned())).collect()
  }
}

impl<'any, T, const N: usize> IntoOwned<'any> for [T; N]
where
  T: IntoOwned<'any>,
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
