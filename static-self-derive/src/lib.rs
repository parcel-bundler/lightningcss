use proc_macro::TokenStream;

mod into_owned;

#[proc_macro_derive(IntoOwned)]
pub fn derive_into_owned(input: TokenStream) -> TokenStream {
  into_owned::derive_into_owned(input)
}
