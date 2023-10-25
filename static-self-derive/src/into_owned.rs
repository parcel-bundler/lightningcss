use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, parse_quote, Data, DataEnum, DeriveInput, Field, Fields, Ident, Member};

pub(crate) fn derive_into_owned(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident: self_name,
    data,
    mut generics,
    ..
  } = parse_macro_input!(input);

  let res = match data {
    Data::Struct(s) => {
      let fields = s
        .fields
        .iter()
        .enumerate()
        .map(|(index, Field { ident, .. })| {
          let name = ident
            .as_ref()
            .map_or_else(|| Member::Unnamed(index.into()), |ident| Member::Named(ident.clone()));

          let value = into_owned(quote! { self.#name });
          if let Some(ident) = ident {
            quote! { #ident: #value }
          } else {
            value
          }
        })
        .collect::<Vec<proc_macro2::TokenStream>>();

      match s.fields {
        Fields::Unnamed(_) => {
          quote! {
            #self_name(#(#fields),*)
          }
        }
        Fields::Named(_) => {
          quote! {
            #self_name { #(#fields),* }
          }
        }
        Fields::Unit => quote! {},
      }
    }
    Data::Enum(DataEnum { variants, .. }) => {
      let variants = variants
        .iter()
        .map(|variant| {
          let name = &variant.ident;
          let mut field_names = Vec::new();
          let mut static_fields = Vec::new();
          for (index, Field { ident, .. }) in variant.fields.iter().enumerate() {
            let name = ident.as_ref().map_or_else(
              || Ident::new(&format!("_{}", index), Span::call_site()),
              |ident| ident.clone(),
            );
            field_names.push(name.clone());
            let value = into_owned(quote! { #name });
            static_fields.push(if let Some(ident) = ident {
              quote! { #ident: #value }
            } else {
              value
            })
          }

          match variant.fields {
            Fields::Unnamed(_) => {
              quote! {
                #self_name::#name(#(#field_names),*) => {
                  #self_name::#name(#(#static_fields),*)
                }
              }
            }
            Fields::Named(_) => {
              quote! {
                #self_name::#name { #(#field_names),* } => {
                  #self_name::#name { #(#static_fields),* }
                }
              }
            }
            Fields::Unit => quote! {
              #self_name::#name => #self_name::#name,
            },
          }
        })
        .collect::<proc_macro2::TokenStream>();

      quote! {
        match self {
          #variants
        }
      }
    }
    _ => {
      panic!("can only derive IntoOwned for enums and structs")
    }
  };

  let orig_generics = generics.clone();

  // Add generic bounds for all type parameters.
  let mut type_param_names = vec![];

  for ty in generics.type_params() {
    type_param_names.push(ty.ident.clone());
  }

  for type_param in type_param_names {
    generics.make_where_clause().predicates.push_value(parse_quote! {
      #type_param: ::static_self::IntoOwned<'any>
    })
  }

  let has_lifetime = generics
    .params
    .first()
    .map_or(false, |v| matches!(v, syn::GenericParam::Lifetime(..)));
  let has_generic = !generics.params.is_empty();

  // Prepend `'any` to generics
  let any = syn::GenericParam::Lifetime(syn::LifetimeDef {
    attrs: Default::default(),
    lifetime: syn::Lifetime {
      apostrophe: Span::call_site(),
      ident: Ident::new("any", Span::call_site()),
    },
    colon_token: None,
    bounds: Default::default(),
  });
  generics.params.insert(0, any.clone());

  let (impl_generics, _, where_clause) = generics.split_for_impl();
  let (_, ty_generics, _) = orig_generics.split_for_impl();

  let into_owned = if !has_generic {
    quote! {
      impl #impl_generics ::static_self::IntoOwned<'any> for #self_name #ty_generics #where_clause {
        type Owned = Self;

        #[inline]
        fn into_owned(self) -> Self {
          self
        }
      }
    }
  } else {
    let mut generics_without_default = generics.clone();

    let mut params = Vec::new();

    for p in generics_without_default.params.iter_mut() {
      if let syn::GenericParam::Type(ty) = p {
        ty.default = None;

        params.push(quote!(<#ty as static_self::IntoOwned<'any>>::Owned));
      }
    }

    if has_lifetime {
      quote! {
        impl #impl_generics ::static_self::IntoOwned<'any> for #self_name #ty_generics #where_clause {
          type Owned = #self_name<'any, #(#params),*>;
          /// Consumes the value and returns an owned clone.
          fn into_owned(self) -> Self::Owned {
            use ::static_self::IntoOwned;

            #res
          }
        }
      }
    } else {
      quote! {
        impl #impl_generics ::static_self::IntoOwned<'any> for #self_name #ty_generics #where_clause {
          type Owned = #self_name<#(#params),*>;
          /// Consumes the value and returns an owned clone.
          fn into_owned(self) -> Self::Owned {
            use ::static_self::IntoOwned;

            #res
          }
        }
      }
    }
  };

  into_owned.into()
}

fn into_owned(name: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
  quote! { #name.into_owned() }
}
