use std::collections::HashSet;

use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{
  parse::Parse, parse_macro_input, parse_quote, Attribute, Data, DataEnum, DeriveInput, Field, Fields,
  GenericArgument, GenericParam, Ident, Member, PathArguments, Token, Type,
};

pub(crate) fn derive_to_static(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident: self_name,
    data,
    generics,
    attrs,
    ..
  } = parse_macro_input!(input);

  let res = match data {
    Data::Struct(s) => {
      let fields = s
        .fields
        .iter()
        .enumerate()
        .map(|(index, Field { ty, ident, attrs, .. })| {
          let name = ident
            .as_ref()
            .map_or_else(|| Member::Unnamed(index.into()), |ident| Member::Named(ident.clone()));

          let value = if has_lifetime(ty) {
            quote! { self.#name.to_static() }
          } else {
            quote! { self.#name }
          };

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
            Self(#(#fields),*)
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
          for (index, Field { ty, ident, .. }) in variant.fields.iter().enumerate() {
            let name = ident.as_ref().map_or_else(
              || Ident::new(&format!("_{}", index), Span::call_site()),
              |ident| ident.clone(),
            );
            field_names.push(name.clone());

            if let Type::Path(path) = ty {
              let value = if path.path.segments.iter().any(|s| !s.arguments.is_empty()) {
                quote! { #name.to_static() }
              } else {
                quote! { #name }
              };

              if let Some(ident) = ident {
                static_fields.push(quote! { #ident: #value });
              } else {
                static_fields.push(value);
              }
            }
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
      quote! {}
    }
  };

  let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

  let to_static = if generics.lifetimes().next().is_none() {
    // quote! {
    //   impl #ident #ty_generics #where_clause {
    //     pub(crate) fn to_static(&mut self) -> &mut Self {
    //       self
    //     }
    //   }
    // }
    quote! {}
  } else {
    quote! {
      impl #impl_generics crate::traits::ToStatic for #self_name #ty_generics #where_clause {
        fn to_static(self) -> #self_name<'static> {
          use crate::traits::ToStatic;
          #res
        }
      }
    }
  };

  to_static.into()
}

fn has_lifetime(ty: &Type) -> bool {
  match ty {
    Type::Path(path) => path.path.segments.iter().any(|s| match &s.arguments {
      PathArguments::AngleBracketed(args) => args.args.iter().any(|arg| match arg {
        GenericArgument::Lifetime(..) => true,
        GenericArgument::Type(ty) => has_lifetime(ty),
        _ => false,
      }),
      _ => false,
    }),
    _ => false, // TODO
  }
}
