use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{
  parse_macro_input, Data, DataEnum, DeriveInput, Field, Fields, GenericArgument, Ident, Member, PathArguments,
  Type,
};

pub(crate) fn derive_into_owned(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident: self_name,
    data,
    generics,
    ..
  } = parse_macro_input!(input);

  let res = match data {
    Data::Struct(s) => {
      let fields = s
        .fields
        .iter()
        .enumerate()
        .map(|(index, Field { ty, ident, .. })| {
          let name = ident
            .as_ref()
            .map_or_else(|| Member::Unnamed(index.into()), |ident| Member::Named(ident.clone()));

          let value = into_owned(ty, quote! { self.#name });
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
          if !variant.fields.iter().any(|f| has_lifetime(&f.ty)) {
            return quote! {};
          }

          let name = &variant.ident;
          let mut field_names = Vec::new();
          let mut static_fields = Vec::new();
          for (index, Field { ty, ident, .. }) in variant.fields.iter().enumerate() {
            let name = ident.as_ref().map_or_else(
              || Ident::new(&format!("_{}", index), Span::call_site()),
              |ident| ident.clone(),
            );
            field_names.push(name.clone());
            let value = into_owned(ty, quote! { #name });
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
            Fields::Unit => quote! {},
          }
        })
        .collect::<proc_macro2::TokenStream>();

      quote! {
        match self {
          #variants
          _ => unsafe { std::mem::transmute_copy(&self) }
        }
      }
    }
    _ => {
      panic!("can only derive IntoOwned for enums and structs")
    }
  };

  let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

  let into_owned = if generics.lifetimes().next().is_none() {
    panic!("can't derive IntoOwned on a type without any lifetimes")
  } else {
    let params = generics.type_params();
    quote! {
      impl #impl_generics #self_name #ty_generics #where_clause {
        /// Consumes the value and returns an owned clone.
        pub fn into_owned<'x>(self) -> #self_name<'x, #(#params),*> {
          #res
        }
      }
    }
  };

  into_owned.into()
}

fn into_owned(ty: &Type, name: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
  if has_lifetime(ty) {
    match ty {
      Type::Path(path) => {
        let last = path.path.segments.last().unwrap();
        if last.ident == "Option" {
          let v = quote! { v };
          let into_owned = match &last.arguments {
            PathArguments::AngleBracketed(args) => match args.args.first().unwrap() {
              GenericArgument::Type(ty) => into_owned(ty, v.clone()),
              _ => quote! { #v.into_owned() },
            },
            _ => quote! { #v.into_owned() },
          };
          quote! { #name.map(|#v| #into_owned) }
        } else if last.ident == "Vec"
          || last.ident == "SmallVec"
          || last.ident == "CustomIdentList"
          || last.ident == "AnimationList"
          || last.ident == "AnimationNameList"
        {
          let v = quote! { v };
          let into_owned = match &last.arguments {
            PathArguments::AngleBracketed(args) => match args.args.first().unwrap() {
              GenericArgument::Type(ty) => into_owned(ty, v.clone()),
              _ => quote! { #v.into_owned() },
            },
            _ => quote! { #v.into_owned() },
          };
          quote! { #name.into_iter().map(|#v| #into_owned).collect() }
        } else if last.ident == "Box" {
          quote! { Box::new(#name.into_owned()) }
        } else {
          quote! { #name.into_owned() }
        }
      }
      Type::Reference(_) => panic!("can't derive IntoOwned on a type with references"),
      _ => quote! { #name.into_owned() },
    }
  } else {
    quote! { #name }
  }
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
    Type::Array(arr) => has_lifetime(&*arr.elem),
    _ => false, // TODO
  }
}
