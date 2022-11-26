use std::collections::HashSet;

use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{
  parse::Parse, parse_macro_input, parse_quote, Attribute, Data, DataEnum, DeriveInput, Field, Fields,
  GenericParam, Ident, Member, Token, Type,
};

#[proc_macro_derive(Visit, attributes(visit, skip_visit, skip_type))]
pub fn derive_visit_children(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident,
    data,
    generics,
    attrs,
    ..
  } = parse_macro_input!(input);

  let mut impl_generics = generics.clone();
  if impl_generics.lifetimes().next().is_none() {
    impl_generics.params.insert(0, parse_quote! { 'i })
  }

  let lifetime = impl_generics.lifetimes().next().unwrap().clone();
  let t = impl_generics.type_params().find(|g| &g.ident.to_string() == "R");
  let v = quote! { __V };
  let t = if let Some(t) = t {
    GenericParam::Type(t.ident.clone().into())
  } else {
    let t: GenericParam = parse_quote! { __T };
    impl_generics.params.push(parse_quote! { #t: Visit<#lifetime, __T, #v> });
    t
  };

  impl_generics
    .params
    .push(parse_quote! { #v: crate::visitor::Visitor<#lifetime, #t> });

  for ty in generics.type_params() {
    let name = &ty.ident;
    impl_generics.make_where_clause().predicates.push(parse_quote! {
      #name: Visit<#lifetime, #t, #v>
    })
  }

  let options = if let Some(attr) = attrs.iter().find(|attr| attr.path.is_ident("visit")) {
    let opts: VisitOptions = attr.parse_args().unwrap();
    Some(opts)
  } else {
    None
  };

  let mut seen_types = HashSet::new();
  let mut child_types = Vec::new();
  let mut visit = Vec::new();
  match data {
    Data::Struct(s) => {
      for (index, Field { ty, ident, attrs, .. }) in s.fields.iter().enumerate() {
        if attrs.iter().any(|attr| attr.path.is_ident("skip_visit")) {
          continue;
        }

        if matches!(ty, Type::Reference(_)) {
          continue;
        }

        if !seen_types.contains(ty) && !skip_type(attrs) {
          seen_types.insert(ty.clone());
          child_types.push(quote! {
            <#ty as Visit<#lifetime, #t, #v>>::CHILD_TYPES.bits()
          });
        }

        let name = ident
          .as_ref()
          .map_or_else(|| Member::Unnamed(index.into()), |ident| Member::Named(ident.clone()));
        visit.push(quote! { self.#name.visit(visitor); })
      }
    }
    Data::Enum(DataEnum { variants, .. }) => {
      let variants = variants
        .iter()
        .map(|variant| {
          let name = &variant.ident;
          let mut field_names = Vec::new();
          let mut visit_fields = Vec::new();
          for (index, Field { ty, ident, attrs, .. }) in variant.fields.iter().enumerate() {
            let name = ident.as_ref().map_or_else(
              || Ident::new(&format!("_{}", index), Span::call_site()),
              |ident| ident.clone(),
            );
            field_names.push(name.clone());

            if matches!(ty, Type::Reference(_)) {
              continue;
            }

            if !seen_types.contains(ty) && !skip_type(attrs) && !skip_type(&variant.attrs) {
              seen_types.insert(ty.clone());
              child_types.push(quote! {
                <#ty as Visit<#lifetime, #t, #v>>::CHILD_TYPES.bits()
              });
            }

            visit_fields.push(quote! { #name.visit(visitor); })
          }

          match variant.fields {
            Fields::Unnamed(_) => {
              quote! {
                Self::#name(#(#field_names),*) => {
                  #(#visit_fields)*
                }
              }
            }
            Fields::Named(_) => {
              quote! {
                Self::#name { #(#field_names),* } => {
                  #(#visit_fields)*
                }
              }
            }
            Fields::Unit => quote! {},
          }
        })
        .collect::<proc_macro2::TokenStream>();

      visit.push(quote! {
        match self {
          #variants
          _ => {}
        }
      })
    }
    _ => {}
  }

  if child_types.is_empty() {
    child_types.push(quote! { crate::visitor::VisitTypes::empty().bits() });
  }

  let self_visit = if let Some(VisitOptions { visit, kind }) = options {
    child_types.push(quote! { crate::visitor::VisitTypes::#kind.bits() });

    quote! {
      fn visit(&mut self, visitor: &mut #v) {
        if #v::TYPES.contains(crate::visitor::VisitTypes::#kind) {
          visitor.#visit(self)
        } else {
          self.visit_children(visitor)
        }
      }
    }
  } else {
    quote! {}
  };

  let (_, ty_generics, _) = generics.split_for_impl();
  let (impl_generics, _, where_clause) = impl_generics.split_for_impl();

  let child_types = quote! {
    unsafe { crate::visitor::VisitTypes::from_bits_unchecked(#(#child_types)|*) }
  };

  let output = quote! {
    impl #impl_generics Visit<#lifetime, #t, #v> for #ident #ty_generics #where_clause {
      const CHILD_TYPES: crate::visitor::VisitTypes = #child_types;

      #self_visit

      fn visit_children(&mut self, visitor: &mut #v) {
        if !<Self as Visit<#lifetime, #t, #v>>::CHILD_TYPES.intersects(#v::TYPES) {
          return
        }

        #(#visit)*
      }
    }
  };

  output.into()
}

fn skip_type(attrs: &Vec<Attribute>) -> bool {
  attrs.iter().any(|attr| attr.path.is_ident("skip_type"))
}

struct VisitOptions {
  visit: Ident,
  kind: Ident,
}

impl Parse for VisitOptions {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let visit: Ident = input.parse()?;
    let _: Token![,] = input.parse()?;
    let kind: Ident = input.parse()?;
    Ok(Self { visit, kind })
  }
}
