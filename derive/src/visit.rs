use std::collections::HashSet;

use proc_macro::{self, TokenStream};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
  parse::Parse, parse_macro_input, parse_quote, Attribute, Data, DataEnum, DeriveInput, Field, Fields,
  GenericParam, Generics, Ident, Member, Token, Type, Visibility,
};

pub fn derive_visit_children(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident,
    data,
    generics,
    attrs,
    ..
  } = parse_macro_input!(input);

  let options: Vec<VisitOptions> = attrs
    .iter()
    .filter_map(|attr| {
      if attr.path.is_ident("visit") {
        let opts: VisitOptions = attr.parse_args().unwrap();
        Some(opts)
      } else {
        None
      }
    })
    .collect();

  let visit_types = if let Some(attr) = attrs.iter().find(|attr| attr.path.is_ident("visit_types")) {
    let types: VisitTypes = attr.parse_args().unwrap();
    let types = types.types;
    Some(quote! { crate::visit_types!(#(#types)|*) })
  } else {
    None
  };

  if options.is_empty() {
    derive(&ident, &data, &generics, None, visit_types)
  } else {
    options
      .into_iter()
      .map(|options| derive(&ident, &data, &generics, Some(options), visit_types.clone()))
      .collect()
  }
}

fn derive(
  ident: &Ident,
  data: &Data,
  generics: &Generics,
  options: Option<VisitOptions>,
  visit_types: Option<TokenStream2>,
) -> TokenStream {
  let mut impl_generics = generics.clone();
  let mut type_defs = quote! {};
  let generics = if let Some(VisitOptions {
    generic: Some(generic), ..
  }) = &options
  {
    let mappings = generics
      .type_params()
      .zip(generic.type_params())
      .map(|(a, b)| quote! { type #a = #b; });
    type_defs = quote! { #(#mappings)* };
    impl_generics.params.clear();
    generic
  } else {
    &generics
  };

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
    impl_generics
      .params
      .push(parse_quote! { #t: crate::visitor::Visit<#lifetime, __T, #v> });
    t
  };

  impl_generics
    .params
    .push(parse_quote! { #v: ?Sized + crate::visitor::Visitor<#lifetime, #t> });

  for ty in generics.type_params() {
    let name = &ty.ident;
    impl_generics.make_where_clause().predicates.push(parse_quote! {
      #name: Visit<#lifetime, #t, #v>
    })
  }

  let mut seen_types = HashSet::new();
  let mut child_types = Vec::new();
  let mut visit = Vec::new();
  match data {
    Data::Struct(s) => {
      for (
        index,
        Field {
          vis, ty, ident, attrs, ..
        },
      ) in s.fields.iter().enumerate()
      {
        if attrs.iter().any(|attr| attr.path.is_ident("skip_visit")) {
          continue;
        }

        if matches!(ty, Type::Reference(_)) || !matches!(vis, Visibility::Public(..)) {
          continue;
        }

        if visit_types.is_none() && !seen_types.contains(ty) && !skip_type(attrs) {
          seen_types.insert(ty.clone());
          child_types.push(quote! {
            <#ty as Visit<#lifetime, #t, #v>>::CHILD_TYPES.bits()
          });
        }

        let name = ident
          .as_ref()
          .map_or_else(|| Member::Unnamed(index.into()), |ident| Member::Named(ident.clone()));
        visit.push(quote! { self.#name.visit(visitor)?; })
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

            if visit_types.is_none() && !seen_types.contains(ty) && !skip_type(attrs) && !skip_type(&variant.attrs)
            {
              seen_types.insert(ty.clone());
              child_types.push(quote! {
                <#ty as Visit<#lifetime, #t, #v>>::CHILD_TYPES.bits()
              });
            }

            visit_fields.push(quote! { #name.visit(visitor)?; })
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

  if visit_types.is_none() && child_types.is_empty() {
    child_types.push(quote! { crate::visitor::VisitTypes::empty().bits() });
  }

  let (_, ty_generics, _) = generics.split_for_impl();
  let (impl_generics, _, where_clause) = impl_generics.split_for_impl();

  let self_visit = if let Some(VisitOptions {
    visit: Some(visit),
    kind: Some(kind),
    ..
  }) = &options
  {
    child_types.push(quote! { crate::visitor::VisitTypes::#kind.bits() });

    quote! {
      fn visit(&mut self, visitor: &mut #v) -> Result<(), #v::Error> {
        if visitor.visit_types().contains(crate::visitor::VisitTypes::#kind) {
          visitor.#visit(self)
        } else {
          self.visit_children(visitor)
        }
      }
    }
  } else {
    quote! {}
  };

  let child_types = visit_types.unwrap_or_else(|| {
    quote! {
      {
        #type_defs
        crate::visitor::VisitTypes::from_bits_retain(#(#child_types)|*)
      }
    }
  });

  let output = quote! {
    impl #impl_generics Visit<#lifetime, #t, #v> for #ident #ty_generics #where_clause {
      const CHILD_TYPES: crate::visitor::VisitTypes = #child_types;

      #self_visit

      fn visit_children(&mut self, visitor: &mut #v) -> Result<(), #v::Error> {
        if !<Self as Visit<#lifetime, #t, #v>>::CHILD_TYPES.intersects(visitor.visit_types()) {
          return Ok(())
        }

        #(#visit)*

        Ok(())
      }
    }
  };

  output.into()
}

fn skip_type(attrs: &Vec<Attribute>) -> bool {
  attrs.iter().any(|attr| attr.path.is_ident("skip_type"))
}

struct VisitOptions {
  visit: Option<Ident>,
  kind: Option<Ident>,
  generic: Option<Generics>,
}

impl Parse for VisitOptions {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let (visit, kind, comma) = if input.peek(Ident) {
      let visit: Ident = input.parse()?;
      let _: Token![,] = input.parse()?;
      let kind: Ident = input.parse()?;
      let comma: Result<Token![,], _> = input.parse();
      (Some(visit), Some(kind), comma.is_ok())
    } else {
      (None, None, true)
    };
    let generic: Option<Generics> = if comma { Some(input.parse()?) } else { None };
    Ok(Self { visit, kind, generic })
  }
}

struct VisitTypes {
  types: Vec<Ident>,
}

impl Parse for VisitTypes {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let first: Ident = input.parse()?;
    let mut types = vec![first];
    while input.parse::<Token![|]>().is_ok() {
      let id: Ident = input.parse()?;
      types.push(id);
    }
    Ok(Self { types })
  }
}
