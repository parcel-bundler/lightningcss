use convert_case::Casing;
use proc_macro::{self, TokenStream};
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Fields, Ident, Type};

use crate::parse::CssOptions;

pub fn derive_to_css(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident,
    data,
    generics,
    attrs,
    ..
  } = parse_macro_input!(input);

  let opts = CssOptions::parse_attributes(&attrs).unwrap();
  let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

  let imp = match &data {
    Data::Enum(data) => derive_enum(&data, &opts),
    _ => todo!(),
  };

  let output = quote! {
    impl #impl_generics ToCss for #ident #ty_generics #where_clause {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
      where
        W: std::fmt::Write,
      {
        #imp
      }
    }
  };

  output.into()
}

fn derive_enum(data: &DataEnum, opts: &CssOptions) -> TokenStream2 {
  let variants = data
    .variants
    .iter()
    .map(|variant| {
      let name = &variant.ident;
      let fields = variant
        .fields
        .iter()
        .enumerate()
        .map(|(index, field)| {
          field.ident.as_ref().map_or_else(
            || Ident::new(&format!("_{}", index), Span::call_site()),
            |ident| ident.clone(),
          )
        })
        .collect::<Vec<_>>();

      #[derive(PartialEq)]
      enum NeedsSpace {
        Yes,
        No,
        Maybe,
      }

      let mut needs_space = NeedsSpace::No;
      let mut fields_iter = variant.fields.iter().zip(fields.iter()).peekable();
      let mut writes = Vec::new();
      let mut has_needs_space = false;
      while let Some((field, name)) = fields_iter.next() {
        writes.push(if fields.len() > 1 {
          let space = match needs_space {
            NeedsSpace::Yes => quote! { dest.write_char(' ')?; },
            NeedsSpace::No => quote! {},
            NeedsSpace::Maybe => {
              has_needs_space = true;
              quote! {
                if needs_space {
                  dest.write_char(' ')?;
                }
              }
            }
          };

          if is_option(&field.ty) {
            needs_space = NeedsSpace::Maybe;
            let after_space = if matches!(fields_iter.peek(), Some((field, _)) if !is_option(&field.ty)) {
              // If the next field is non-optional, just insert the space here.
              needs_space = NeedsSpace::No;
              quote! { dest.write_char(' ')?; }
            } else {
              quote! {}
            };
            quote! {
              if let Some(v) = #name {
                #space
                v.to_css(dest)?;
                #after_space
              }
            }
          } else {
            needs_space = NeedsSpace::Yes;
            quote! {
              #space
              #name.to_css(dest)?;
            }
          }
        } else {
          quote! { #name.to_css(dest) }
        });
      }

      if writes.len() > 1 {
        writes.push(quote! { Ok(()) });
      }

      if has_needs_space {
        writes.insert(0, quote! { let mut needs_space = false });
      }

      match variant.fields {
        Fields::Unit => {
          let s = Literal::string(&variant.ident.to_string().to_case(opts.case));
          quote! {
            Self::#name => dest.write_str(#s)
          }
        }
        Fields::Named(_) => {
          quote! {
            Self::#name { #(#fields),* } => {
              #(#writes)*
            }
          }
        }
        Fields::Unnamed(_) => {
          quote! {
            Self::#name(#(#fields),*) => {
              #(#writes)*
            }
          }
        }
      }
    })
    .collect::<Vec<_>>();

  let output = quote! {
    match self {
      #(#variants),*
    }
  };

  output.into()
}

fn is_option(ty: &Type) -> bool {
  matches!(&ty, Type::Path(p) if p.qself.is_none() && p.path.segments.iter().next().unwrap().ident == "Option")
}
