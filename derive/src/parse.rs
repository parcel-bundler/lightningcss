use convert_case::Casing;
use proc_macro::{self, TokenStream};
use proc_macro2::{Literal, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
  parse::Parse, parse_macro_input, parse_quote, Attribute, Data, DataEnum, DeriveInput, Fields, Ident, Token,
};

pub fn derive_parse(input: TokenStream) -> TokenStream {
  let DeriveInput {
    ident,
    data,
    mut generics,
    attrs,
    ..
  } = parse_macro_input!(input);
  let opts = CssOptions::parse_attributes(&attrs).unwrap();
  let cloned_generics = generics.clone();
  let (_, ty_generics, _) = cloned_generics.split_for_impl();

  if generics.lifetimes().next().is_none() {
    generics.params.insert(0, parse_quote! { 'i })
  }

  let lifetime = generics.lifetimes().next().unwrap().clone();
  let (impl_generics, _, where_clause) = generics.split_for_impl();

  let imp = match &data {
    Data::Enum(data) => derive_enum(&data, &ident, &opts),
    _ => todo!(),
  };

  let output = quote! {
    impl #impl_generics Parse<#lifetime> for #ident #ty_generics #where_clause {
      fn parse<'t>(input: &mut Parser<#lifetime, 't>) -> Result<Self, ParseError<#lifetime, ParserError<#lifetime>>> {
        #imp
      }
    }
  };

  output.into()
}

fn derive_enum(data: &DataEnum, ident: &Ident, opts: &CssOptions) -> TokenStream2 {
  let mut idents = Vec::new();
  let mut non_idents = Vec::new();
  for (index, variant) in data.variants.iter().enumerate() {
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

    let mut expr = match &variant.fields {
      Fields::Unit => {
        idents.push((
          Literal::string(&variant.ident.to_string().to_case(opts.case)),
          name.clone(),
        ));
        continue;
      }
      Fields::Named(_) => {
        quote! {
          return Ok(#ident::#name { #(#fields),* })
        }
      }
      Fields::Unnamed(_) => {
        quote! {
          return Ok(#ident::#name(#(#fields),*))
        }
      }
    };

    // Group multiple ident branches together.
    if !idents.is_empty() {
      if idents.len() == 1 {
        let (s, name) = idents.remove(0);
        non_idents.push(quote! {
          if input.try_parse(|input| input.expect_ident_matching(#s)).is_ok() {
            return Ok(#ident::#name)
          }
        });
      } else {
        let matches = idents
          .iter()
          .map(|(s, name)| {
            quote! {
              #s => return Ok(#ident::#name),
            }
          })
          .collect::<Vec<_>>();
        non_idents.push(quote! {
          {
            let state = input.state();
            if let Ok(ident) = input.try_parse(|input| input.expect_ident_cloned()) {
              cssparser::match_ignore_ascii_case! { &*ident,
                #(#matches)*
                _ => {}
              }
              input.reset(&state);
            }
          }
        });
        idents.clear();
      }
    }

    let is_last = index == data.variants.len() - 1;

    for (index, field) in variant.fields.iter().enumerate().rev() {
      let ty = &field.ty;
      let field_name = field.ident.as_ref().map_or_else(
        || Ident::new(&format!("_{}", index), Span::call_site()),
        |ident| ident.clone(),
      );
      if is_last {
        expr = quote! {
          let #field_name = <#ty>::parse(input)?;
          #expr
        };
      } else {
        expr = quote! {
          if let Ok(#field_name) = input.try_parse(<#ty>::parse) {
            #expr
          }
        };
      }
    }

    non_idents.push(expr);
  }

  let idents = if idents.is_empty() {
    quote! {}
  } else if idents.len() == 1 {
    let (s, name) = idents.remove(0);
    quote! {
      input.expect_ident_matching(#s)?;
      Ok(#ident::#name)
    }
  } else {
    let idents = idents
      .into_iter()
      .map(|(s, name)| {
        quote! {
          #s => Ok(#ident::#name),
        }
      })
      .collect::<Vec<_>>();
    quote! {
      let location = input.current_source_location();
      let ident = input.expect_ident()?;
      cssparser::match_ignore_ascii_case! { &*ident,
        #(#idents)*
        _ => Err(location.new_unexpected_token_error(
          cssparser::Token::Ident(ident.clone())
        ))
      }
    }
  };

  let output = quote! {
    #(#non_idents)*
    #idents
  };

  output.into()
}

pub struct CssOptions {
  pub case: convert_case::Case,
}

impl CssOptions {
  pub fn parse_attributes(attrs: &Vec<Attribute>) -> syn::Result<Self> {
    for attr in attrs {
      if attr.path.is_ident("css") {
        let opts: CssOptions = attr.parse_args()?;
        return Ok(opts);
      }
    }

    Ok(CssOptions {
      case: convert_case::Case::Kebab,
    })
  }
}

impl Parse for CssOptions {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let mut case = convert_case::Case::Kebab;
    while !input.is_empty() {
      let k: Ident = input.parse()?;
      let _: Token![=] = input.parse()?;
      let v: Ident = input.parse()?;

      if k == "case" {
        if v == "lower" {
          case = convert_case::Case::Flat;
        }
      }
    }

    Ok(Self { case })
  }
}
