use cssparser::*;
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::properties::PropertyId;
use crate::values::color::CssColor;
use crate::values::length::serialize_dimension;
use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;
use crate::prefixes::Feature;
use crate::error::{ParserError, PrinterError};

#[derive(Debug, Clone, PartialEq)]
pub struct CustomProperty<'i> {
  pub name: CowRcStr<'i>,
  pub value: TokenList<'i>
}

impl<'i> CustomProperty<'i> {
  pub fn parse<'t>(
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = TokenList::parse(input)?;
    Ok(CustomProperty {
      name,
      value
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnparsedProperty<'i> {
  pub property_id: PropertyId<'i>,
  pub value: TokenList<'i>
}

impl<'i> UnparsedProperty<'i> {
  pub fn parse<'t>(
    property_id: PropertyId<'i>,
    input: &mut Parser<'i, 't>
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = TokenList::parse(input)?;
    Ok(UnparsedProperty {
      property_id,
      value
    })
  }

  pub fn get_prefixed(&self, targets: Option<Browsers>, feature: Feature) -> UnparsedProperty<'i> {
    let mut clone = self.clone();
    if self.property_id.prefix().contains(VendorPrefix::None) {
      if let Some(targets) = targets {
        clone.property_id = clone.property_id.with_prefix(feature.prefixes_for(targets))
      }
    }
    clone
  }

  pub fn with_property_id(&self, property_id: PropertyId<'i>) -> UnparsedProperty<'i> {
    UnparsedProperty {
      property_id,
      value: self.value.clone()
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenList<'i>(pub Vec<TokenOrValue<'i>>);

#[derive(Debug, Clone, PartialEq)]
pub enum TokenOrValue<'i> {
  Token(Token<'i>),
  Color(CssColor)
}

impl<'i> From<Token<'i>> for TokenOrValue<'i> {
  fn from(token: Token<'i>) -> TokenOrValue<'i> {
    TokenOrValue::Token(token)
  }
}

impl<'i> TokenOrValue<'i> {
  pub fn is_whitespace(&self) -> bool {
    matches!(self, TokenOrValue::Token(Token::WhiteSpace(_)))
  }
}

impl<'i> TokenList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
      let mut tokens = vec![];
      TokenList::parse_into(input, &mut tokens)?;

      // Slice off leading and trailing whitespace if there are at least two tokens.
      // If there is only one token, we must preserve it. e.g. `--foo: ;` is valid. 
      if tokens.len() >= 2 {
        let mut slice = &tokens[..];
        if matches!(tokens.first(), Some(token) if token.is_whitespace()) {
          slice = &slice[1..];
        }
        if matches!(tokens.last(), Some(token) if token.is_whitespace()) {
          slice = &slice[..slice.len() - 1];
        }
        return Ok(TokenList(slice.to_vec()))
      }

      return Ok(TokenList(tokens))
    })
  }

  fn parse_into<'t>(input: &mut Parser<'i, 't>, tokens: &mut Vec<TokenOrValue<'i>>) -> Result<(), ParseError<'i, ParserError<'i>>> {
    let mut last_is_delim = false;
    let mut last_is_whitespace = false;
    loop {
      let state = input.state();
      match input.next_including_whitespace_and_comments() {
        Ok(&Token::WhiteSpace(..)) | Ok(&Token::Comment(..)) => {
          // Skip whitespace if the last token was a delimeter.
          // Otherwise, replace all whitespace and comments with a single space character.
          if !last_is_delim {
            tokens.push(Token::WhiteSpace(" ").into());
            last_is_whitespace = true;
          }
        }
        Ok(&Token::Function(ref f)) => {
          // Attempt to parse embedded color values into hex tokens.
          let f = f.clone();
          if let Some(color) = try_parse_color_token(&f, &state, input) {
            tokens.push(TokenOrValue::Color(color));
            last_is_delim = false;
            last_is_whitespace = false;
          } else {
            tokens.push(Token::Function(f).into()); 
            input.parse_nested_block(|input| {
              TokenList::parse_into(input, tokens)
            })?;
            tokens.push(Token::CloseParenthesis.into());
            last_is_delim = true; // Whitespace is not required after any of these chars.
            last_is_whitespace = false;  
          }
        }
        Ok(&Token::Hash(ref h)) | Ok(&Token::IDHash(ref h)) => {
          if let Ok(color) = Color::parse_hash(h.as_bytes()) {
            tokens.push(TokenOrValue::Color(CssColor(color)));
          } else {
            tokens.push(Token::Hash(h.clone()).into());
          }
          last_is_delim = false;
          last_is_whitespace = false;
        }
        Ok(token @ &Token::ParenthesisBlock) | 
        Ok(token @ &Token::SquareBracketBlock) | 
        Ok(token @ &Token::CurlyBracketBlock) => {
          tokens.push(token.clone().into()); 
          let closing_delimiter = match token {
            Token::ParenthesisBlock => Token::CloseParenthesis,
            Token::SquareBracketBlock => Token::CloseSquareBracket,
            Token::CurlyBracketBlock => Token::CloseCurlyBracket,
            _ => unreachable!()
          };

          input.parse_nested_block(|input| {
            TokenList::parse_into(input, tokens)
          })?;

          tokens.push(closing_delimiter.into());
          last_is_delim = true; // Whitespace is not required after any of these chars.
          last_is_whitespace = false;  
        }
        Ok(token) => {
          last_is_delim = matches!(token, Token::Delim(_) | Token::Comma);

          // If this is a delimeter, and the last token was whitespace,
          // replace the whitespace with the delimeter since both are not required.
          if last_is_delim && last_is_whitespace {
            let last = tokens.last_mut().unwrap();
            *last = token.clone().into();
          } else {
            tokens.push(token.clone().into());
          }

          last_is_whitespace = false;
        }
        Err(_) => {
          break
        }
      }
    }

    Ok(())
  }
}

#[inline]
fn try_parse_color_token<'i, 't>(f: &CowRcStr<'i>, state: &ParserState, input: &mut Parser<'i, 't>) -> Option<CssColor> {
  match_ignore_ascii_case! { &*f,
    "rgb" | "rgba" | "hsl" | "hsla" | "hwb" => {
      let s = input.state();
      input.reset(&state);
      if let Ok(color) = CssColor::parse(input) {
        return Some(color)
      }
      input.reset(&s);
    },
    _ => {}
  }

  None
}

impl<'i> ToCss for TokenList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use cssparser::ToCss;
    for (i, token_or_value) in self.0.iter().enumerate() {
      match token_or_value {
        TokenOrValue::Color(color) => color.to_css(dest)?,
        TokenOrValue::Token(token) => {
          match token {
            Token::Delim(d) => {
              if *d == '+' || *d == '-' {
                dest.write_char(' ')?;
                dest.write_char(*d)?;
                dest.write_char(' ')?;
              } else {
                let ws_before = *d == '/' || *d == '*';
                dest.delim(*d, ws_before)?;
              }
            }
            Token::Comma => {
              dest.delim(',', false)?;
            }
            Token::CloseParenthesis | Token::CloseSquareBracket | Token::CloseCurlyBracket => {
              token.to_css(dest)?;
              if !dest.minify && i != self.0.len() - 1 {
                // Whitespace is removed during parsing, so add it back if we aren't minifying.
                dest.write_char(' ')?;
              }
            }
            Token::Dimension { value, unit, .. } => {
              serialize_dimension(*value, unit, dest)?;
            }
            _ => {
              token.to_css(dest)?;
            }
          }
    
        }
      }
    }

    Ok(())
  }
}
