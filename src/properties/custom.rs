use cssparser::*;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::properties::PropertyId;
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
pub struct TokenList<'i>(pub Vec<Token<'i>>);

impl<'i> TokenList<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
      let mut tokens = vec![];
      TokenList::parse_into(input, &mut tokens)?;

      // Slice off leading and trailing whitespace if there are at least two tokens.
      // If there is only one token, we must preserve it. e.g. `--foo: ;` is valid. 
      if tokens.len() >= 2 {
        let mut slice = &tokens[..];
        if matches!(tokens.first(), Some(Token::WhiteSpace(_))) {
          slice = &slice[1..];
        }
        if matches!(tokens.last(), Some(Token::WhiteSpace(_))) {
          slice = &slice[..slice.len() - 1];
        }
        return Ok(TokenList(slice.to_vec()))
      }

      return Ok(TokenList(tokens))
    })
  }

  fn parse_into<'t>(input: &mut Parser<'i, 't>, tokens: &mut Vec<Token<'i>>) -> Result<(), ParseError<'i, ParserError<'i>>> {
    let mut last_is_delim = false;
    let mut last_is_whitespace = false;
    while let Ok(token) = input.next_including_whitespace_and_comments() {
      match token {
        Token::WhiteSpace(..) | Token::Comment(..) => {
          // Skip whitespace if the last token was a delimeter.
          // Otherwise, replace all whitespace and comments with a single space character.
          if !last_is_delim {
            tokens.push(Token::WhiteSpace(" "));
            last_is_whitespace = true;
          }
        }
        Token::Function(_) | Token::ParenthesisBlock | Token::SquareBracketBlock | Token::CurlyBracketBlock => {
          tokens.push(token.clone());
          
          let closing_delimiter = match &token {
            Token::Function(_) | Token::ParenthesisBlock => Token::CloseParenthesis,
            Token::SquareBracketBlock => Token::CloseSquareBracket,
            Token::CurlyBracketBlock => Token::CloseCurlyBracket,
            _ => unreachable!()
          };

          input.parse_nested_block(|input| {
            TokenList::parse_into(input, tokens)
          })?;

          tokens.push(closing_delimiter);
          last_is_delim = true; // Whitespace is not required after any of these chars.
          last_is_whitespace = false;
        }
        _ => {
          last_is_delim = matches!(token, Token::Delim(_) | Token::Comma);

          // If this is a delimeter, and the last token was whitespace,
          // replace the whitespace with the delimeter since both are not required.
          if last_is_delim && last_is_whitespace {
            let last = tokens.last_mut().unwrap();
            *last = token.clone();
          } else {
            tokens.push(token.clone());
          }

          last_is_whitespace = false;
        }
      }
    }

    Ok(())
  }
}

impl<'i> ToCss for TokenList<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use cssparser::ToCss;
    for (i, token) in self.0.iter().enumerate() {
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
        _ => {
          token.to_css(dest)?;
        }
      }
    }

    Ok(())
  }
}
