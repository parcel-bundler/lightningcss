use crate::values::string::CowArcStr;
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
  pub name: CowArcStr<'i>,
  pub value: TokenList<'i>
}

impl<'i> CustomProperty<'i> {
  pub fn parse<'t>(
    name: CowRcStr<'i>,
    input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = TokenList::parse(input)?;
    Ok(CustomProperty {
      name: name.into(),
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
        Ok(&cssparser::Token::WhiteSpace(..)) | Ok(&cssparser::Token::Comment(..)) => {
          // Skip whitespace if the last token was a delimeter.
          // Otherwise, replace all whitespace and comments with a single space character.
          if !last_is_delim {
            tokens.push(Token::WhiteSpace(" ").into());
            last_is_whitespace = true;
          }
        }
        Ok(&cssparser::Token::Function(ref f)) => {
          // Attempt to parse embedded color values into hex tokens.
          let f = f.into();
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
        Ok(&cssparser::Token::Hash(ref h)) | Ok(&cssparser::Token::IDHash(ref h)) => {
          if let Ok(color) = Color::parse_hash(h.as_bytes()) {
            tokens.push(TokenOrValue::Color(CssColor(color)));
          } else {
            tokens.push(Token::Hash(h.into()).into());
          }
          last_is_delim = false;
          last_is_whitespace = false;
        }
        Ok(token @ &cssparser::Token::ParenthesisBlock) | 
        Ok(token @ &cssparser::Token::SquareBracketBlock) | 
        Ok(token @ &cssparser::Token::CurlyBracketBlock) => {
          tokens.push(Token::from(token).into()); 
          let closing_delimiter = match token {
            cssparser::Token::ParenthesisBlock => Token::CloseParenthesis,
            cssparser::Token::SquareBracketBlock => Token::CloseSquareBracket,
            cssparser::Token::CurlyBracketBlock => Token::CloseCurlyBracket,
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
          last_is_delim = matches!(token, cssparser::Token::Delim(_) | cssparser::Token::Comma);

          // If this is a delimeter, and the last token was whitespace,
          // replace the whitespace with the delimeter since both are not required.
          if last_is_delim && last_is_whitespace {
            let last = tokens.last_mut().unwrap();
            *last = Token::from(token).into();
          } else {
            tokens.push(Token::from(token).into());
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
fn try_parse_color_token<'i, 't>(f: &CowArcStr<'i>, state: &ParserState, input: &mut Parser<'i, 't>) -> Option<CssColor> {
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

// Copied from cssparser to change CowRcStr to Cow<str>
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
  /// A [`<ident-token>`](https://drafts.csswg.org/css-syntax/#ident-token-diagram)
  Ident(CowArcStr<'a>),

  /// A [`<at-keyword-token>`](https://drafts.csswg.org/css-syntax/#at-keyword-token-diagram)
  ///
  /// The value does not include the `@` marker.
  AtKeyword(CowArcStr<'a>),

  /// A [`<hash-token>`](https://drafts.csswg.org/css-syntax/#hash-token-diagram) with the type flag set to "unrestricted"
  ///
  /// The value does not include the `#` marker.
  Hash(CowArcStr<'a>),

  /// A [`<hash-token>`](https://drafts.csswg.org/css-syntax/#hash-token-diagram) with the type flag set to "id"
  ///
  /// The value does not include the `#` marker.
  IDHash(CowArcStr<'a>), // Hash that is a valid ID selector.

  /// A [`<string-token>`](https://drafts.csswg.org/css-syntax/#string-token-diagram)
  ///
  /// The value does not include the quotes.
  QuotedString(CowArcStr<'a>),

  /// A [`<url-token>`](https://drafts.csswg.org/css-syntax/#url-token-diagram)
  ///
  /// The value does not include the `url(` `)` markers.  Note that `url( <string-token> )` is represented by a
  /// `Function` token.
  UnquotedUrl(CowArcStr<'a>),

  /// A `<delim-token>`
  Delim(char),

  /// A [`<number-token>`](https://drafts.csswg.org/css-syntax/#number-token-diagram)
  Number {
      /// Whether the number had a `+` or `-` sign.
      ///
      /// This is used is some cases like the <An+B> micro syntax. (See the `parse_nth` function.)
      has_sign: bool,

      /// The value as a float
      value: f32,

      /// If the origin source did not include a fractional part, the value as an integer.
      int_value: Option<i32>,
  },

  /// A [`<percentage-token>`](https://drafts.csswg.org/css-syntax/#percentage-token-diagram)
  Percentage {
    /// Whether the number had a `+` or `-` sign.
    has_sign: bool,

    /// The value as a float, divided by 100 so that the nominal range is 0.0 to 1.0.
    unit_value: f32,

    /// If the origin source did not include a fractional part, the value as an integer.
    /// It is **not** divided by 100.
    int_value: Option<i32>,
  },

  /// A [`<dimension-token>`](https://drafts.csswg.org/css-syntax/#dimension-token-diagram)
  Dimension {
    /// Whether the number had a `+` or `-` sign.
    ///
    /// This is used is some cases like the <An+B> micro syntax. (See the `parse_nth` function.)
    has_sign: bool,

    /// The value as a float
    value: f32,

    /// If the origin source did not include a fractional part, the value as an integer.
    int_value: Option<i32>,

    /// The unit, e.g. "px" in `12px`
    unit: CowArcStr<'a>,
  },

  /// A [`<whitespace-token>`](https://drafts.csswg.org/css-syntax/#whitespace-token-diagram)
  WhiteSpace(&'a str),

  /// A comment.
  ///
  /// The CSS Syntax spec does not generate tokens for comments,
  /// But we do, because we can (borrowed &str makes it cheap).
  ///
  /// The value does not include the `/*` `*/` markers.
  Comment(&'a str),

  /// A `:` `<colon-token>`
  Colon, // :

  /// A `;` `<semicolon-token>`
  Semicolon, // ;

  /// A `,` `<comma-token>`
  Comma, // ,

  /// A `~=` [`<include-match-token>`](https://drafts.csswg.org/css-syntax/#include-match-token-diagram)
  IncludeMatch,

  /// A `|=` [`<dash-match-token>`](https://drafts.csswg.org/css-syntax/#dash-match-token-diagram)
  DashMatch,

  /// A `^=` [`<prefix-match-token>`](https://drafts.csswg.org/css-syntax/#prefix-match-token-diagram)
  PrefixMatch,

  /// A `$=` [`<suffix-match-token>`](https://drafts.csswg.org/css-syntax/#suffix-match-token-diagram)
  SuffixMatch,

  /// A `*=` [`<substring-match-token>`](https://drafts.csswg.org/css-syntax/#substring-match-token-diagram)
  SubstringMatch,

  /// A `<!--` [`<CDO-token>`](https://drafts.csswg.org/css-syntax/#CDO-token-diagram)
  CDO,

  /// A `-->` [`<CDC-token>`](https://drafts.csswg.org/css-syntax/#CDC-token-diagram)
  CDC,

  /// A [`<function-token>`](https://drafts.csswg.org/css-syntax/#function-token-diagram)
  ///
  /// The value (name) does not include the `(` marker.
  Function(CowArcStr<'a>),

  /// A `<(-token>`
  ParenthesisBlock,

  /// A `<[-token>`
  SquareBracketBlock,

  /// A `<{-token>`
  CurlyBracketBlock,

  /// A `<bad-url-token>`
  ///
  /// This token always indicates a parse error.
  BadUrl(CowArcStr<'a>),

  /// A `<bad-string-token>`
  ///
  /// This token always indicates a parse error.
  BadString(CowArcStr<'a>),

  /// A `<)-token>`
  ///
  /// When obtained from one of the `Parser::next*` methods,
  /// this token is always unmatched and indicates a parse error.
  CloseParenthesis,

  /// A `<]-token>`
  ///
  /// When obtained from one of the `Parser::next*` methods,
  /// this token is always unmatched and indicates a parse error.
  CloseSquareBracket,

  /// A `<}-token>`
  ///
  /// When obtained from one of the `Parser::next*` methods,
  /// this token is always unmatched and indicates a parse error.
  CloseCurlyBracket,
}

impl<'a> From<&cssparser::Token<'a>> for Token<'a> {
  #[inline]
  fn from(t: &cssparser::Token<'a>) -> Token<'a> {
    match t {
      cssparser::Token::Ident(x) => Token::Ident(x.into()),
      cssparser::Token::AtKeyword(x) => Token::AtKeyword(x.into()),
      cssparser::Token::Hash(x) => Token::Hash(x.into()),
      cssparser::Token::IDHash(x) => Token::IDHash(x.into()),
      cssparser::Token::QuotedString(x) => Token::QuotedString(x.into()),
      cssparser::Token::UnquotedUrl(x) => Token::UnquotedUrl(x.into()),
      cssparser::Token::Function(x) => Token::Function(x.into()),
      cssparser::Token::BadUrl(x) => Token::BadUrl(x.into()),
      cssparser::Token::BadString(x) => Token::BadString(x.into()),
      cssparser::Token::Delim(c) => Token::Delim(*c),
      cssparser::Token::Number { has_sign, value, int_value } => Token::Number { has_sign: *has_sign, value: *value, int_value: *int_value },
      cssparser::Token::Dimension { has_sign, value, int_value, unit } => Token::Dimension { has_sign: *has_sign, value: *value, int_value: *int_value, unit: unit.into() },
      cssparser::Token::Percentage { has_sign, unit_value, int_value } => Token::Percentage { has_sign: *has_sign, unit_value: *unit_value, int_value: *int_value },
      cssparser::Token::WhiteSpace(w) => Token::WhiteSpace(w),
      cssparser::Token::Comment(c) => Token::Comment(c),
      cssparser::Token::Colon => Token::Colon,
      cssparser::Token::Semicolon => Token::Semicolon,
      cssparser::Token::Comma => Token::Comma,
      cssparser::Token::IncludeMatch => Token::IncludeMatch,
      cssparser::Token::DashMatch => Token::DashMatch,
      cssparser::Token::PrefixMatch => Token::PrefixMatch,
      cssparser::Token::SuffixMatch => Token::SuffixMatch,
      cssparser::Token::SubstringMatch => Token::SubstringMatch,
      cssparser::Token::CDO => Token::CDO,
      cssparser::Token::CDC => Token::CDC,
      cssparser::Token::ParenthesisBlock => Token::ParenthesisBlock,
      cssparser::Token::SquareBracketBlock => Token::SquareBracketBlock,
      cssparser::Token::CurlyBracketBlock => Token::CurlyBracketBlock,
      cssparser::Token::CloseParenthesis => Token::CloseParenthesis,
      cssparser::Token::CloseSquareBracket => Token::CloseSquareBracket,
      cssparser::Token::CloseCurlyBracket => Token::CloseCurlyBracket
    }
  }
}

impl<'a> ToCss for Token<'a> {
  #[inline]
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use cssparser::ToCss;
    match self {
      Token::Ident(x) => cssparser::Token::Ident(x.as_ref().into()).to_css(dest)?,
      Token::AtKeyword(x) => cssparser::Token::AtKeyword(x.as_ref().into()).to_css(dest)?,
      Token::Hash(x) => cssparser::Token::Hash(x.as_ref().into()).to_css(dest)?,
      Token::IDHash(x) => cssparser::Token::IDHash(x.as_ref().into()).to_css(dest)?,
      Token::QuotedString(x) => cssparser::Token::QuotedString(x.as_ref().into()).to_css(dest)?,
      Token::UnquotedUrl(x) => cssparser::Token::UnquotedUrl(x.as_ref().into()).to_css(dest)?,
      Token::Function(x) => cssparser::Token::Function(x.as_ref().into()).to_css(dest)?,
      Token::BadUrl(x) => cssparser::Token::BadUrl(x.as_ref().into()).to_css(dest)?,
      Token::BadString(x) => cssparser::Token::BadString(x.as_ref().into()).to_css(dest)?,
      Token::Delim(c) => cssparser::Token::Delim(*c).to_css(dest)?,
      Token::Number { has_sign, value, int_value } => cssparser::Token::Number { has_sign: *has_sign, value: *value, int_value: *int_value }.to_css(dest)?,
      Token::Dimension { has_sign, value, int_value, unit } => cssparser::Token::Dimension { has_sign: *has_sign, value: *value, int_value: *int_value, unit: unit.as_ref().into() }.to_css(dest)?,
      Token::Percentage { has_sign, unit_value, int_value } => cssparser::Token::Percentage { has_sign: *has_sign, unit_value: *unit_value, int_value: *int_value }.to_css(dest)?,
      Token::WhiteSpace(w) => cssparser::Token::WhiteSpace(w).to_css(dest)?,
      Token::Comment(c) => cssparser::Token::Comment(c).to_css(dest)?,
      Token::Colon => cssparser::Token::Colon.to_css(dest)?,
      Token::Semicolon => cssparser::Token::Semicolon.to_css(dest)?,
      Token::Comma => cssparser::Token::Comma.to_css(dest)?,
      Token::IncludeMatch => cssparser::Token::IncludeMatch.to_css(dest)?,
      Token::DashMatch => cssparser::Token::DashMatch.to_css(dest)?,
      Token::PrefixMatch => cssparser::Token::PrefixMatch.to_css(dest)?,
      Token::SuffixMatch => cssparser::Token::SuffixMatch.to_css(dest)?,
      Token::SubstringMatch => cssparser::Token::SubstringMatch.to_css(dest)?,
      Token::CDO => cssparser::Token::CDO.to_css(dest)?,
      Token::CDC => cssparser::Token::CDC.to_css(dest)?,
      Token::ParenthesisBlock => cssparser::Token::ParenthesisBlock.to_css(dest)?,
      Token::SquareBracketBlock => cssparser::Token::SquareBracketBlock.to_css(dest)?,
      Token::CurlyBracketBlock => cssparser::Token::CurlyBracketBlock.to_css(dest)?,
      Token::CloseParenthesis => cssparser::Token::CloseParenthesis.to_css(dest)?,
      Token::CloseSquareBracket => cssparser::Token::CloseSquareBracket.to_css(dest)?,
      Token::CloseCurlyBracket => cssparser::Token::CloseCurlyBracket.to_css(dest)?
    }

    Ok(())
  }
}
