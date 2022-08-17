//! CSS custom properties and unparsed token values.

use crate::compat;
use crate::error::{ParserError, PrinterError, PrinterErrorKind};
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::PropertyId;
use crate::rules::supports::SupportsCondition;
use crate::stylesheet::ParserOptions;
use crate::targets::Browsers;
use crate::traits::{Parse, ParseWithOptions, ToCss};
use crate::values::color::{
  parse_hsl_hwb_components, parse_rgb_components, ColorFallbackKind, ComponentParser, CssColor,
};
use crate::values::ident::DashedIdentReference;
use crate::values::length::serialize_dimension;
use crate::values::percentage::Percentage;
use crate::values::string::CowArcStr;
use crate::values::url::Url;
use crate::vendor_prefix::VendorPrefix;
use cssparser::*;

/// A CSS custom property, representing any unknown property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CustomProperty<'i> {
  /// The name of the property.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: CowArcStr<'i>,
  /// The property value, stored as a raw token list.
  pub value: TokenList<'i>,
}

impl<'i> CustomProperty<'i> {
  /// Parses a custom property with the given name.
  pub fn parse<'t>(
    name: CowArcStr<'i>,
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
      TokenList::parse(input, options, 0)
    })?;
    Ok(CustomProperty { name, value })
  }
}

/// A known property with an unparsed value.
///
/// This type is used when the value of a known property could not
/// be parsed, e.g. in the case css `var()` references are encountered.
/// In this case, the raw tokens are stored instead.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct UnparsedProperty<'i> {
  /// The id of the property.
  pub property_id: PropertyId<'i>,
  /// The property value, stored as a raw token list.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub value: TokenList<'i>,
}

impl<'i> UnparsedProperty<'i> {
  /// Parses a property with the given id as a token list.
  pub fn parse<'t>(
    property_id: PropertyId<'i>,
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let value = input.parse_until_before(Delimiter::Bang | Delimiter::Semicolon, |input| {
      TokenList::parse(input, options, 0)
    })?;
    Ok(UnparsedProperty { property_id, value })
  }

  pub(crate) fn get_prefixed(&self, targets: Option<Browsers>, feature: Feature) -> UnparsedProperty<'i> {
    let mut clone = self.clone();
    if self.property_id.prefix().contains(VendorPrefix::None) {
      if let Some(targets) = targets {
        clone.property_id = clone.property_id.with_prefix(feature.prefixes_for(targets))
      }
    }
    clone
  }

  /// Returns a new UnparsedProperty with the same value and the given property id.
  pub fn with_property_id(&self, property_id: PropertyId<'i>) -> UnparsedProperty<'i> {
    UnparsedProperty {
      property_id,
      value: self.value.clone(),
    }
  }
}

/// A raw list of CSS tokens, with embedded parsed values.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TokenList<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub Vec<TokenOrValue<'i>>);

/// A raw CSS token, or a parsed value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum TokenOrValue<'i> {
  /// A token.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Token(Token<'i>),
  /// A parsed CSS color.
  Color(CssColor),
  /// A color with unresolved components.
  UnresolvedColor(UnresolvedColor<'i>),
  /// A parsed CSS url.
  Url(Url<'i>),
  /// A CSS variable reference.
  Var(Variable<'i>),
}

impl<'i> From<Token<'i>> for TokenOrValue<'i> {
  fn from(token: Token<'i>) -> TokenOrValue<'i> {
    TokenOrValue::Token(token)
  }
}

impl<'i> TokenOrValue<'i> {
  /// Returns whether the token is whitespace.
  pub fn is_whitespace(&self) -> bool {
    matches!(self, TokenOrValue::Token(Token::WhiteSpace(_)))
  }
}

impl<'i> TokenList<'i> {
  pub(crate) fn parse<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
    depth: usize,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut tokens = vec![];
    TokenList::parse_into(input, &mut tokens, options, depth)?;

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
      return Ok(TokenList(slice.to_vec()));
    }

    return Ok(TokenList(tokens));
  }

  fn parse_into<'t>(
    input: &mut Parser<'i, 't>,
    tokens: &mut Vec<TokenOrValue<'i>>,
    options: &ParserOptions,
    depth: usize,
  ) -> Result<(), ParseError<'i, ParserError<'i>>> {
    if depth > 500 {
      return Err(input.new_custom_error(ParserError::MaximumNestingDepth));
    }

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
          } else if let Ok(color) = input.try_parse(|input| UnresolvedColor::parse(&f, input, options)) {
            tokens.push(TokenOrValue::UnresolvedColor(color));
            last_is_delim = true;
            last_is_whitespace = false;
          } else if f == "url" {
            input.reset(&state);
            tokens.push(TokenOrValue::Url(Url::parse(input)?));
            last_is_delim = false;
            last_is_whitespace = false;
          } else if f == "var" {
            let var = input.parse_nested_block(|input| {
              let var = Variable::parse(input, options, depth + 1)?;
              Ok(TokenOrValue::Var(var))
            })?;
            tokens.push(var);
            last_is_delim = true;
            last_is_whitespace = false;
          } else {
            tokens.push(Token::Function(f).into());
            input.parse_nested_block(|input| TokenList::parse_into(input, tokens, options, depth + 1))?;
            tokens.push(Token::CloseParenthesis.into());
            last_is_delim = true; // Whitespace is not required after any of these chars.
            last_is_whitespace = false;
          }
        }
        Ok(&cssparser::Token::Hash(ref h)) | Ok(&cssparser::Token::IDHash(ref h)) => {
          if let Ok(color) = Color::parse_hash(h.as_bytes()) {
            tokens.push(TokenOrValue::Color(color.into()));
          } else {
            tokens.push(Token::Hash(h.into()).into());
          }
          last_is_delim = false;
          last_is_whitespace = false;
        }
        Ok(&cssparser::Token::UnquotedUrl(_)) => {
          input.reset(&state);
          tokens.push(TokenOrValue::Url(Url::parse(input)?));
          last_is_delim = false;
          last_is_whitespace = false;
        }
        Ok(token @ &cssparser::Token::ParenthesisBlock)
        | Ok(token @ &cssparser::Token::SquareBracketBlock)
        | Ok(token @ &cssparser::Token::CurlyBracketBlock) => {
          tokens.push(Token::from(token).into());
          let closing_delimiter = match token {
            cssparser::Token::ParenthesisBlock => Token::CloseParenthesis,
            cssparser::Token::SquareBracketBlock => Token::CloseSquareBracket,
            cssparser::Token::CurlyBracketBlock => Token::CloseCurlyBracket,
            _ => unreachable!(),
          };

          input.parse_nested_block(|input| TokenList::parse_into(input, tokens, options, depth + 1))?;

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
        Err(_) => break,
      }
    }

    Ok(())
  }
}

#[inline]
fn try_parse_color_token<'i, 't>(
  f: &CowArcStr<'i>,
  state: &ParserState,
  input: &mut Parser<'i, 't>,
) -> Option<CssColor> {
  match_ignore_ascii_case! { &*f,
    "rgb" | "rgba" | "hsl" | "hsla" | "hwb" | "lab" | "lch" | "oklab" | "oklch" | "color" | "color-mix" => {
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

impl<'i> TokenList<'i> {
  pub(crate) fn to_css<W>(&self, dest: &mut Printer<W>, is_custom_property: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if !dest.minify && self.0.len() == 1 && matches!(self.0.first(), Some(token) if token.is_whitespace()) {
      return Ok(());
    }

    let mut has_whitespace = false;
    for (i, token_or_value) in self.0.iter().enumerate() {
      has_whitespace = match token_or_value {
        TokenOrValue::Color(color) => {
          color.to_css(dest)?;
          false
        }
        TokenOrValue::UnresolvedColor(color) => {
          color.to_css(dest, is_custom_property)?;
          false
        }
        TokenOrValue::Url(url) => {
          if dest.dependencies.is_some() && is_custom_property && !url.is_absolute() {
            return Err(dest.error(
              PrinterErrorKind::AmbiguousUrlInCustomProperty {
                url: url.url.as_ref().to_owned(),
              },
              url.loc,
            ));
          }
          url.to_css(dest)?;
          false
        }
        TokenOrValue::Var(var) => {
          var.to_css(dest, is_custom_property)?;
          if !dest.minify
            && i != self.0.len() - 1
            && !matches!(
              self.0[i + 1],
              TokenOrValue::Token(Token::Comma) | TokenOrValue::Token(Token::CloseParenthesis)
            )
          {
            // Whitespace is removed during parsing, so add it back if we aren't minifying.
            dest.write_char(' ')?;
            true
          } else {
            false
          }
        }
        TokenOrValue::Token(token) => {
          match token {
            Token::Delim(d) => {
              if *d == '+' || *d == '-' {
                dest.write_char(' ')?;
                dest.write_char(*d)?;
                dest.write_char(' ')?;
              } else {
                let ws_before = !has_whitespace && (*d == '/' || *d == '*');
                dest.delim(*d, ws_before)?;
              }
              true
            }
            Token::Comma => {
              dest.delim(',', false)?;
              true
            }
            Token::CloseParenthesis | Token::CloseSquareBracket | Token::CloseCurlyBracket => {
              token.to_css(dest)?;
              if !dest.minify
                && i != self.0.len() - 1
                && !matches!(self.0[i + 1], TokenOrValue::Token(Token::Comma))
              {
                // Whitespace is removed during parsing, so add it back if we aren't minifying.
                dest.write_char(' ')?;
                true
              } else {
                false
              }
            }
            Token::Dimension { value, unit, .. } => {
              serialize_dimension(*value, unit, dest)?;
              false
            }
            Token::Number { value, .. } => {
              value.to_css(dest)?;
              false
            }
            _ => {
              token.to_css(dest)?;
              matches!(token, Token::WhiteSpace(..))
            }
          }
        }
      };
    }

    Ok(())
  }
}

/// A raw CSS token.
// Copied from cssparser to change CowRcStr to CowArcStr
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Token<'a> {
  /// A [`<ident-token>`](https://drafts.csswg.org/css-syntax/#ident-token-diagram)
  Ident(#[cfg_attr(feature = "serde", serde(borrow))] CowArcStr<'a>),

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
  String(CowArcStr<'a>),

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
      cssparser::Token::QuotedString(x) => Token::String(x.into()),
      cssparser::Token::UnquotedUrl(x) => Token::UnquotedUrl(x.into()),
      cssparser::Token::Function(x) => Token::Function(x.into()),
      cssparser::Token::BadUrl(x) => Token::BadUrl(x.into()),
      cssparser::Token::BadString(x) => Token::BadString(x.into()),
      cssparser::Token::Delim(c) => Token::Delim(*c),
      cssparser::Token::Number {
        has_sign,
        value,
        int_value,
      } => Token::Number {
        has_sign: *has_sign,
        value: *value,
        int_value: *int_value,
      },
      cssparser::Token::Dimension {
        has_sign,
        value,
        int_value,
        unit,
      } => Token::Dimension {
        has_sign: *has_sign,
        value: *value,
        int_value: *int_value,
        unit: unit.into(),
      },
      cssparser::Token::Percentage {
        has_sign,
        unit_value,
        int_value,
      } => Token::Percentage {
        has_sign: *has_sign,
        unit_value: *unit_value,
        int_value: *int_value,
      },
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
      cssparser::Token::CloseCurlyBracket => Token::CloseCurlyBracket,
    }
  }
}

impl<'a> ToCss for Token<'a> {
  #[inline]
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use cssparser::ToCss;
    match self {
      Token::Ident(x) => cssparser::Token::Ident(x.as_ref().into()).to_css(dest)?,
      Token::AtKeyword(x) => cssparser::Token::AtKeyword(x.as_ref().into()).to_css(dest)?,
      Token::Hash(x) => cssparser::Token::Hash(x.as_ref().into()).to_css(dest)?,
      Token::IDHash(x) => cssparser::Token::IDHash(x.as_ref().into()).to_css(dest)?,
      Token::String(x) => cssparser::Token::QuotedString(x.as_ref().into()).to_css(dest)?,
      Token::UnquotedUrl(x) => cssparser::Token::UnquotedUrl(x.as_ref().into()).to_css(dest)?,
      Token::Function(x) => cssparser::Token::Function(x.as_ref().into()).to_css(dest)?,
      Token::BadUrl(x) => cssparser::Token::BadUrl(x.as_ref().into()).to_css(dest)?,
      Token::BadString(x) => cssparser::Token::BadString(x.as_ref().into()).to_css(dest)?,
      Token::Delim(c) => cssparser::Token::Delim(*c).to_css(dest)?,
      Token::Number {
        has_sign,
        value,
        int_value,
      } => cssparser::Token::Number {
        has_sign: *has_sign,
        value: *value,
        int_value: *int_value,
      }
      .to_css(dest)?,
      Token::Dimension {
        has_sign,
        value,
        int_value,
        unit,
      } => cssparser::Token::Dimension {
        has_sign: *has_sign,
        value: *value,
        int_value: *int_value,
        unit: unit.as_ref().into(),
      }
      .to_css(dest)?,
      Token::Percentage {
        has_sign,
        unit_value,
        int_value,
      } => cssparser::Token::Percentage {
        has_sign: *has_sign,
        unit_value: *unit_value,
        int_value: *int_value,
      }
      .to_css(dest)?,
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
      Token::CloseCurlyBracket => cssparser::Token::CloseCurlyBracket.to_css(dest)?,
    }

    Ok(())
  }
}

impl<'i> TokenList<'i> {
  pub(crate) fn get_necessary_fallbacks(&self, targets: Browsers) -> ColorFallbackKind {
    let mut fallbacks = ColorFallbackKind::empty();
    for token in &self.0 {
      if let TokenOrValue::Color(color) = token {
        fallbacks |= color.get_possible_fallbacks(targets);
      }
    }

    fallbacks
  }

  pub(crate) fn get_fallback(&self, kind: ColorFallbackKind) -> Self {
    let tokens = self
      .0
      .iter()
      .map(|token| match token {
        TokenOrValue::Color(color) => TokenOrValue::Color(color.get_fallback(kind)),
        _ => token.clone(),
      })
      .collect();
    TokenList(tokens)
  }

  pub(crate) fn get_fallbacks(&mut self, targets: Browsers) -> Vec<(SupportsCondition<'i>, Self)> {
    // Get the full list of possible fallbacks, and remove the lowest one, which will replace
    // the original declaration. The remaining fallbacks need to be added as @supports rules.
    let mut fallbacks = self.get_necessary_fallbacks(targets);
    let lowest_fallback = fallbacks.lowest();
    fallbacks.remove(lowest_fallback);

    let mut res = Vec::new();
    if fallbacks.contains(ColorFallbackKind::P3) {
      res.push((
        ColorFallbackKind::P3.supports_condition(),
        self.get_fallback(ColorFallbackKind::P3),
      ));
    }

    if fallbacks.contains(ColorFallbackKind::LAB) {
      res.push((
        ColorFallbackKind::LAB.supports_condition(),
        self.get_fallback(ColorFallbackKind::LAB),
      ));
    }

    if !lowest_fallback.is_empty() {
      for token in self.0.iter_mut() {
        if let TokenOrValue::Color(color) = token {
          *color = color.get_fallback(lowest_fallback);
        }
      }
    }

    res
  }
}

/// A CSS variable reference.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Variable<'i> {
  /// The variable name.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdentReference<'i>,
  /// A fallback value in case the variable is not defined.
  pub fallback: Option<TokenList<'i>>,
}

impl<'i> Variable<'i> {
  fn parse<'t>(
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
    depth: usize,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let name = DashedIdentReference::parse_with_options(input, options)?;

    let fallback = if input.try_parse(|input| input.expect_comma()).is_ok() {
      Some(TokenList::parse(input, options, depth)?)
    } else {
      None
    };

    Ok(Variable { name, fallback })
  }

  fn to_css<W>(&self, dest: &mut Printer<W>, is_custom_property: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_str("var(")?;
    self.name.to_css(dest)?;
    if let Some(fallback) = &self.fallback {
      dest.delim(',', false)?;
      fallback.to_css(dest, is_custom_property)?;
    }
    dest.write_char(')')
  }
}

/// A color value with an unresolved alpha value (e.g. a variable).
/// These can be converted from the modern slash syntax to older comma syntax.
/// This can only be done when the only unresolved component is the alpha
/// since variables can resolve to multiple tokens.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "lowercase")
)]
pub enum UnresolvedColor<'i> {
  /// An rgb() color.
  RGB {
    /// The red component.
    r: f32,
    /// The green component.
    g: f32,
    /// The blue component.
    b: f32,
    /// The unresolved alpha component.
    #[cfg_attr(feature = "serde", serde(borrow))]
    alpha: TokenList<'i>,
  },
  /// An hsl() color.
  HSL {
    /// The hue component.
    h: f32,
    /// The saturation component.
    s: f32,
    /// The lightness component.
    l: f32,
    /// The unresolved alpha component.
    #[cfg_attr(feature = "serde", serde(borrow))]
    alpha: TokenList<'i>,
  },
}

impl<'i> UnresolvedColor<'i> {
  fn parse<'t>(
    f: &CowArcStr<'i>,
    input: &mut Parser<'i, 't>,
    options: &ParserOptions,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let parser = ComponentParser { allow_none: false };
    match_ignore_ascii_case! { &*f,
      "rgb" => {
        input.parse_nested_block(|input| {
          let (r, g, b) = parse_rgb_components(input, &parser)?;
          input.expect_delim('/')?;
          let alpha = TokenList::parse(input, options, 0)?;
          Ok(UnresolvedColor::RGB { r, g, b, alpha })
        })
      },
      "hsl" => {
        input.parse_nested_block(|input| {
          let (h, s, l) = parse_hsl_hwb_components(input, &parser)?;
          input.expect_delim('/')?;
          let alpha = TokenList::parse(input, options, 0)?;
          Ok(UnresolvedColor::HSL { h, s, l, alpha })
        })
      },
      _ => Err(input.new_custom_error(ParserError::InvalidValue))
    }
  }

  fn to_css<W>(&self, dest: &mut Printer<W>, is_custom_property: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[inline]
    fn c(c: &f32) -> i32 {
      (c * 255.0).round().clamp(0.0, 255.0) as i32
    }

    match self {
      UnresolvedColor::RGB { r, g, b, alpha } => {
        if let Some(targets) = dest.targets {
          if !compat::Feature::SpaceSeparatedColorFunction.is_compatible(targets) {
            dest.write_str("rgba(")?;
            c(r).to_css(dest)?;
            dest.delim(',', false)?;
            c(g).to_css(dest)?;
            dest.delim(',', false)?;
            c(b).to_css(dest)?;
            dest.delim(',', false)?;
            alpha.to_css(dest, is_custom_property)?;
            dest.write_char(')')?;
            return Ok(());
          }
        }

        dest.write_str("rgb(")?;
        c(r).to_css(dest)?;
        dest.write_char(' ')?;
        c(g).to_css(dest)?;
        dest.write_char(' ')?;
        c(b).to_css(dest)?;
        dest.delim('/', true)?;
        alpha.to_css(dest, is_custom_property)?;
        dest.write_char(')')
      }
      UnresolvedColor::HSL { h, s, l, alpha } => {
        if let Some(targets) = dest.targets {
          if !compat::Feature::SpaceSeparatedColorFunction.is_compatible(targets) {
            dest.write_str("hsla(")?;
            h.to_css(dest)?;
            dest.delim(',', false)?;
            Percentage(*s).to_css(dest)?;
            dest.delim(',', false)?;
            Percentage(*l).to_css(dest)?;
            dest.delim(',', false)?;
            alpha.to_css(dest, is_custom_property)?;
            dest.write_char(')')?;
            return Ok(());
          }
        }

        dest.write_str("hsl(")?;
        h.to_css(dest)?;
        dest.write_char(' ')?;
        Percentage(*s).to_css(dest)?;
        dest.write_char(' ')?;
        Percentage(*l).to_css(dest)?;
        dest.delim('/', true)?;
        alpha.to_css(dest, is_custom_property)?;
        dest.write_char(')')
      }
    }
  }
}
