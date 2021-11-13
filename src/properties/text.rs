use cssparser::*;
use crate::traits::{Parse, ToCss};
use crate::macros::enum_property;
use crate::printer::Printer;
use bitflags::bitflags;
use std::fmt::Write;

// https://www.w3.org/TR/2021/CRD-css-text-3-20210422/#text-transform-property
enum_property!(TextTransformCase,
  None,
  Uppercase,
  Lowercase,
  Capitalize
);

impl Default for TextTransformCase {
  fn default() -> TextTransformCase {
    TextTransformCase::None
  }
}

bitflags! {
  pub struct TextTransformOther: u8 {
    const FullWidth    = 0b00000001;
    const FullSizeKana = 0b00000010;
  }
}

impl Parse for TextTransformOther {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &ident,
      "full-width" => Ok(TextTransformOther::FullWidth),
      "full-size-kana" => Ok(TextTransformOther::FullSizeKana),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for TextTransformOther {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut needs_space = false;
    if self.contains(TextTransformOther::FullWidth) {
      dest.write_str("full-width")?;
      needs_space = true;
    }

    if self.contains(TextTransformOther::FullSizeKana) {
      if needs_space {
        dest.write_char(' ')?;
      }
      dest.write_str("full-size-kana")?;
    }

    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextTransform {
  case: TextTransformCase,
  other: TextTransformOther
}

impl Parse for TextTransform {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut case = None;
    let mut other = TextTransformOther::empty();

    loop {
      if case.is_none() {
        if let Ok(c) = input.try_parse(TextTransformCase::parse) {
          case = Some(c);
          if c == TextTransformCase::None {
            other = TextTransformOther::empty();
            break
          }
          continue
        }
      }

      if let Ok(o) = input.try_parse(TextTransformOther::parse) {
        other |= o;
        continue
      }

      break
    }

    Ok(TextTransform {
      case: case.unwrap_or_default(),
      other
    })
  }
}

impl ToCss for TextTransform {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    let mut needs_space = false;
    if self.case != TextTransformCase::None || self.other.is_empty() {
      self.case.to_css(dest)?;
      needs_space = true;
    }

    if !self.other.is_empty() {
      if needs_space {
        dest.write_char(' ')?;
      }
      self.other.to_css(dest)?;
    }
    Ok(())
  }
}

enum_property!(WhiteSpace,
  ("normal", Normal),
  ("pre", Pre),
  ("nowrap", NoWrap),
  ("pre-wrap", PreWrap),
  ("break-spaces", BreakSpaces),
  ("pre-line", PreLine)
);

enum_property!(WordBreak,
  ("normal", Normal),
  ("keep-all", KeepAll),
  ("break-all", BreakAll),
  ("break-word", BreakWord)
);

enum_property!(LineBreak,
  Auto,
  Loose,
  Normal,
  Strict,
  Anywhere
);

enum_property!(Hyphens,
  None,
  Manual,
  Auto
);

enum_property!(OverflowWrap,
  ("normal", Normal),
  ("break-word", BreakWord),
  ("anywhere", Anywhere)
);
