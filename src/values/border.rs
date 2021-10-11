use super::length::*;
use cssparser::*;
use super::traits::Parse;
use super::color::CssColor;

#[derive(Debug, Clone, PartialEq)]
pub enum BorderSideWidth {
  /// `thin`
  Thin,
  /// `medium`
  Medium,
  /// `thick`
  Thick,
  /// `<length>`
  Length(Length),
}

impl Parse for BorderSideWidth {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if let Ok(length) = input.try_parse(|i| Length::parse(i)) {
      return Ok(BorderSideWidth::Length(length));
    }
    let ident = input.expect_ident_cloned()?;
    match_ignore_ascii_case! { &ident,
      "thin" => Ok(BorderSideWidth::Thin),
      "medium" => Ok(BorderSideWidth::Medium),
      "thick" => Ok(BorderSideWidth::Thick),
      _ => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }
}

impl ToCss for BorderSideWidth {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    use BorderSideWidth::*;
    match self {
      Thin => dest.write_str("thin"),
      Medium => dest.write_str("medium"),
      Thick => dest.write_str("thick"),
      Length(length) => length.to_css(dest)
    }
  }
}

macro_rules! enum_property {
  ($name: ident, $( $x: ident ),+) => {
    #[derive(Debug, Clone, PartialEq)]
    pub enum $name {
      $(
        $x,
      )+
    }

    impl Parse for $name {
      fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
        let ident = input.expect_ident()?;
        match &ident[..] {
          $(
            s if s.eq_ignore_ascii_case(stringify!($x)) => Ok($name::$x),
          )+
          _ => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
        use $name::*;
        match self {
          $(
            $x => dest.write_str(&stringify!($x).to_lowercase()),
          )+
        }
      }
    }
  };
}

enum_property!(BorderStyle,
  Hidden,
  None,
  Inset,
  Groove,
  Outset,
  Ridge,
  Dotted,
  Dashed,
  Solid,
  Double
);

#[derive(Debug, Clone, PartialEq)]
pub struct Border {
  width: BorderSideWidth,
  style: BorderStyle,
  color: CssColor
}

impl Parse for Border {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    // Order doesn't matter...
    let mut color = None;
    let mut style = None;
    let mut width = None;
    let mut any = false;
    loop {
        if width.is_none() {
            if let Ok(value) = input.try_parse(|i| BorderSideWidth::parse(i)) {
                width = Some(value);
                any = true;
            }
        }
        if style.is_none() {
            if let Ok(value) = input.try_parse(BorderStyle::parse) {
                style = Some(value);
                any = true;
                continue
            }
        }
        if color.is_none() {
            if let Ok(value) = input.try_parse(|i| CssColor::parse(i)) {
                color = Some(value);
                any = true;
                continue
            }
        }
        break
    }
    if any {
      Ok(Border {
        width: width.unwrap_or(BorderSideWidth::Medium),
        style: style.unwrap_or(BorderStyle::None),
        color: color.unwrap_or_else(|| CssColor::current_color())
      })
    } else {
      Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
    }
  }
}

impl ToCss for Border {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    if self.width != BorderSideWidth::Medium {
      self.width.to_css(dest)?;
      dest.write_str(" ")?;
    }
    if self.style != BorderStyle::None {
      self.style.to_css(dest)?;
      dest.write_str(" ")?;
    }
    if self.color != CssColor::current_color() {
      self.color.to_css(dest)?;
    }
    Ok(())
  }
}
