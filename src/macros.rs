macro_rules! enum_property {
  ($name: ident, $( $x: ident ),+) => {
    #[derive(Debug, Clone, Copy, PartialEq)]
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
      fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
        use $name::*;
        match self {
          $(
            $x => dest.write_str(&stringify!($x).to_lowercase()),
          )+
        }
      }
    }
  };
  ($name: ident, $( ($str: expr, $id: ident) ),+) => {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum $name {
      $(
        $id,
      )+
    }

    impl Parse for $name {
      fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
        let ident = input.expect_ident()?;
        match &ident[..] {
          $(
            s if s.eq_ignore_ascii_case($str) => Ok($name::$id),
          )+
          _ => return Err(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
        }
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
        use $name::*;
        match self {
          $(
            $id => dest.write_str($str),
          )+
        }
      }
    }
  };
}

pub(crate) use enum_property;
