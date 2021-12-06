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

    impl $name {
      #[allow(dead_code)]
      pub fn from_str(s: &str) -> Option<Self> {
        match s {
          $(
            s if s.eq_ignore_ascii_case(stringify!($x)) => Some($name::$x),
          )+
          _ => None
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

    impl $name {
      #[allow(dead_code)]
      pub fn from_str(s: &str) -> Option<Self> {
        match s {
          $(
            s if s.eq_ignore_ascii_case($str) => Some($name::$id),
          )+
          _ => None
        }
      }
    }
  };
}

pub(crate) use enum_property;

macro_rules! shorthand_property {
  (
    $name: ident
    { $first_key: ident: $first_type: ty, $( $key: ident: $type: ty, )* }
  ) => {
    #[derive(Debug, Clone, PartialEq)]
    pub struct $name {
      pub $first_key: $first_type,
      $(
        pub $key: $type,
      )*
    }

    impl Parse for $name {
      fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
        let mut $first_key = None;
        $(
          let mut $key = None;
        )*

        macro_rules! parse_one {
          ($k: ident, $t: ty) => {
            if $k.is_none() {
              if let Ok(val) = input.try_parse(<$t>::parse) {
                $k = Some(val);
                continue
              }
            }
          };
        }

        loop {
          parse_one!($first_key, $first_type);
          $(
            parse_one!($key, $type);
          )*
          break
        }

        Ok($name {
          $first_key: $first_key.unwrap_or_default(),
          $(
            $key: $key.unwrap_or_default(),
          )*
        })
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
        let mut needs_space = false;
        macro_rules! print_one {
          ($k: ident, $t: ty) => {
            if self.$k != <$t>::default() {
              if needs_space {
                dest.write_char(' ')?;
              }
              self.$k.to_css(dest)?;
              needs_space = true;
            }
          };
        }

        print_one!($first_key, $first_type);
        $(
          print_one!($key, $type);
        )*
        if !needs_space {
          self.$first_key.to_css(dest)?;
        }
        Ok(())
      }
    }
  };
}

pub (crate) use shorthand_property;

macro_rules! shorthand_handler {
  (
    $name: ident -> $shorthand: ident
    { $( $key: ident: $prop: ident($type: ty), )+ }
  ) => {
    #[derive(Default)]
    pub(crate) struct $name {
      $(
        pub $key: Option<$type>,
      )*
      has_any: bool
    }

    impl PropertyHandler for $name {
      fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
        match property {
          $(
            Property::$prop(val) => {
              self.$key = Some(val.clone());
              self.has_any = true;
            },
          )+
          Property::$shorthand(val) => {
            $(
              self.$key = Some(val.$key.clone());
            )+
            self.has_any = true;
          }
          Property::Unparsed(val) if matches!(val.property_id, $( PropertyId::$prop | )+ PropertyId::$shorthand) => {
            self.finalize(dest);
            dest.push(property.clone());
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, dest: &mut DeclarationList) {
        if !self.has_any {
          return
        }

        self.has_any = false;

        $(
          let $key = std::mem::take(&mut self.$key);
        )+

        if $( $key.is_some() && )* true {
          dest.push(Property::$shorthand($shorthand {
            $(
              $key: $key.unwrap(),
            )+
          }))
        } else {
          $(
            if let Some(val) = $key {
              dest.push(Property::$prop(val))
            }
          )+
        }
      }
    }
  };
}

pub (crate) use shorthand_handler;
