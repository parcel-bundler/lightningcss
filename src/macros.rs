macro_rules! enum_property {
  (
    $(#[$outer:meta])*
    $vis:vis enum $name:ident {
      $(
        $(#[$meta: meta])*
        $x: ident,
      )+
    }
  ) => {
    $(#[$outer])*
    #[derive(Debug, Clone, Copy, PartialEq)]
    $vis enum $name {
      $(
        $(#[$meta])*
        $x,
      )+
    }

    impl<'i> Parse<'i> for $name {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        match &ident[..] {
          $(
            s if s.eq_ignore_ascii_case(stringify!($x)) => Ok($name::$x),
          )+
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        }
      }

      fn parse_string(input: &'i str) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        match input {
          $(
            s if s.eq_ignore_ascii_case(stringify!($x)) => Ok($name::$x),
          )+
          _ => return Err(ParseError {
            kind: ParseErrorKind::Basic(BasicParseErrorKind::UnexpectedToken(cssparser::Token::Ident(input.into()))),
            location: cssparser::SourceLocation { line: 0, column: 1 }
          })
        }
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
        use $name::*;
        match self {
          $(
            $x => dest.write_str(&stringify!($x).to_lowercase()),
          )+
        }
      }
    }
  };
  (
    $(#[$outer:meta])*
    $vis:vis enum $name:ident {
      $(
        $(#[$meta: meta])*
        $str: literal: $id: ident,
      )+
    }
  ) => {
    $(#[$outer])*
    #[derive(Debug, Clone, Copy, PartialEq)]
    $vis enum $name {
      $(
        $(#[$meta])*
        $id,
      )+
    }

    impl<'i> Parse<'i> for $name {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        match &ident[..] {
          $(
            s if s.eq_ignore_ascii_case($str) => Ok($name::$id),
          )+
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          ))
        }
      }

      fn parse_string(input: &'i str) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        match input {
          $(
            s if s.eq_ignore_ascii_case($str) => Ok($name::$id),
          )+
          _ => return Err(ParseError {
            kind: ParseErrorKind::Basic(BasicParseErrorKind::UnexpectedToken(cssparser::Token::Ident(input.into()))),
            location: cssparser::SourceLocation { line: 0, column: 1 }
          })
        }
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

macro_rules! shorthand_property {
  (
    $(#[$outer:meta])*
    $vis:vis struct $name: ident$(<$l: lifetime>)? {
      $(#[$first_meta: meta])*
      $first_key: ident: $first_type: ty,
      $(
        $(#[$meta: meta])*
        $key: ident: $type: ty,
      )*
    }
  ) => {
    $(#[$outer])*
    #[derive(Debug, Clone, PartialEq)]
    pub struct $name$(<$l>)? {
      $(#[$first_meta])*
      pub $first_key: $first_type,
      $(
        $(#[$meta])*
        pub $key: $type,
      )*
    }

    impl<'i> Parse<'i> for $name$(<$l>)? {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
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

    impl$(<$l>)? ToCss for $name$(<$l>)? {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

pub(crate) use shorthand_property;

macro_rules! shorthand_handler {
  (
    $name: ident -> $shorthand: ident$(<$l: lifetime>)?
    { $( $key: ident: $prop: ident($type: ty $(, fallback: $fallback: literal)?), )+ }
  ) => {
    #[derive(Default)]
    pub(crate) struct $name$(<$l>)? {
      targets: Option<Browsers>,
      $(
        pub $key: Option<$type>,
      )*
      has_any: bool
    }

    impl$(<$l>)? $name$(<$l>)? {
      pub fn new(targets: Option<Browsers>) -> Self {
        Self {
          targets,
          ..Self::default()
        }
      }
    }

    impl<'i> PropertyHandler<'i> for $name$(<$l>)? {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i>) -> bool {
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
            self.finalize(dest, context);

            let mut unparsed = val.clone();
            context.add_unparsed_fallbacks(&mut unparsed);
            dest.push(Property::Unparsed(unparsed));
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, dest: &mut DeclarationList<'i>, _: &mut PropertyHandlerContext<'i>) {
        if !self.has_any {
          return
        }

        self.has_any = false;

        $(
          let $key = std::mem::take(&mut self.$key);
        )+

        if $( $key.is_some() && )* true {
          let mut shorthand = $shorthand {
            $(
              $key: $key.unwrap(),
            )+
          };

          if let Some(targets) = self.targets {
            let fallbacks = shorthand.get_fallbacks(targets);
            for fallback in fallbacks {
              dest.push(Property::$shorthand(fallback));
            }
          }

          dest.push(Property::$shorthand(shorthand))
        } else {
          $(
            #[allow(unused_mut)]
            if let Some(mut val) = $key {
              $(
                if $fallback {
                  if let Some(targets) = self.targets {
                    let fallbacks = val.get_fallbacks(targets);
                    for fallback in fallbacks {
                      dest.push(Property::$prop(fallback));
                    }
                  }
                }
              )?

              dest.push(Property::$prop(val))
            }
          )+
        }
      }
    }
  };
}

pub(crate) use shorthand_handler;
