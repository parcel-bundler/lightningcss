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
    #[derive(Debug, Clone, Copy, PartialEq, Parse, ToCss)]
    #[cfg_attr(feature = "visitor", derive(Visit))]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "kebab-case"))]
    #[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
    #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
    $(#[$outer])*
    $vis enum $name {
      $(
        $(#[$meta])*
        $x,
      )+
    }

    impl $name {
      /// Returns a string representation of the value.
      pub fn as_str(&self) -> &str {
        use $name::*;
        match self {
          $(
            $x => const_str::convert_ascii_case!(kebab, stringify!($x)),
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
    #[derive(Debug, Clone, Copy, PartialEq)] #[cfg_attr(feature = "visitor", derive(Visit))]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    #[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
    #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
    $vis enum $name {
      $(
        $(#[$meta])*
        #[cfg_attr(feature = "serde", serde(rename = $str))]
        $id,
      )+
    }

    impl<'i> Parse<'i> for $name {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let location = input.current_source_location();
        let ident = input.expect_ident()?;
        cssparser::match_ignore_ascii_case! { &*ident,
          $(
            $str => Ok($name::$id),
          )+
          _ => Err(location.new_unexpected_token_error(
            cssparser::Token::Ident(ident.clone())
          )),
        }
      }
    }

    impl $name {
      /// Returns a string representation of the value.
      pub fn as_str(&self) -> &str {
        use $name::*;
        match self {
          $(
            $id => $str,
          )+
        }
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
        dest.write_str(self.as_str())
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
      $first_key: ident: $first_prop: ident($first_type: ty $(, $first_vp: ty)?),
      $(
        $(#[$meta: meta])*
        $key: ident: $prop: ident($type: ty $(, $vp: ty)?),
      )*
    }
  ) => {
    define_shorthand! {
      $(#[$outer])*
      pub struct $name$(<$l>)? {
        $(#[$first_meta])*
        $first_key: $first_prop($first_type $($first_vp)?),
        $(
          $(#[$meta])*
          $key: $prop($type $($vp)?),
        )*
      }
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

macro_rules! shorthand_property_bitflags {
  ($name:ident, $first:ident, $($rest:ident),*) => {
    crate::macros::shorthand_property_bitflags!($name, [$first,$($rest),+] $($rest),+ ; 0; $first = 0);
  };
  ($name:ident, [$($all:ident),*] $cur:ident, $($rest:ident),* ; $last_index: expr ; $($var:ident = $index:expr)+) => {
    crate::macros::shorthand_property_bitflags!($name, [$($all),*] $($rest),* ; $last_index + 1; $($var = $index)* $cur = $last_index + 1);
  };
  ($name:ident, [$($all:ident),*] $cur:ident; $last_index:expr ; $($var:ident = $index:expr)+) => {
    pastey::paste! {
      crate::macros::property_bitflags! {
        #[derive(Default, Debug)]
        struct [<$name Property>]: u8 {
          $(const $var = 1 << $index);*;
          const $cur = 1 << ($last_index + 1);
          const $name = $(Self::$all.bits())|*;
        }
      }
    }
  };
}

pub(crate) use shorthand_property_bitflags;

macro_rules! shorthand_handler {
  (
    $name: ident -> $shorthand: ident$(<$l: lifetime>)? $(fallbacks: $shorthand_fallback: literal)?
    { $( $key: ident: $prop: ident($type: ty $(, fallback: $fallback: literal)? $(, image: $image: literal)?), )+ }
  ) => {
    crate::macros::shorthand_property_bitflags!($shorthand, $($prop),*);

    #[derive(Default)]
    pub(crate) struct $name$(<$l>)? {
      $(
        pub $key: Option<$type>,
      )*
      flushed_properties: pastey::paste!([<$shorthand Property>]),
      has_any: bool
    }

    impl<'i> PropertyHandler<'i> for $name$(<$l>)? {
      fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) -> bool {
        use crate::traits::IsCompatible;

        match property {
          $(
            Property::$prop(val) => {
              if self.$key.is_some() && matches!(context.targets.browsers, Some(targets) if !val.is_compatible(targets)) {
                self.flush(dest, context);
              }
              self.$key = Some(val.clone());
              self.has_any = true;
            },
          )+
          Property::$shorthand(val) => {
            $(
              if self.$key.is_some() && matches!(context.targets.browsers, Some(targets) if !val.$key.is_compatible(targets)) {
                self.flush(dest, context);
              }
            )+
            $(
              self.$key = Some(val.$key.clone());
            )+
            self.has_any = true;
          }
          Property::Unparsed(val) if matches!(val.property_id, $( PropertyId::$prop | )+ PropertyId::$shorthand) => {
            self.flush(dest, context);

            let mut unparsed = val.clone();
            context.add_unparsed_fallbacks(&mut unparsed);
            pastey::paste! {
              self.flushed_properties.insert([<$shorthand Property>]::try_from(&unparsed.property_id).unwrap());
            };
            dest.push(Property::Unparsed(unparsed));
          }
          _ => return false
        }

        true
      }

      fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
        self.flush(dest, context);
        self.flushed_properties = pastey::paste!([<$shorthand Property>]::empty());
      }
    }

    impl<'i> $name$(<$l>)? {
      #[allow(unused_variables)]
      fn flush(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
        if !self.has_any {
          return
        }

        self.has_any = false;

        $(
          let $key = std::mem::take(&mut self.$key);
        )+

        if $( $key.is_some() && )* true {
          #[allow(unused_mut)]
          let mut shorthand = $shorthand {
            $(
              $key: $key.unwrap(),
            )+
          };

          $(
            if $shorthand_fallback && !self.flushed_properties.intersects(pastey::paste!([<$shorthand Property>]::$shorthand)) {
              let fallbacks = shorthand.get_fallbacks(context.targets);
              for fallback in fallbacks {
                dest.push(Property::$shorthand(fallback));
              }
            }
          )?

          dest.push(Property::$shorthand(shorthand));
          pastey::paste! {
            self.flushed_properties.insert([<$shorthand Property>]::$shorthand);
          };
        } else {
          $(
            #[allow(unused_mut)]
            if let Some(mut val) = $key {
              $(
                if $fallback && !self.flushed_properties.intersects(pastey::paste!([<$shorthand Property>]::$prop)) {
                  let fallbacks = val.get_fallbacks(context.targets);
                  for fallback in fallbacks {
                    dest.push(Property::$prop(fallback));
                  }
                }
              )?

              dest.push(Property::$prop(val));
              pastey::paste! {
                self.flushed_properties.insert([<$shorthand Property>]::$prop);
              };
            }
          )+
        }
      }
    }
  };
}

pub(crate) use shorthand_handler;

macro_rules! define_shorthand {
  (
    $(#[$outer:meta])*
    $vis:vis struct $name: ident$(<$l: lifetime>)?$(($prefix: ty))? {
      $(
        $(#[$meta: meta])*
        $key: ident: $prop: ident($type: ty $(, $vp: ty)?),
      )+
    }
  ) => {
    $(#[$outer])*
    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "visitor", derive(Visit))]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "camelCase"))]
    #[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
    #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
    pub struct $name$(<$l>)? {
      $(
        $(#[$meta])*
        pub $key: $type,
      )+
    }

    crate::macros::impl_shorthand! {
      $name($name$(<$l>)? $(, $prefix)?) {
        $(
          $key: [ $prop$(($vp))?, ],
        )+
      }
    }
  };
}

pub(crate) use define_shorthand;

macro_rules! impl_shorthand {
  (
    $name: ident($t: ty $(, $prefix: ty)?) {
      $(
        $key: ident: [ $( $prop: ident$(($vp: ty))? $(,)?)+ ],
      )+
    }

    $(
      fn is_valid($v: ident) {
        $($body: tt)+
      }
    )?
  ) => {
    #[allow(unused_macros)]
    macro_rules! vp_name {
      ($x: ty, $n: ident) => {
        $n
      };
      ($x: ty, $n: expr) => {
        $n
      };
    }

    impl<'i> Shorthand<'i> for $t {
      #[allow(unused_variables)]
      fn from_longhands(decls: &DeclarationBlock<'i>, vendor_prefix: crate::vendor_prefix::VendorPrefix) -> Option<(Self, bool)> {
        use pastey::paste;

        $(
          $(
            paste! {
              let mut [<$prop:snake _value>] = None;
            }
          )+
        )+

        let mut count = 0;
        let mut important_count = 0;
        for (property, important) in decls.iter() {
          match property {
            $(
              $(
                Property::$prop(val $(, vp_name!($vp, p))?) => {
                  $(
                    if *vp_name!($vp, p) != vendor_prefix {
                      return None
                    }
                  )?

                  paste! {
                    [<$prop:snake _value>] = Some(val.clone());
                  }
                  count += 1;
                  if important {
                    important_count += 1;
                  }
                }
              )+
            )+
            Property::$name(val $(, vp_name!($prefix, p))?) => {
              $(
                if *vp_name!($prefix, p) != vendor_prefix {
                  return None
                }
              )?

              $(
                $(
                  paste! {
                    [<$prop:snake _value>] = Some(val.$key.clone());
                  }
                  count += 1;
                  if important {
                    important_count += 1;
                  }
                )+
              )+
            }
            _ => {
              $(
                $(
                  if let Some(Property::$prop(longhand $(, vp_name!($vp, _p))?)) = property.longhand(&PropertyId::$prop$((vp_name!($vp, vendor_prefix)))?) {
                    paste! {
                      [<$prop:snake _value>] = Some(longhand);
                    }
                    count += 1;
                    if important {
                      important_count += 1;
                    }
                  }
                )+
              )+
            }
          }
        }

        // !important flags must match to produce a shorthand.
        if important_count > 0 && important_count != count {
          return None
        }

        if $($(paste! { [<$prop:snake _value>].is_some() } &&)+)+ true {
          // All properties in the group must have a matching value to produce a shorthand.
          $(
            let mut $key = None;
            $(
              if $key == None {
                paste! {
                  $key = [<$prop:snake _value>];
                }
              } else if paste! { $key != [<$prop:snake _value>] } {
                return None
              }
            )+
          )+

          let value = $name {
            $(
              $key: $key.unwrap(),
            )+
          };

          $(
            #[inline]
            fn is_valid($v: &$name) -> bool {
              $($body)+
            }

            if !is_valid(&value) {
              return None
            }
          )?

          return Some((value, important_count > 0));
        }

        None
      }

      #[allow(unused_variables)]
      fn longhands(vendor_prefix: crate::vendor_prefix::VendorPrefix) -> Vec<PropertyId<'static>> {
        vec![$($(PropertyId::$prop$((vp_name!($vp, vendor_prefix)))?, )+)+]
      }

      fn longhand(&self, property_id: &PropertyId) -> Option<Property<'i>> {
        match property_id {
          $(
            $(
              PropertyId::$prop$((vp_name!($vp, p)))? => {
                Some(Property::$prop(self.$key.clone() $(, *vp_name!($vp, p))?))
              }
            )+
          )+
          _ => None
        }
      }

      fn set_longhand(&mut self, property: &Property<'i>) -> Result<(), ()> {
        macro_rules! count {
          ($p: ident) => {
            1
          }
        }

        $(
          #[allow(non_upper_case_globals)]
          const $key: u8 = 0 $( + count!($prop))+;
        )+

        match property {
          $(
            $(
              Property::$prop(val $(, vp_name!($vp, _p))?) => {
                // If more than one longhand maps to this key, bail.
                if $key > 1 {
                  return Err(())
                }
                self.$key = val.clone();
                return Ok(())
              }
            )+
          )+
          _ => {}
        }
        Err(())
      }
    }
  }
}

pub(crate) use impl_shorthand;

macro_rules! define_list_shorthand {
  (
    $(#[$outer:meta])*
    $vis:vis struct $name: ident$(<$l: lifetime>)?$(($prefix: ty))? {
      $(
        $(#[$meta: meta])*
        $key: ident: $prop: ident($type: ty $(, $vp: ty)?),
      )+
    }
  ) => {
    $(#[$outer])*
    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "visitor", derive(Visit))]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "camelCase"))]
    #[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
    #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
    pub struct $name$(<$l>)? {
      $(
        $(#[$meta])*
        pub $key: $type,
      )+
    }

    #[allow(unused_macros)]
    macro_rules! vp_name {
      ($x: ty, $n: ident) => {
        $n
      };
      ($x: ty, $n: expr) => {
        $n
      };
    }

    impl<'i> Shorthand<'i> for SmallVec<[$name$(<$l>)?; 1]> {
      #[allow(unused_variables)]
      fn from_longhands(decls: &DeclarationBlock<'i>, vendor_prefix: crate::vendor_prefix::VendorPrefix) -> Option<(Self, bool)> {
        $(
          let mut $key = None;
        )+

        let mut count = 0;
        let mut important_count = 0;
        let mut length = None;
        for (property, important) in decls.iter() {
          let mut len = 0;
          match property {
            $(
              Property::$prop(val $(, vp_name!($vp, p))?) => {
                $(
                  if *vp_name!($vp, p) != vendor_prefix {
                    return None
                  }
                )?

                $key = Some(val.clone());
                len = val.len();
                count += 1;
                if important {
                  important_count += 1;
                }
              }
            )+
            Property::$name(val $(, vp_name!($prefix, p))?) => {
              $(
                if *vp_name!($prefix, p) != vendor_prefix {
                  return None
                }
              )?
              $(
                $key = Some(val.iter().map(|b| b.$key.clone()).collect());
              )+
              len = val.len();
              count += 1;
              if important {
                important_count += 1;
              }
            }
            _ => {
              $(
                if let Some(Property::$prop(longhand $(, vp_name!($vp, _p))?)) = property.longhand(&PropertyId::$prop$((vp_name!($vp, vendor_prefix)))?) {
                  len = longhand.len();
                  $key = Some(longhand);
                  count += 1;
                  if important {
                    important_count += 1;
                  }
                }
              )+
            }
          }

          // Lengths must be equal.
          if length.is_none() {
            length = Some(len);
          } else if length.unwrap() != len {
            return None
          }
        }

        // !important flags must match to produce a shorthand.
        if important_count > 0 && important_count != count {
          return None
        }

        if $($key.is_some() &&)+ true {
          let values = izip!(
            $(
              $key.unwrap().drain(..),
            )+
          ).map(|($($key,)+)| {
            $name {
              $(
                $key,
              )+
            }
          }).collect();
          return Some((values, important_count > 0))
        }

        None
      }

      #[allow(unused_variables)]
      fn longhands(vendor_prefix: crate::vendor_prefix::VendorPrefix) -> Vec<PropertyId<'static>> {
        vec![$(PropertyId::$prop$((vp_name!($vp, vendor_prefix)))?, )+]
      }

      fn longhand(&self, property_id: &PropertyId) -> Option<Property<'i>> {
        match property_id {
          $(
            PropertyId::$prop$((vp_name!($vp, p)))? => {
              Some(Property::$prop(self.iter().map(|v| v.$key.clone()).collect() $(, *vp_name!($vp, p))?))
            }
          )+
          _ => None
        }
      }

      fn set_longhand(&mut self, property: &Property<'i>) -> Result<(), ()> {
        match property {
          $(
            Property::$prop(val $(, vp_name!($vp, _p))?) => {
              if val.len() != self.len() {
                return Err(())
              }

              for (i, item) in self.iter_mut().enumerate() {
                item.$key = val[i].clone();
              }
              return Ok(())
            }
          )+
          _ => {}
        }
        Err(())
      }
    }
  };
}

pub(crate) use define_list_shorthand;

macro_rules! rect_shorthand {
  (
    $(#[$meta: meta])*
    $vis:vis struct $name: ident<$t: ty> {
      $top: ident,
      $right: ident,
      $bottom: ident,
      $left: ident
    }
  ) => {
    define_shorthand! {
      $(#[$meta])*
      pub struct $name {
        /// The top value.
        top: $top($t),
        /// The right value.
        right: $right($t),
        /// The bottom value.
        bottom: $bottom($t),
        /// The left value.
        left: $left($t),
      }
    }

    impl<'i> Parse<'i> for $name {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let rect = Rect::parse(input)?;
        Ok(Self {
          top: rect.0,
          right: rect.1,
          bottom: rect.2,
          left: rect.3,
        })
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
      where
        W: std::fmt::Write,
      {
        Rect::new(&self.top, &self.right, &self.bottom, &self.left).to_css(dest)
      }
    }
  };
}

pub(crate) use rect_shorthand;

macro_rules! size_shorthand {
  (
    $(#[$outer:meta])*
    $vis:vis struct $name: ident<$t: ty> {
      $(#[$a_meta: meta])*
      $a_key: ident: $a_prop: ident,
      $(#[$b_meta: meta])*
      $b_key: ident: $b_prop: ident,
    }
  ) => {
    define_shorthand! {
      $(#[$outer])*
      $vis struct $name {
        $(#[$a_meta])*
        $a_key: $a_prop($t),
        $(#[$b_meta])*
        $b_key: $b_prop($t),
      }
    }

    impl<'i> Parse<'i> for $name {
      fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
        let size = Size2D::parse(input)?;
        Ok(Self {
          $a_key: size.0,
          $b_key: size.1,
        })
      }
    }

    impl ToCss for $name {
      fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
      where
        W: std::fmt::Write,
      {
        Size2D(&self.$a_key, &self.$b_key).to_css(dest)
      }
    }
  };
}

pub(crate) use size_shorthand;

macro_rules! property_bitflags {
  (
    $(#[$outer:meta])*
    $vis:vis struct $BitFlags:ident: $T:ty {
      $(
        $(#[$inner:ident $($args:tt)*])*
        const $Flag:ident $(($vp:ident))? = $value:expr;
      )*
    }
  ) => {
    bitflags::bitflags! {
      $(#[$outer])*
      $vis struct $BitFlags: $T {
        $(
          $(#[$inner $($args)*])*
            const $Flag = $value;
        )*
      }
    }

    impl<'i> TryFrom<&PropertyId<'i>> for $BitFlags {
      type Error = ();

      fn try_from(value: &PropertyId<'i>) -> Result<$BitFlags, Self::Error> {
        match value {
          $(
            PropertyId::$Flag $(($vp))? => Ok($BitFlags::$Flag),
          )*
          _ => Err(())
        }
      }
    }
  };
}

pub(crate) use property_bitflags;
