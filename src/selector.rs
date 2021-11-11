use cssparser::*;
use selectors::{SelectorList, parser::{SelectorImpl, Selector, Combinator, Component}};
use std::fmt;
use crate::printer::Printer;
use crate::traits::ToCss;
use std::fmt::Write;
use super::parser::CssString;

#[derive(Debug, Clone, PartialEq)]
pub struct Selectors;

impl SelectorImpl for Selectors {
  type AttrValue = CssString;
  type Identifier = CssString;
  type LocalName = CssString;
  type NamespacePrefix = CssString;
  type NamespaceUrl = String;
  type BorrowedNamespaceUrl = String;
  type BorrowedLocalName = String;

  type NonTSPseudoClass = PseudoClass;
  type PseudoElement = PseudoElement;

  type ExtraMatchingData = ();
}

pub struct SelectorParser;
impl<'i> selectors::parser::Parser<'i> for SelectorParser {
  type Impl = Selectors;
  type Error = selectors::parser::SelectorParseErrorKind<'i>;

  fn parse_non_ts_pseudo_class(
    &self,
    location: SourceLocation,
    name: CowRcStr<'i>,
  ) -> Result<PseudoClass, ParseError<'i, Self::Error>> {
      use PseudoClass::*;
      let pseudo_class = match_ignore_ascii_case! { &name,
        "active" => Active,
        "any-link" => AnyLink,
        "checked" => Checked,
        "defined" => Defined,
        "disabled" => Disabled,
        "enabled" => Enabled,
        "focus" => Focus,
        "fullscreen" => Fullscreen,
        "hover" => Hover,
        "indeterminate" => Indeterminate,
        "link" => Link,
        "placeholder-shown" => PlaceholderShown,
        "read-write" => ReadWrite,
        "read-only" => ReadOnly,
        "target" => Target,
        "visited" => Visited,
        _ => Custom(name.as_ref().into())
      };

      Ok(pseudo_class)
  }

  fn parse_non_ts_functional_pseudo_class<'t>(
      &self,
      name: CowRcStr<'i>,
      parser: &mut cssparser::Parser<'i, 't>,
  ) -> Result<PseudoClass, ParseError<'i, Self::Error>> {
      use PseudoClass::*;
      let pseudo_class = match_ignore_ascii_case! { &name,
        "lang" => {
          Lang(parser.expect_ident_or_string()?.as_ref().into())
        },
        _ => return Err(parser.new_custom_error(selectors::parser::SelectorParseErrorKind::UnexpectedIdent(name.clone()))),
      };

      Ok(pseudo_class)
  }

  fn parse_pseudo_element(
    &self,
    location: SourceLocation,
    name: CowRcStr<'i>,
  ) -> Result<PseudoElement, ParseError<'i, Self::Error>> {
    use PseudoElement::*;
    let pseudo_element = match_ignore_ascii_case! { &name,
      "before" => Before,
      "after" => After,
      "first-line" => FirstLine,
      "first-letter" => FirstLetter,
      "selection" => Selection,
      _ => Custom(name.as_ref().into())
    };

    Ok(pseudo_element)
  }
}

/// https://drafts.csswg.org/selectors-4/#structural-pseudos
#[derive(Clone, Eq, PartialEq)]
pub enum PseudoClass {
  Active,
  AnyLink,
  Checked,
  Defined,
  Disabled,
  Enabled,
  Focus,
  Fullscreen,
  Hover,
  Indeterminate,
  Lang(Box<str>),
  Link,
  PlaceholderShown,
  ReadWrite,
  ReadOnly,
  Target,
  Visited,
  Custom(String)
}

impl selectors::parser::NonTSPseudoClass for PseudoClass {
  type Impl = Selectors;

  fn is_active_or_hover(&self) -> bool {
      matches!(*self, PseudoClass::Active | PseudoClass::Hover)
  }

  fn is_user_action_state(&self) -> bool {
      matches!(*self, PseudoClass::Active | PseudoClass::Hover | PseudoClass::Focus)
  }
}

impl cssparser::ToCss for PseudoClass {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result
  where
      W: fmt::Write,
  {
      use PseudoClass::*;
      if let Lang(ref lang) = *self {
        dest.write_str(":lang(")?;
        serialize_identifier(lang, dest)?;
        return dest.write_str(")");
      }

      dest.write_str(match &self {
        Active => ":active",
        AnyLink => ":any-link",
        Checked => ":checked",
        Defined => ":defined",
        Disabled => ":disabled",
        Enabled => ":enabled",
        Focus => ":focus",
        Fullscreen => ":fullscreen",
        Hover => ":hover",
        Indeterminate => ":indeterminate",
        Link => ":link",
        PlaceholderShown => ":placeholder-shown",
        ReadWrite => ":read-write",
        ReadOnly => ":read-only",
        Target => ":target",
        Visited => ":visited",
        Lang(_) => unreachable!(),
        Custom(val) => {
          dest.write_char(':')?;
          return dest.write_str(&val)
        }
      })
  }
}


#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum PseudoElement {
  After,
  Before,
  FirstLine,
  FirstLetter,
  Selection,
  Custom(String)
}

impl cssparser::ToCss for PseudoElement {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result
  where
      W: fmt::Write,
  {
    use PseudoElement::*;
    dest.write_str(match &self {
      // CSS2 pseudo elements support a single colon syntax in addition
      // to the more correct double colon for other pseudo elements.
      // We use that here because it's supported everywhere and is shorter.
      After => ":after",
      Before => ":before",
      FirstLine => ":first-line",
      FirstLetter => ":first-letter",
      Selection => "::selection",
      Custom(val) => {
        dest.write_str("::")?;
        return dest.write_str(val)
      }
    })
  }
}

impl selectors::parser::PseudoElement for PseudoElement {
  type Impl = Selectors;
}

impl ToCss for SelectorList<Selectors> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    serialize_selector_list(self.0.iter(), dest)
  }
}

impl ToCss for Combinator {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result
  where
      W: fmt::Write,
  {
      match *self {
          Combinator::Child => dest.delim('>', true),
          Combinator::Descendant => dest.write_str(" "),
          Combinator::NextSibling => dest.delim('+', true),
          Combinator::LaterSibling => dest.delim('~', true),
          Combinator::PseudoElement | Combinator::Part | Combinator::SlotAssignment => Ok(()),
      }
  }
}

// Copied from the selectors crate and modified to override to_css implementation.
impl ToCss for selectors::parser::Selector<Selectors> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result
  where
      W: fmt::Write,
  {
    use selectors::parser::*;
      // Compound selectors invert the order of their contents, so we need to
      // undo that during serialization.
      //
      // This two-iterator strategy involves walking over the selector twice.
      // We could do something more clever, but selector serialization probably
      // isn't hot enough to justify it, and the stringification likely
      // dominates anyway.
      //
      // NB: A parse-order iterator is a Rev<>, which doesn't expose as_slice(),
      // which we need for |split|. So we split by combinators on a match-order
      // sequence and then reverse.

      let mut combinators = self
          .iter_raw_match_order()
          .rev()
          .filter_map(|x| x.as_combinator());
      let compound_selectors = self
          .iter_raw_match_order()
          .as_slice()
          .split(|x| x.is_combinator())
          .rev();

      let mut combinators_exhausted = false;
      for compound in compound_selectors {
          debug_assert!(!combinators_exhausted);

          // https://drafts.csswg.org/cssom/#serializing-selectors
          if compound.is_empty() {
              continue;
          }

          // 1. If there is only one simple selector in the compound selectors
          //    which is a universal selector, append the result of
          //    serializing the universal selector to s.
          //
          // Check if `!compound.empty()` first--this can happen if we have
          // something like `... > ::before`, because we store `>` and `::`
          // both as combinators internally.
          //
          // If we are in this case, after we have serialized the universal
          // selector, we skip Step 2 and continue with the algorithm.
          let (can_elide_namespace, first_non_namespace) = match compound[0] {
              Component::ExplicitAnyNamespace |
              Component::ExplicitNoNamespace |
              Component::Namespace(..) => (false, 1),
              Component::DefaultNamespace(..) => (true, 1),
              _ => (true, 0),
          };
          let mut perform_step_2 = true;
          let next_combinator = combinators.next();
          if first_non_namespace == compound.len() - 1 {
              match (next_combinator, &compound[first_non_namespace]) {
                  // We have to be careful here, because if there is a
                  // pseudo element "combinator" there isn't really just
                  // the one simple selector. Technically this compound
                  // selector contains the pseudo element selector as well
                  // -- Combinator::PseudoElement, just like
                  // Combinator::SlotAssignment, don't exist in the
                  // spec.
                  (Some(Combinator::PseudoElement), _) |
                  (Some(Combinator::SlotAssignment), _) => (),
                  (_, &Component::ExplicitUniversalType) => {
                      // Iterate over everything so we serialize the namespace
                      // too.
                      for simple in compound.iter() {
                          simple.to_css(dest)?;
                      }
                      // Skip step 2, which is an "otherwise".
                      perform_step_2 = false;
                  },
                  _ => (),
              }
          }

          // 2. Otherwise, for each simple selector in the compound selectors
          //    that is not a universal selector of which the namespace prefix
          //    maps to a namespace that is not the default namespace
          //    serialize the simple selector and append the result to s.
          //
          // See https://github.com/w3c/csswg-drafts/issues/1606, which is
          // proposing to change this to match up with the behavior asserted
          // in cssom/serialize-namespaced-type-selectors.html, which the
          // following code tries to match.
          if perform_step_2 {
              for simple in compound.iter() {
                  if let Component::ExplicitUniversalType = *simple {
                      // Can't have a namespace followed by a pseudo-element
                      // selector followed by a universal selector in the same
                      // compound selector, so we don't have to worry about the
                      // real namespace being in a different `compound`.
                      if can_elide_namespace {
                          continue;
                      }
                  }
                  simple.to_css(dest)?;
              }
          }

          // 3. If this is not the last part of the chain of the selector
          //    append a single SPACE (U+0020), followed by the combinator
          //    ">", "+", "~", ">>", "||", as appropriate, followed by another
          //    single SPACE (U+0020) if the combinator was not whitespace, to
          //    s.
          match next_combinator {
              Some(c) => c.to_css(dest)?,
              None => combinators_exhausted = true,
          };

          // 4. If this is the last part of the chain of the selector and
          //    there is a pseudo-element, append "::" followed by the name of
          //    the pseudo-element, to s.
          //
          // (we handle this above)
      }

      Ok(())
  }
}

impl ToCss for Component<Selectors> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> fmt::Result where W: fmt::Write {
    use Component::*;
    match &self {
      Combinator(ref c) => c.to_css(dest),
      AttributeInNoNamespace {
        ref local_name,
        operator,
        ref value,
        case_sensitivity,
        ..
    } => {
        dest.write_char('[')?;
        local_name.write_identifier(dest)?;
        cssparser::ToCss::to_css(operator, dest)?;

        if dest.minify {
          // Serialize as both an identifier and a string and choose the shorter one.
          let mut id = String::new();
          value.write_identifier(&mut id)?;

          let mut s = String::new();
          value.write_string(&mut s)?;

          if id.len() > 0 && id.len() < s.len() + 2 {
            dest.write_str(&id)?;
          } else {
            dest.write_char('"')?;
            dest.write_str(&s)?;
            dest.write_char('"')?;
          }
        } else {
          dest.write_char('"')?;
          value.write_string(dest)?;
          dest.write_char('"')?;
        }

        match case_sensitivity {
          selectors::attr::ParsedCaseSensitivity::CaseSensitive |
          selectors::attr::ParsedCaseSensitivity::AsciiCaseInsensitiveIfInHtmlElementInHtmlDocument => {},
          selectors::attr::ParsedCaseSensitivity::AsciiCaseInsensitive => dest.write_str(" i")?,
          selectors::attr::ParsedCaseSensitivity::ExplicitCaseSensitive => dest.write_str(" s")?,
        }
        dest.write_char(']')
      },
      Is(ref list) | Where(ref list) | Negation(ref list) => {
        match *self {
          Where(..) => dest.write_str(":where(")?,
          Is(..) => dest.write_str(":is(")?,
          Negation(..) => dest.write_str(":not(")?,
          _ => unreachable!(),
        }
        serialize_selector_list(list.iter(), dest)?;
        dest.write_str(")")
      },
      _ => {
        cssparser::ToCss::to_css(self, dest)
      }
    }
  }
}

fn serialize_selector_list<'a, I, W>(iter: I, dest: &mut Printer<W>) -> fmt::Result
where
    I: Iterator<Item = &'a Selector<Selectors>>,
    W: fmt::Write,
{  let mut first = true;
  for selector in iter {
    if !first {
      dest.delim(',', false)?;
    }
    first = false;
    selector.to_css(dest)?;
  }
  Ok(())
}
