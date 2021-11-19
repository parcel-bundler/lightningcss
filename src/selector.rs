use cssparser::*;
use selectors::{SelectorList, parser::{SelectorImpl, Selector, Combinator, Component}, attr::{AttrSelectorOperator, ParsedAttrSelectorOperation, ParsedCaseSensitivity}};
use std::fmt;
use crate::printer::Printer;
use crate::traits::ToCss;
use super::parser::CssString;
use crate::compat::Feature;
use crate::properties::prefixes::Browsers;

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
    _: SourceLocation,
    name: CowRcStr<'i>,
  ) -> Result<PseudoClass, ParseError<'i, Self::Error>> {
      use PseudoClass::*;
      let pseudo_class = match_ignore_ascii_case! { &name,
        // https://drafts.csswg.org/selectors-4/#useraction-pseudos
        "hover" => Hover,
        "active" => Active,
        "focus" => Focus,
        "focus-visible" => FocusVisible,
        "focus-within" => FocusWithin,

        // https://drafts.csswg.org/selectors-4/#time-pseudos
        "current" => Current,
        "past" => Past,
        "future" => Future,

        // https://drafts.csswg.org/selectors-4/#resource-pseudos
        "playing" => Playing,
        "paused" => Paused,
        "seeking" => Seeking,
        "buffering" => Buffering,
        "stalled" => Stalled,
        "muted" => Muted,
        "volume-locked" => VolumeLocked,

        // https://fullscreen.spec.whatwg.org/#:fullscreen-pseudo-class
        "fullscreen" => Fullscreen,

        // https://drafts.csswg.org/selectors-4/#the-defined-pseudo
        "defined" => Defined,

        // https://drafts.csswg.org/selectors-4/#location
        "any-link" => AnyLink,
        "link" => Link,
        "local-link" => LocalLink,
        "target" => Target,
        "target-within" => TargetWithin,
        "visited" => Visited,

        // https://drafts.csswg.org/selectors-4/#input-pseudos
        "enabled" => Enabled,
        "disabled" => Disabled,
        "read-only" => ReadOnly,
        "read-write" => ReadWrite,
        "placeholder-shown" => PlaceholderShown,
        "default" => Default,
        "checked" => Checked,
        "indeterminate" => Indeterminate,
        "blank" => Blank,
        "valid" => Valid,
        "invalid" => Invalid,
        "in-range" => InRange,
        "out-of-range" => OutOfRange,
        "required" => Required,
        "optional" => Optional,
        "user-valid" => UserValid,
        "user-invalid" => UserInvalid,

        // https://html.spec.whatwg.org/multipage/semantics-other.html#selector-autofill
        "autofill" => Autofill,

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
        "lang" => Lang(parser.expect_ident_or_string()?.as_ref().into()),
        "dir" => Dir(parser.expect_ident_or_string()?.as_ref().into()),
        _ => return Err(parser.new_custom_error(selectors::parser::SelectorParseErrorKind::UnexpectedIdent(name.clone()))),
      };

      Ok(pseudo_class)
  }

  fn parse_pseudo_element(
    &self,
    _: SourceLocation,
    name: CowRcStr<'i>,
  ) -> Result<PseudoElement, ParseError<'i, Self::Error>> {
    use PseudoElement::*;
    let pseudo_element = match_ignore_ascii_case! { &name,
      "before" => Before,
      "after" => After,
      "first-line" => FirstLine,
      "first-letter" => FirstLetter,
      "selection" => Selection,
      "placeholder" => Placeholder,
      "marker" => Marker,
      "backdrop" => Backdrop,
      _ => Custom(name.as_ref().into())
    };

    Ok(pseudo_element)
  }
}

/// https://drafts.csswg.org/selectors-4/#structural-pseudos
#[derive(Clone, Eq, PartialEq)]
pub enum PseudoClass {
  // https://drafts.csswg.org/selectors-4/#linguistic-pseudos
  Lang(Box<str>),
  Dir(Box<str>),

  // https://drafts.csswg.org/selectors-4/#useraction-pseudos
  Hover,
  Active,
  Focus,
  FocusVisible,
  FocusWithin,

  // https://drafts.csswg.org/selectors-4/#time-pseudos
  Current,
  Past,
  Future,

  // https://drafts.csswg.org/selectors-4/#resource-pseudos
  Playing,
  Paused,
  Seeking,
  Buffering,
  Stalled,
  Muted,
  VolumeLocked,

  // https://fullscreen.spec.whatwg.org/#:fullscreen-pseudo-class
  Fullscreen,

  // https://drafts.csswg.org/selectors-4/#the-defined-pseudo
  Defined,

  // https://drafts.csswg.org/selectors-4/#location
  AnyLink,
  Link,
  LocalLink,
  Target,
  TargetWithin,
  Visited,

  // https://drafts.csswg.org/selectors-4/#input-pseudos
  Enabled,
  Disabled,
  ReadOnly,
  ReadWrite,
  PlaceholderShown,
  Default,
  Checked,
  Indeterminate,
  Blank,
  Valid,
  Invalid,
  InRange,
  OutOfRange,
  Required,
  Optional,
  UserValid,
  UserInvalid,

  // https://html.spec.whatwg.org/multipage/semantics-other.html#selector-autofill
  Autofill,

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
      match &self {
        Lang(lang) => {
          dest.write_str(":lang(")?;
          serialize_identifier(lang, dest)?;
          return dest.write_str(")");
        }
        Dir(dir) => {
          dest.write_str(":dir(")?;
          serialize_identifier(dir, dest)?;
          return dest.write_str(")");
        }
        _ => {}
      }

      dest.write_str(match &self {
        // https://drafts.csswg.org/selectors-4/#useraction-pseudos
        Hover => ":hover",
        Active => ":active",
        Focus => ":focus",
        FocusVisible => ":focus-visible",
        FocusWithin => ":focus-within",

        // https://drafts.csswg.org/selectors-4/#time-pseudos
        Current => ":current",
        Past => ":past",
        Future => ":future",

        // https://drafts.csswg.org/selectors-4/#resource-pseudos
        Playing => ":playing",
        Paused => ":paused",
        Seeking => ":seeking",
        Buffering => ":buffering",
        Stalled => ":stalled",
        Muted => ":muted",
        VolumeLocked => ":volume-locked",

        // https://fullscreen.spec.whatwg.org/#:fullscreen-pseudo-class
        Fullscreen => ":fullscreen",

        // https://drafts.csswg.org/selectors-4/#the-defined-pseudo
        Defined => ":defined",

        // https://drafts.csswg.org/selectors-4/#location
        AnyLink => ":any-link",
        Link => ":link",
        LocalLink => ":local-link",
        Target => ":target",
        TargetWithin => ":target-within",
        Visited => ":visited",

        // https://drafts.csswg.org/selectors-4/#input-pseudos
        Enabled => ":enabled",
        Disabled => ":disabled",
        ReadOnly => ":read-only",
        ReadWrite => ":read-write",
        PlaceholderShown => ":placeholder-shown",
        Default => ":default",
        Checked => ":checked",
        Indeterminate => ":indeterminate",
        Blank => ":blank",
        Valid => ":valid",
        Invalid => ":invalid",
        InRange => ":in-range",
        OutOfRange => ":out-of-range",
        Required => ":required",
        Optional => ":optional",
        UserValid => ":user-valid",
        UserInvalid => ":user-invalid",

        // https://html.spec.whatwg.org/multipage/semantics-other.html#selector-autofill
        Autofill => ":autofill",

        Lang(_) | Dir(_) => unreachable!(),
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
  Placeholder,
  Marker,
  Backdrop,
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
      Placeholder => "::placeholder",
      Marker => "::marker",
      Backdrop => "::backdrop",
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

pub fn is_compatible(selectors: &SelectorList<Selectors>, targets: Option<Browsers>) -> bool {
  for selector in &selectors.0 {
    let iter = selector.iter();
    for component in iter {
      let feature = match component {
        Component::ID(_) |
        Component::Class(_) |
        Component::LocalName(_) => continue,

        Component::ExplicitAnyNamespace |
        Component::ExplicitNoNamespace |
        Component::DefaultNamespace(_) |
        Component::Namespace(_, _) => Feature::CssNamespaces,

        Component::ExplicitUniversalType => Feature::CssSel2,

        Component::AttributeInNoNamespaceExists { .. } => Feature::CssSel2,
        Component::AttributeInNoNamespace { operator, case_sensitivity, .. } => {
          if *case_sensitivity != ParsedCaseSensitivity::CaseSensitive {
            Feature::CssCaseInsensitive
          } else {
            match operator {
              AttrSelectorOperator::Equal |
              AttrSelectorOperator::Includes |
              AttrSelectorOperator::DashMatch => Feature::CssSel2,
              AttrSelectorOperator::Prefix |
              AttrSelectorOperator::Substring |
              AttrSelectorOperator::Suffix => Feature::CssSel3,
            }
          }
        }
        Component::AttributeOther(attr) => {
          match attr.operation {
            ParsedAttrSelectorOperation::Exists => Feature::CssSel2,
            ParsedAttrSelectorOperation::WithValue { operator, case_sensitivity, .. } => {
              if case_sensitivity != ParsedCaseSensitivity::CaseSensitive {
                Feature::CssCaseInsensitive
              } else {
                match operator {
                  AttrSelectorOperator::Equal |
                  AttrSelectorOperator::Includes |
                  AttrSelectorOperator::DashMatch => Feature::CssSel2,
                  AttrSelectorOperator::Prefix |
                  AttrSelectorOperator::Substring |
                  AttrSelectorOperator::Suffix => Feature::CssSel3,
                }
              }
            }
          }
        }

        Component::FirstChild => Feature::CssSel2,

        Component::Empty |
        Component::FirstOfType |
        Component::LastChild |
        Component::LastOfType |
        Component::Negation(_) |
        Component::NthChild(_, _) |
        Component::NthLastChild(_, _) |
        Component::NthLastOfType(_, _) |
        Component::NthOfType(_, _) |
        Component::OnlyChild |
        Component::OnlyOfType |
        Component::Root => Feature::CssSel3,

        Component::Is(_) => Feature::CssMatchesPseudo,

        Component::Scope |
        Component::Host(_) |
        Component::Slotted(_) => Feature::Shadowdomv1,

        Component::Part(_) |
        Component::Where(_) => return false, // TODO: find this data in caniuse-lite

        Component::NonTSPseudoClass(pseudo) => {
          match pseudo {
            PseudoClass::Link |
            PseudoClass::Visited |
            PseudoClass::Active |
            PseudoClass::Hover |
            PseudoClass::Focus |
            PseudoClass::Lang(_) => Feature::CssSel2,

            PseudoClass::Checked |
            PseudoClass::Disabled |
            PseudoClass::Enabled |
            PseudoClass::Target => Feature::CssSel3,

            PseudoClass::AnyLink => Feature::CssAnyLink,
            PseudoClass::Indeterminate => Feature::CssIndeterminatePseudo,
            
            PseudoClass::Fullscreen => Feature::Fullscreen,
            
            PseudoClass::FocusVisible => Feature::CssFocusVisible,
            PseudoClass::FocusWithin => Feature::CssFocusWithin,
            PseudoClass::Default => Feature::CssDefaultPseudo,
            PseudoClass::Dir(_) => Feature::CssDirPseudo,
            PseudoClass::Optional => Feature::CssOptionalPseudo,
            PseudoClass::PlaceholderShown => Feature::CssPlaceholderShown,

            PseudoClass::ReadOnly |
            PseudoClass::ReadWrite => Feature::CssReadOnlyWrite,

            PseudoClass::Valid |
            PseudoClass::Invalid |
            PseudoClass::Required => Feature::FormValidation,

            PseudoClass::InRange |
            PseudoClass::OutOfRange => Feature::CssInOutOfRange,

            PseudoClass::Autofill => Feature::CssAutofill,

            // Experimental, no browser support.
            PseudoClass::Current |
            PseudoClass::Past |
            PseudoClass::Future |
            PseudoClass::Playing |
            PseudoClass::Paused |
            PseudoClass::Seeking |
            PseudoClass::Stalled |
            PseudoClass::Buffering |
            PseudoClass::Muted |
            PseudoClass::VolumeLocked |
            PseudoClass::TargetWithin |
            PseudoClass::LocalLink |
            PseudoClass::Blank |
            PseudoClass::UserInvalid |
            PseudoClass::UserValid |
            PseudoClass::Defined => return false,

            PseudoClass::Custom(_) => return false
          }
        }

        Component::PseudoElement(pseudo) => {
          match pseudo {
            PseudoElement::After |
            PseudoElement::Before => Feature::CssGencontent,
            PseudoElement::FirstLine => Feature::CssFirstLine,
            PseudoElement::FirstLetter => Feature::CssFirstLetter,
            PseudoElement::Selection => Feature::CssSelection,
            PseudoElement::Placeholder => Feature::CssPlaceholder,
            PseudoElement::Marker => Feature::CssMarkerPseudo,
            PseudoElement::Backdrop => Feature::Dialog,
            PseudoElement::Custom(_) => return false
          }
        }

        Component::Combinator(combinator) => {
          match combinator {
            Combinator::Child | Combinator::NextSibling => Feature::CssSel2,
            Combinator::LaterSibling => Feature::CssSel3,
            _ => continue
          }
        }
      };

      if let Some(targets) = targets {
        if !feature.is_compatible(targets) {
          return false
        }
      } else {
        return false
      }
    }
  }
  true
}
