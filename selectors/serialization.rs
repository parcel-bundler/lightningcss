use crate::{
  attr::{
    AttrSelectorOperator, AttrSelectorWithOptionalNamespace, NamespaceConstraint, ParsedAttrSelectorOperation,
    ParsedCaseSensitivity,
  },
  builder::SelectorBuilder,
  parser::{Combinator, Component, LocalName, Selector},
  SelectorImpl,
};
use std::borrow::Cow;

#[cfg(feature = "jsonschema")]
use schemars::JsonSchema;

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
#[cfg_attr(
  feature = "jsonschema",
  derive(schemars::JsonSchema),
  schemars(
    bound = "Impl: JsonSchema, Impl::NonTSPseudoClass: schemars::JsonSchema, Impl::PseudoElement: schemars::JsonSchema, Impl::VendorPrefix: schemars::JsonSchema, PseudoClass: schemars::JsonSchema, PseudoElement: schemars::JsonSchema, VendorPrefix: schemars::JsonSchema"
  )
)]
enum SerializedComponent<'i, 's, Impl: SelectorImpl<'s>, PseudoClass, PseudoElement, VendorPrefix> {
  Combinator {
    value: Combinator,
  },
  Universal,
  #[serde(borrow)]
  Namespace(Namespace<'i>),
  Type {
    name: Cow<'i, str>,
  },
  #[serde(rename = "id")]
  ID {
    name: Cow<'i, str>,
  },
  Class {
    name: Cow<'i, str>,
  },
  Attribute(AttrSelector<'i>),
  #[serde(
    borrow,
    bound(
      serialize = "PseudoClass: serde::Serialize, Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize, VendorPrefix: serde::Serialize",
      deserialize = "PseudoClass: serde::Deserialize<'de>, Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>, VendorPrefix: serde::Deserialize<'de>"
    )
  )]
  PseudoClass(SerializedPseudoClass<'s, Impl, PseudoClass, VendorPrefix>),
  #[serde(
    borrow,
    bound(
      serialize = "PseudoElement: serde::Serialize",
      deserialize = "PseudoElement: serde::Deserialize<'de>"
    )
  )]
  PseudoElement(SerializedPseudoElement<'i, 's, Impl, PseudoElement>),
  Nesting,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[serde(tag = "value", rename_all = "kebab-case")]
enum Namespace<'i> {
  None,
  Any,
  Default { url: Cow<'i, str> },
  Some { prefix: Cow<'i, str>, url: Cow<'i, str> },
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
#[cfg_attr(
  feature = "jsonschema",
  derive(schemars::JsonSchema),
  schemars(
    bound = "Impl: JsonSchema, Impl::NonTSPseudoClass: schemars::JsonSchema, Impl::PseudoElement: schemars::JsonSchema, Impl::VendorPrefix: schemars::JsonSchema, VendorPrefix: schemars::JsonSchema"
  )
)]
enum TSPseudoClass<'s, Impl: SelectorImpl<'s>, VendorPrefix> {
  Not {
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>"
      )
    )]
    selectors: Box<[Selector<'s, Impl>]>,
  },
  FirstChild,
  LastChild,
  OnlyChild,
  Root,
  Empty,
  Scope,
  NthChild {
    a: i32,
    b: i32,
  },
  NthLastChild {
    a: i32,
    b: i32,
  },
  NthCol {
    a: i32,
    b: i32,
  },
  NthLastCol {
    a: i32,
    b: i32,
  },
  NthOfType {
    a: i32,
    b: i32,
  },
  NthLastOfType {
    a: i32,
    b: i32,
  },
  FirstOfType,
  LastOfType,
  OnlyOfType,
  Host {
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
      )
    )]
    selectors: Option<Selector<'s, Impl>>,
  },
  Where {
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
      )
    )]
    selectors: Box<[Selector<'s, Impl>]>,
  },
  Is {
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
      )
    )]
    selectors: Box<[Selector<'s, Impl>]>,
  },
  #[serde(rename_all = "camelCase")]
  Any {
    vendor_prefix: VendorPrefix,
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
      )
    )]
    selectors: Box<[Selector<'s, Impl>]>,
  },
  Has {
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
      )
    )]
    selectors: Box<[Selector<'s, Impl>]>,
  },
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged, rename_all = "kebab-case")]
#[cfg_attr(
  feature = "jsonschema",
  derive(schemars::JsonSchema),
  schemars(
    bound = "Impl: JsonSchema, Impl::NonTSPseudoClass: schemars::JsonSchema, Impl::PseudoElement: schemars::JsonSchema, Impl::VendorPrefix: schemars::JsonSchema, PseudoClass: schemars::JsonSchema, VendorPrefix: schemars::JsonSchema"
  )
)]
enum SerializedPseudoClass<'s, Impl: SelectorImpl<'s>, PseudoClass, VendorPrefix> {
  #[serde(
    borrow,
    bound(
      serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize, VendorPrefix: serde::Serialize",
      deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>, VendorPrefix: serde::Deserialize<'de>"
    )
  )]
  TS(TSPseudoClass<'s, Impl, VendorPrefix>),
  NonTS(PseudoClass),
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
#[cfg_attr(
  feature = "jsonschema",
  derive(schemars::JsonSchema),
  schemars(
    bound = "Impl: JsonSchema, Impl::NonTSPseudoClass: schemars::JsonSchema, Impl::PseudoElement: schemars::JsonSchema, Impl::VendorPrefix: schemars::JsonSchema"
  )
)]
enum BuiltinPseudoElement<'i, 's, Impl: SelectorImpl<'s>> {
  Slotted {
    #[serde(
      borrow,
      bound(
        serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
        deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
      )
    )]
    selector: Selector<'s, Impl>,
  },
  Part {
    names: Vec<Cow<'i, str>>,
  },
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged, rename_all = "kebab-case")]
#[cfg_attr(
  feature = "jsonschema",
  derive(schemars::JsonSchema),
  schemars(
    bound = "Impl: JsonSchema, Impl::NonTSPseudoClass: schemars::JsonSchema, Impl::PseudoElement: schemars::JsonSchema, Impl::VendorPrefix: schemars::JsonSchema, PseudoElement: schemars::JsonSchema"
  )
)]
enum SerializedPseudoElement<'i, 's, Impl: SelectorImpl<'s>, PseudoElement> {
  #[serde(
    borrow,
    bound(
      serialize = "Impl::NonTSPseudoClass: serde::Serialize, Impl::PseudoElement: serde::Serialize, Impl::VendorPrefix: serde::Serialize",
      deserialize = "Impl::NonTSPseudoClass: serde::Deserialize<'de>, Impl::PseudoElement: serde::Deserialize<'de>, Impl::VendorPrefix: serde::Deserialize<'de>"
    )
  )]
  Builtin(BuiltinPseudoElement<'i, 's, Impl>),
  Custom(PseudoElement),
}

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
struct AttrSelector<'i> {
  #[serde(borrow)]
  namespace: Option<NamespaceConstraint<NamespaceValue<'i>>>,
  name: Cow<'i, str>,
  operation: Option<AttrOperation<'i>>,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
struct NamespaceValue<'i> {
  prefix: Cow<'i, str>,
  url: Cow<'i, str>,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
struct AttrOperation<'i> {
  operator: AttrSelectorOperator,
  value: Cow<'i, str>,
  #[serde(default)]
  case_sensitivity: ParsedCaseSensitivity,
}

impl<'i, Impl: SelectorImpl<'i>> serde::Serialize for Component<'i, Impl>
where
  Impl::NonTSPseudoClass: serde::Serialize,
  Impl::PseudoElement: serde::Serialize,
  Impl::VendorPrefix: serde::Serialize,
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let c: SerializedComponent<'_, 'i, Impl, _, _, _> = match self {
      Component::Combinator(c) => SerializedComponent::Combinator { value: c.clone() },
      Component::ExplicitUniversalType => SerializedComponent::Universal,
      Component::ExplicitAnyNamespace => SerializedComponent::Namespace(Namespace::Any),
      Component::ExplicitNoNamespace => SerializedComponent::Namespace(Namespace::None),
      Component::DefaultNamespace(url) => SerializedComponent::Namespace(Namespace::Default {
        url: url.as_ref().into(),
      }),
      Component::Namespace(prefix, url) => SerializedComponent::Namespace(Namespace::Some {
        prefix: prefix.as_ref().into(),
        url: url.as_ref().into(),
      }),
      Component::LocalName(name) => SerializedComponent::Type {
        name: name.name.as_ref().into(),
      },
      Component::ID(name) => SerializedComponent::ID {
        name: name.as_ref().into(),
      },
      Component::Class(name) => SerializedComponent::Class {
        name: name.as_ref().into(),
      },
      Component::AttributeInNoNamespace {
        local_name,
        operator,
        value,
        case_sensitivity,
        never_matches,
      } => SerializedComponent::Attribute(AttrSelector {
        namespace: None,
        name: local_name.as_ref().into(),
        operation: Some(AttrOperation {
          operator: operator.clone(),
          case_sensitivity: case_sensitivity.clone(),
          value: value.as_ref().into(),
        }),
      }),
      Component::AttributeInNoNamespaceExists { local_name, .. } => SerializedComponent::Attribute(AttrSelector {
        namespace: None,
        name: local_name.as_ref().into(),
        operation: None,
      }),
      Component::AttributeOther(other) => SerializedComponent::Attribute(AttrSelector {
        namespace: other.namespace.as_ref().map(|namespace| match namespace {
          NamespaceConstraint::Any => NamespaceConstraint::Any,
          NamespaceConstraint::Specific(s) => NamespaceConstraint::Specific(NamespaceValue {
            prefix: s.0.as_ref().into(),
            url: s.1.as_ref().into(),
          }),
        }),
        name: other.local_name.as_ref().into(),
        operation: match &other.operation {
          ParsedAttrSelectorOperation::Exists => None,
          ParsedAttrSelectorOperation::WithValue {
            operator,
            case_sensitivity,
            expected_value,
          } => Some(AttrOperation {
            operator: operator.clone(),
            case_sensitivity: case_sensitivity.clone(),
            value: expected_value.as_ref().into(),
          }),
        },
      }),
      Component::NonTSPseudoClass(c) => SerializedComponent::PseudoClass(SerializedPseudoClass::NonTS(c)),
      Component::Negation(s) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Not { selectors: s.clone() }))
      }
      Component::FirstChild => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::FirstChild))
      }
      Component::LastChild => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::LastChild))
      }
      Component::OnlyChild => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::OnlyChild))
      }
      Component::Root => SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Root)),
      Component::Empty => SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Empty)),
      Component::Scope => SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Scope)),
      Component::FirstOfType => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::FirstOfType))
      }
      Component::LastOfType => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::LastOfType))
      }
      Component::OnlyOfType => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::OnlyOfType))
      }
      Component::NthChild(a, b) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::NthChild { a: *a, b: *b }))
      }
      Component::NthLastChild(a, b) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::NthLastChild { a: *a, b: *b }))
      }
      Component::NthCol(a, b) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::NthCol { a: *a, b: *b }))
      }
      Component::NthLastCol(a, b) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::NthLastCol { a: *a, b: *b }))
      }
      Component::NthOfType(a, b) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::NthOfType { a: *a, b: *b }))
      }
      Component::NthLastOfType(a, b) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::NthLastOfType { a: *a, b: *b }))
      }
      Component::Host(s) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Host { selectors: s.clone() }))
      }
      Component::Where(s) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Where { selectors: s.clone() }))
      }
      Component::Is(s) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Is { selectors: s.clone() }))
      }
      Component::Any(v, s) => SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Any {
        vendor_prefix: v.clone(),
        selectors: s.clone(),
      })),
      Component::Has(s) => {
        SerializedComponent::PseudoClass(SerializedPseudoClass::TS(TSPseudoClass::Has { selectors: s.clone() }))
      }
      Component::PseudoElement(e) => SerializedComponent::PseudoElement(SerializedPseudoElement::Custom(e)),
      Component::Slotted(s) => {
        SerializedComponent::PseudoElement(SerializedPseudoElement::Builtin(BuiltinPseudoElement::Slotted {
          selector: s.clone(),
        }))
      }
      Component::Part(p) => {
        SerializedComponent::PseudoElement(SerializedPseudoElement::Builtin(BuiltinPseudoElement::Part {
          names: p.iter().map(|name| name.as_ref().into()).collect(),
        }))
      }
      Component::Nesting => SerializedComponent::Nesting,
    };

    c.serialize(serializer)
  }
}

impl<'de: 'i, 'i, Impl: SelectorImpl<'i>> serde::Deserialize<'de> for Component<'i, Impl>
where
  Impl::NonTSPseudoClass: serde::Deserialize<'de>,
  Impl::PseudoElement: serde::Deserialize<'de>,
  Impl::VendorPrefix: serde::Deserialize<'de>,
{
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let c: SerializedComponent<'i, '_, Impl, _, _, _> = SerializedComponent::deserialize(deserializer)?;
    Ok(match c {
      SerializedComponent::Combinator { value } => Component::Combinator(value),
      SerializedComponent::Universal => Component::ExplicitUniversalType,
      SerializedComponent::Namespace(n) => match n {
        Namespace::Any => Component::ExplicitAnyNamespace,
        Namespace::None => Component::ExplicitNoNamespace,
        Namespace::Default { url } => Component::DefaultNamespace(url.into()),
        Namespace::Some { prefix, url } => Component::Namespace(prefix.into(), url.into()),
      },
      SerializedComponent::Type { name } => {
        let name: Impl::LocalName = name.into();
        Component::LocalName(LocalName {
          name: name.clone(),
          lower_name: name,
        })
      }
      SerializedComponent::ID { name } => Component::ID(name.into()),
      SerializedComponent::Class { name } => Component::Class(name.into()),
      SerializedComponent::Attribute(attr) => {
        let (local_name_lower_cow, local_name_is_ascii_lowercase) =
          if let Some(first_uppercase) = attr.name.bytes().position(|byte| byte >= b'A' && byte <= b'Z') {
            let mut string = attr.name.to_string();
            string[first_uppercase..].make_ascii_lowercase();
            (string.into(), false)
          } else {
            (attr.name.clone(), true)
          };

        if let Some(namespace) = attr.namespace {
          Component::AttributeOther(Box::new(AttrSelectorWithOptionalNamespace {
            namespace: Some(match namespace {
              NamespaceConstraint::Any => NamespaceConstraint::Any,
              NamespaceConstraint::Specific(c) => NamespaceConstraint::Specific((c.prefix.into(), c.url.into())),
            }),
            local_name: attr.name.into(),
            local_name_lower: local_name_lower_cow.into(),
            operation: match attr.operation {
              None => ParsedAttrSelectorOperation::Exists,
              Some(AttrOperation {
                operator,
                case_sensitivity,
                value,
              }) => ParsedAttrSelectorOperation::WithValue {
                operator,
                case_sensitivity,
                expected_value: value.into(),
              },
            },
            never_matches: false, // TODO
          }))
        } else {
          match attr.operation {
            None => Component::AttributeInNoNamespaceExists {
              local_name: attr.name.into(),
              local_name_lower: local_name_lower_cow.into(),
            },
            Some(AttrOperation {
              operator,
              case_sensitivity,
              value,
            }) => Component::AttributeInNoNamespace {
              local_name: attr.name.into(),
              operator,
              value: value.into(),
              case_sensitivity,
              never_matches: false, // TODO
            },
          }
        }
      }
      SerializedComponent::PseudoClass(c) => match c {
        SerializedPseudoClass::NonTS(c) => Component::NonTSPseudoClass(c),
        SerializedPseudoClass::TS(TSPseudoClass::Not { selectors }) => Component::Negation(selectors),
        SerializedPseudoClass::TS(TSPseudoClass::FirstChild) => Component::FirstChild,
        SerializedPseudoClass::TS(TSPseudoClass::LastChild) => Component::LastChild,
        SerializedPseudoClass::TS(TSPseudoClass::OnlyChild) => Component::OnlyChild,
        SerializedPseudoClass::TS(TSPseudoClass::Root) => Component::Root,
        SerializedPseudoClass::TS(TSPseudoClass::Empty) => Component::Empty,
        SerializedPseudoClass::TS(TSPseudoClass::Scope) => Component::Scope,
        SerializedPseudoClass::TS(TSPseudoClass::FirstOfType) => Component::FirstOfType,
        SerializedPseudoClass::TS(TSPseudoClass::LastOfType) => Component::LastOfType,
        SerializedPseudoClass::TS(TSPseudoClass::OnlyOfType) => Component::OnlyOfType,
        SerializedPseudoClass::TS(TSPseudoClass::NthChild { a, b }) => Component::NthChild(a, b),
        SerializedPseudoClass::TS(TSPseudoClass::NthLastChild { a, b }) => Component::NthLastChild(a, b),
        SerializedPseudoClass::TS(TSPseudoClass::NthCol { a, b }) => Component::NthCol(a, b),
        SerializedPseudoClass::TS(TSPseudoClass::NthLastCol { a, b }) => Component::NthLastCol(a, b),
        SerializedPseudoClass::TS(TSPseudoClass::NthOfType { a, b }) => Component::NthOfType(a, b),
        SerializedPseudoClass::TS(TSPseudoClass::NthLastOfType { a, b }) => Component::NthLastOfType(a, b),
        SerializedPseudoClass::TS(TSPseudoClass::Host { selectors }) => Component::Host(selectors),
        SerializedPseudoClass::TS(TSPseudoClass::Where { selectors }) => Component::Where(selectors),
        SerializedPseudoClass::TS(TSPseudoClass::Is { selectors }) => Component::Is(selectors),
        SerializedPseudoClass::TS(TSPseudoClass::Any {
          vendor_prefix,
          selectors,
        }) => Component::Any(vendor_prefix, selectors),
        SerializedPseudoClass::TS(TSPseudoClass::Has { selectors }) => Component::Has(selectors),
      },
      SerializedComponent::PseudoElement(value) => match value {
        SerializedPseudoElement::Custom(e) => Component::PseudoElement(e),
        SerializedPseudoElement::Builtin(BuiltinPseudoElement::Part { names }) => {
          Component::Part(names.into_iter().map(|name| name.into()).collect())
        }
        SerializedPseudoElement::Builtin(BuiltinPseudoElement::Slotted { selector }) => {
          Component::Slotted(selector)
        }
      },
      SerializedComponent::Nesting => Component::Nesting,
    })
  }
}

impl<'i, Impl: SelectorImpl<'i>> serde::Serialize for Selector<'i, Impl>
where
  Impl::NonTSPseudoClass: serde::Serialize,
  Impl::VendorPrefix: serde::Serialize,
  Impl::PseudoElement: serde::Serialize,
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    use serde::ser::SerializeSeq;
    let skipped_combinators = self
      .iter_raw_match_order()
      .filter(|c| {
        matches!(
          c,
          Component::Combinator(Combinator::Part | Combinator::PseudoElement | Combinator::SlotAssignment)
        )
      })
      .count();
    let mut seq = serializer.serialize_seq(Some(self.len() - skipped_combinators))?;

    let mut combinators = self.iter_raw_match_order().rev().filter(|x| x.is_combinator());
    let compound_selectors = self.iter_raw_match_order().as_slice().split(|x| x.is_combinator()).rev();

    for compound in compound_selectors {
      if compound.is_empty() {
        continue;
      }

      for component in compound {
        seq.serialize_element(component)?;
      }

      if let Some(combinator) = combinators.next() {
        if !matches!(
          combinator,
          Component::Combinator(Combinator::Part | Combinator::PseudoElement | Combinator::SlotAssignment)
        ) {
          seq.serialize_element(combinator)?;
        }
      }
    }
    seq.end()
  }
}

impl<'de: 'i, 'i, Impl: SelectorImpl<'i>> serde::Deserialize<'de> for Selector<'i, Impl>
where
  Impl::NonTSPseudoClass: serde::Deserialize<'de>,
  Impl::VendorPrefix: serde::Deserialize<'de>,
  Impl::PseudoElement: serde::Deserialize<'de>,
{
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    #[cfg(feature = "serde")]
    struct SelectorVisitor<'i, Impl: SelectorImpl<'i>> {
      marker: std::marker::PhantomData<Selector<'i, Impl>>,
    }

    #[cfg(feature = "serde")]
    impl<'de: 'i, 'i, Impl: SelectorImpl<'i>> serde::de::Visitor<'de> for SelectorVisitor<'i, Impl>
    where
      Impl::NonTSPseudoClass: serde::Deserialize<'de>,
      Impl::VendorPrefix: serde::Deserialize<'de>,
      Impl::PseudoElement: serde::Deserialize<'de>,
    {
      type Value = Selector<'i, Impl>;

      fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a list of components")
      }

      fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
      where
        A: serde::de::SeqAccess<'de>,
      {
        let mut builder = SelectorBuilder::default();
        while let Some(component) = seq.next_element::<Component<'i, Impl>>()? {
          if let Some(combinator) = component.as_combinator() {
            builder.push_combinator(combinator);
          } else {
            match component {
              Component::Slotted(_) => builder.push_combinator(Combinator::SlotAssignment),
              Component::Part(_) => builder.push_combinator(Combinator::Part),
              Component::PseudoElement(_) => builder.push_combinator(Combinator::PseudoElement),
              _ => {}
            }
            builder.push_simple_selector(component);
          }
        }

        let (spec, components) = builder.build(false, false, false);
        Ok(Selector::new(spec, components))
      }
    }

    deserializer.deserialize_seq(SelectorVisitor {
      marker: std::marker::PhantomData,
    })
  }
}

#[cfg(feature = "jsonschema")]
impl<'i, Impl: SelectorImpl<'i>> schemars::JsonSchema for Selector<'i, Impl>
where
  Impl: schemars::JsonSchema,
  Impl::NonTSPseudoClass: schemars::JsonSchema,
  Impl::PseudoElement: schemars::JsonSchema,
  Impl::VendorPrefix: schemars::JsonSchema,
{
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    Vec::<SerializedComponent<'_, '_, Impl, Impl::NonTSPseudoClass, Impl::PseudoElement, Impl::VendorPrefix>>::json_schema(gen)
  }

  fn schema_name() -> String {
    "Selector".into()
  }
}
