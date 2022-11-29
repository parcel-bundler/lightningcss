use std::borrow::Cow;

use cssparser::CowRcStr;

use crate::{
  attr::{
    AttrSelectorOperator, AttrSelectorWithOptionalNamespace, NamespaceConstraint, ParsedAttrSelectorOperation,
    ParsedCaseSensitivity,
  },
  parser::{Combinator, Component, LocalName, Selector, SelectorList},
  SelectorImpl,
};
// use serde::{Deserialize, Serialize};

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "kebab-case")]
enum SerializedComponent<'i, PseudoClass> {
  Combinator(Combinator),
  Universal,
  #[serde(borrow)]
  Type(Cow<'i, str>),
  ID(Cow<'i, str>),
  Class(Cow<'i, str>),
  Attribute(AttrSelector<'i>),
  PseudoClass(PseudoClass),
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct AttrSelector<'i> {
  #[serde(borrow)]
  namespace: Option<NamespaceConstraint<(Cow<'i, str>, Cow<'i, str>)>>,
  name: Cow<'i, str>,
  operation: Option<AttrOperation<'i>>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct AttrOperation<'i> {
  operator: AttrSelectorOperator,
  case_sensitivity: ParsedCaseSensitivity,
  value: Cow<'i, str>,
}

impl<'i, Impl: SelectorImpl<'i>> serde::Serialize for Component<'i, Impl> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let c = match self {
      Component::Combinator(c) => SerializedComponent::Combinator(c.clone()),
      Component::ExplicitUniversalType => SerializedComponent::Universal,
      Component::LocalName(name) => SerializedComponent::Type(name.name.as_ref().into()),
      Component::ID(name) => SerializedComponent::ID(name.as_ref().into()),
      Component::Class(name) => SerializedComponent::Class(name.as_ref().into()),
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
          NamespaceConstraint::Specific(s) => {
            NamespaceConstraint::Specific((s.0.as_ref().into(), s.1.as_ref().into()))
          }
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
      Component::NonTSPseudoClass(c) => SerializedComponent::PseudoClass(c),
      _ => todo!(),
    };

    c.serialize(serializer)
  }
}

impl<'de: 'i, 'i, Impl: SelectorImpl<'i>> serde::Deserialize<'de> for Component<'i, Impl>
where
  Impl::NonTSPseudoClass: serde::Deserialize<'de>,
{
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let c = SerializedComponent::deserialize(deserializer)?;
    Ok(match c {
      SerializedComponent::Combinator(c) => Component::Combinator(c),
      SerializedComponent::Universal => Component::ExplicitUniversalType,
      SerializedComponent::Type(name) => {
        let name: Impl::LocalName = name.into();
        Component::LocalName(LocalName {
          name: name.clone(),
          lower_name: name,
        })
      }
      SerializedComponent::ID(name) => Component::ID(name.into()),
      SerializedComponent::Class(name) => Component::Class(name.into()),
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
              NamespaceConstraint::Specific(c) => NamespaceConstraint::Specific((c.0.into(), c.1.into())),
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
      SerializedComponent::PseudoClass(c) => Component::NonTSPseudoClass(c),
    })
  }
}
