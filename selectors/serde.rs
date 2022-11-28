use crate::{
  parser::{Combinator, Component, LocalName, Selector, SelectorList},
  SelectorImpl,
};
// use serde::{Deserialize, Serialize};

// #[cfg(feature = "serde")]
// #[derive(serde::Serialize, serde::Deserialize)]
// #[serde(tag = "type", content = "value", rename_all = "kebab-case")]
// enum SerializedComponent<'i, Impl: SelectorImpl<'i>> {
//   Combinator(Combinator),
//   Universal,
//   #[serde(borrow)]
//   Type(Impl::LocalName),
//   ID(Impl::Identifier),
//   Class(Impl::Identifier),
// }

// #[cfg(feature = "serde")]
// impl<'i> From<&Component<'i>> for SerializedComponent<'i> {
//   fn from(component: &Component<'i>) -> Self {
//     match component {
//       Component::Combinator(c) => SerializedComponent::Combinator(c.clone()),
//       Component::ExplicitUniversalType => SerializedComponent::Universal,
//       Component::LocalName(name) => SerializedComponent::Type(name.name.clone()),
//       Component::ID(name) => SerializedComponent::ID(name.clone()),
//       Component::Class(name) => SerializedComponent::Class(name.clone()),
//       _ => todo!(),
//     }
//   }
// }

// #[cfg(feature = "serde")]
// impl<'i> Into<Component<'i>> for SerializedComponent<'i> {
//   fn into(self) -> Component<'i> {
//     match self {
//       SerializedComponent::Combinator(c) => Component::Combinator(c),
//       SerializedComponent::Universal => Component::ExplicitUniversalType,
//       SerializedComponent::Type(name) => Component::LocalName(LocalName {
//         name: name.clone().into(),
//         lower_name: name.into(),
//       }),
//       SerializedComponent::ID(name) => Component::ID(name),
//       SerializedComponent::Class(name) => Component::Class(name),
//     }
//   }
// }

// impl<'i> Serizlie
