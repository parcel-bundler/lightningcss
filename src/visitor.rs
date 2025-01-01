//! Visitors for traversing the values in a StyleSheet.
//!
//! The [Visitor](Visitor) trait includes methods for visiting and transforming rules, properties, and values within a StyleSheet.
//! Each value implements the [Visit](Visit) trait, which knows how to visit the value itself, as well as its children.
//! A Visitor is configured to only visit specific types of values using [VisitTypes](VisitTypes) flags. This enables
//! entire branches to be skipped when a type does not contain any relevant values.
//!
//! # Example
//!
//! This example transforms a stylesheet, adding a prefix to all URLs, and converting pixels to rems.
//!
//! ```
//! use std::convert::Infallible;
//! use lightningcss::{
//!   stylesheet::{StyleSheet, ParserOptions, PrinterOptions},
//!   visitor::{Visitor, Visit, VisitTypes},
//!   visit_types,
//!   values::length::LengthValue,
//!   values::url::Url
//! };
//!
//! let mut stylesheet = StyleSheet::parse(
//!   r#"
//!     .foo {
//!       background: url(bg.png);
//!       width: 32px;
//!     }
//!   "#,
//!   ParserOptions::default()
//! ).unwrap();
//!
//! struct MyVisitor;
//! impl<'i> Visitor<'i> for MyVisitor {
//!   type Error = Infallible;
//!
//!   fn visit_types(&self) -> VisitTypes {
//!     visit_types!(URLS | LENGTHS)
//!   }
//!
//!   fn visit_url(&mut self, url: &mut Url<'i>) -> Result<(), Self::Error> {
//!     url.url = format!("https://mywebsite.com/{}", url.url).into();
//!     Ok(())
//!   }
//!
//!   fn visit_length(&mut self, length: &mut LengthValue) -> Result<(), Self::Error> {
//!     match length {
//!       LengthValue::Px(px) => *length = LengthValue::Rem(*px / 16.0),
//!       _ => {}
//!     }
//!
//!     Ok(())
//!   }
//! }
//!
//! stylesheet.visit(&mut MyVisitor).unwrap();
//!
//! let res = stylesheet.to_css(PrinterOptions { minify: true, ..Default::default() }).unwrap();
//! assert_eq!(res.code, ".foo{background:url(https://mywebsite.com/bg.png);width:2rem}");
//! ```

use crate::{
  declaration::DeclarationBlock,
  media_query::{MediaFeature, MediaFeatureValue, MediaList, MediaQuery},
  parser::DefaultAtRule,
  properties::{
    custom::{EnvironmentVariable, Function, TokenList, TokenOrValue, Variable},
    Property,
  },
  rules::{supports::SupportsCondition, CssRule, CssRuleList},
  selector::{Selector, SelectorList},
  stylesheet::StyleSheet,
  values::{
    angle::Angle,
    color::CssColor,
    ident::{CustomIdent, DashedIdent},
    image::Image,
    length::LengthValue,
    ratio::Ratio,
    resolution::Resolution,
    time::Time,
    url::Url,
  },
};
use bitflags::bitflags;
use indexmap::IndexMap;
use smallvec::SmallVec;

pub(crate) use lightningcss_derive::Visit;

bitflags! {
  /// Describes what a [Visitor](Visitor) will visit when traversing a StyleSheet.
  ///
  /// Flags may be combined to visit multiple types. The [visit_types](visit_types) macro allows
  /// combining flags in a `const` expression.
  #[derive(PartialEq, Eq, Clone, Copy)]
  pub struct VisitTypes: u32 {
    /// Visit rules.
    const RULES = 1 << 0;
    /// Visit properties;
    const PROPERTIES = 1 << 1;
    /// Visit urls.
    const URLS = 1 << 2;
    /// Visit colors.
    const COLORS = 1 << 3;
    /// Visit images.
    const IMAGES = 1 << 4;
    /// Visit lengths.
    const LENGTHS = 1 << 5;
    /// Visit angles.
    const ANGLES = 1 << 6;
    /// Visit ratios.
    const RATIOS = 1 << 7;
    /// Visit resolutions.
    const RESOLUTIONS = 1 << 8;
    /// Visit times.
    const TIMES = 1 << 9;
    /// Visit custom identifiers.
    const CUSTOM_IDENTS = 1 << 10;
    /// Visit dashed identifiers.
    const DASHED_IDENTS = 1 << 11;
    /// Visit variables.
    const VARIABLES = 1 << 12;
    /// Visit environment variables.
    const ENVIRONMENT_VARIABLES = 1 << 13;
    /// Visit media queries.
    const MEDIA_QUERIES = 1 << 14;
    /// Visit supports conditions.
    const SUPPORTS_CONDITIONS = 1 << 15;
    /// Visit selectors.
    const SELECTORS = 1 << 16;
    /// Visit custom functions.
    const FUNCTIONS = 1 << 17;
    /// Visit a token.
    const TOKENS = 1 << 18;
  }
}

/// Constructs a constant [VisitTypes](VisitTypes) from flags.
#[macro_export]
macro_rules! visit_types {
  ($( $flag: ident )|+) => {
    $crate::visitor::VisitTypes::from_bits_truncate(0 $(| $crate::visitor::VisitTypes::$flag.bits())+)
  }
}

/// A trait for visiting or transforming rules, properties, and values in a StyleSheet.
pub trait Visitor<'i, T: Visit<'i, T, Self> = DefaultAtRule> {
  /// The `Err` value for `Result`s returned by `visit_*` methods.
  type Error;

  /// Returns the types of values that this visitor should visit. By default, it returns
  /// `Self::TYPES`, but this can be overridden to change the value at runtime.
  fn visit_types(&self) -> VisitTypes;

  /// Visits a stylesheet.
  #[inline]
  fn visit_stylesheet<'o>(&mut self, stylesheet: &mut StyleSheet<'i, 'o, T>) -> Result<(), Self::Error> {
    stylesheet.visit_children(self)
  }

  /// Visits a rule list.
  #[inline]
  fn visit_rule_list(&mut self, rules: &mut CssRuleList<'i, T>) -> Result<(), Self::Error> {
    rules.visit_children(self)
  }

  /// Visits a rule.
  #[inline]
  fn visit_rule(&mut self, rule: &mut CssRule<'i, T>) -> Result<(), Self::Error> {
    rule.visit_children(self)
  }

  /// Visits a declaration block.
  #[inline]
  fn visit_declaration_block(&mut self, decls: &mut DeclarationBlock<'i>) -> Result<(), Self::Error> {
    decls.visit_children(self)
  }

  /// Visits a property.
  #[inline]
  fn visit_property(&mut self, property: &mut Property<'i>) -> Result<(), Self::Error> {
    property.visit_children(self)
  }

  /// Visits a url.
  fn visit_url(&mut self, _url: &mut Url<'i>) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a color.
  #[allow(unused_variables)]
  fn visit_color(&mut self, color: &mut CssColor) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits an image.
  #[inline]
  fn visit_image(&mut self, image: &mut Image<'i>) -> Result<(), Self::Error> {
    image.visit_children(self)
  }

  /// Visits a length.
  #[allow(unused_variables)]
  fn visit_length(&mut self, length: &mut LengthValue) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits an angle.
  #[allow(unused_variables)]
  fn visit_angle(&mut self, angle: &mut Angle) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a ratio.
  #[allow(unused_variables)]
  fn visit_ratio(&mut self, ratio: &mut Ratio) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a resolution.
  #[allow(unused_variables)]
  fn visit_resolution(&mut self, resolution: &mut Resolution) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a time.
  #[allow(unused_variables)]
  fn visit_time(&mut self, time: &mut Time) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a custom ident.
  #[allow(unused_variables)]
  fn visit_custom_ident(&mut self, ident: &mut CustomIdent) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a dashed ident.
  #[allow(unused_variables)]
  fn visit_dashed_ident(&mut self, ident: &mut DashedIdent) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a variable reference.
  #[inline]
  fn visit_variable(&mut self, var: &mut Variable<'i>) -> Result<(), Self::Error> {
    var.visit_children(self)
  }

  /// Visits an environment variable reference.
  #[inline]
  fn visit_environment_variable(&mut self, env: &mut EnvironmentVariable<'i>) -> Result<(), Self::Error> {
    env.visit_children(self)
  }

  /// Visits a media query list.
  #[inline]
  fn visit_media_list(&mut self, media: &mut MediaList<'i>) -> Result<(), Self::Error> {
    media.visit_children(self)
  }

  /// Visits a media query.
  #[inline]
  fn visit_media_query(&mut self, query: &mut MediaQuery<'i>) -> Result<(), Self::Error> {
    query.visit_children(self)
  }

  /// Visits a media feature.
  #[inline]
  fn visit_media_feature(&mut self, feature: &mut MediaFeature<'i>) -> Result<(), Self::Error> {
    feature.visit_children(self)
  }

  /// Visits a media feature value.
  #[inline]
  fn visit_media_feature_value(&mut self, value: &mut MediaFeatureValue<'i>) -> Result<(), Self::Error> {
    value.visit_children(self)
  }

  /// Visits a supports condition.
  #[inline]
  fn visit_supports_condition(&mut self, condition: &mut SupportsCondition<'i>) -> Result<(), Self::Error> {
    condition.visit_children(self)
  }

  /// Visits a selector list.
  #[inline]
  fn visit_selector_list(&mut self, selectors: &mut SelectorList<'i>) -> Result<(), Self::Error> {
    selectors.visit_children(self)
  }

  /// Visits a selector.
  #[allow(unused_variables)]
  fn visit_selector(&mut self, selector: &mut Selector<'i>) -> Result<(), Self::Error> {
    Ok(())
  }

  /// Visits a custom function.
  #[inline]
  fn visit_function(&mut self, function: &mut Function<'i>) -> Result<(), Self::Error> {
    function.visit_children(self)
  }

  /// Visits a token list.
  #[inline]
  fn visit_token_list(&mut self, tokens: &mut TokenList<'i>) -> Result<(), Self::Error> {
    tokens.visit_children(self)
  }

  /// Visits a token or value in an unparsed property.
  #[inline]
  fn visit_token(&mut self, token: &mut TokenOrValue<'i>) -> Result<(), Self::Error> {
    token.visit_children(self)
  }
}

/// A trait for values that can be visited by a [Visitor](Visitor).
pub trait Visit<'i, T: Visit<'i, T, V>, V: ?Sized + Visitor<'i, T>> {
  /// The types of values contained within this value and its children.
  /// This is used to skip branches that don't have any values requested
  /// by the Visitor.
  const CHILD_TYPES: VisitTypes;

  /// Visits the value by calling an appropriate method on the Visitor.
  /// If no corresponding visitor method exists, then the children are visited.
  #[inline]
  fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.visit_children(visitor)
  }

  /// Visit the children of this value.
  fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error>;
}

impl<'i, T: Visit<'i, T, V>, V: ?Sized + Visitor<'i, T>, U: Visit<'i, T, V>> Visit<'i, T, V> for Option<U> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    if let Some(v) = self {
      v.visit(visitor)
    } else {
      Ok(())
    }
  }

  fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    if let Some(v) = self {
      v.visit_children(visitor)
    } else {
      Ok(())
    }
  }
}

impl<'i, T: Visit<'i, T, V>, V: ?Sized + Visitor<'i, T>, U: Visit<'i, T, V>> Visit<'i, T, V> for Box<U> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.as_mut().visit(visitor)
  }

  fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.as_mut().visit_children(visitor)
  }
}

impl<'i, T: Visit<'i, T, V>, V: ?Sized + Visitor<'i, T>, U: Visit<'i, T, V>> Visit<'i, T, V> for Vec<U> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.iter_mut().try_for_each(|v| v.visit(visitor))
  }

  fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.iter_mut().try_for_each(|v| v.visit_children(visitor))
  }
}

impl<'i, A: smallvec::Array<Item = U>, U: Visit<'i, T, V>, T: Visit<'i, T, V>, V: ?Sized + Visitor<'i, T>>
  Visit<'i, T, V> for SmallVec<A>
{
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.iter_mut().try_for_each(|v| v.visit(visitor))
  }

  fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.iter_mut().try_for_each(|v| v.visit_children(visitor))
  }
}

impl<'i, T, V, U, W> Visit<'i, T, V> for IndexMap<U, W>
where
  T: Visit<'i, T, V>,
  V: ?Sized + Visitor<'i, T>,
  W: Visit<'i, T, V>,
{
  const CHILD_TYPES: VisitTypes = W::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.iter_mut().try_for_each(|(_k, v)| v.visit(visitor))
  }

  fn visit_children(&mut self, visitor: &mut V) -> Result<(), V::Error> {
    self.iter_mut().try_for_each(|(_k, v)| v.visit_children(visitor))
  }
}

macro_rules! impl_visit {
  ($t: ty) => {
    impl<'i, V: ?Sized + Visitor<'i, T>, T: Visit<'i, T, V>> Visit<'i, T, V> for $t {
      const CHILD_TYPES: VisitTypes = VisitTypes::empty();

      fn visit_children(&mut self, _: &mut V) -> Result<(), V::Error> {
        Ok(())
      }
    }
  };
}

impl_visit!(u8);
impl_visit!(u16);
impl_visit!(u32);
impl_visit!(i32);
impl_visit!(f32);
impl_visit!(bool);
impl_visit!(char);
impl_visit!(str);
impl_visit!(String);
impl_visit!((f32, f32));
