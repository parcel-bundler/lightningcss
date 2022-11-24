//! Visitor.

use crate::{
  media_query::MediaQuery,
  properties::{custom::Variable, Property},
  rules::{supports::SupportsCondition, CssRule},
  selector::Selector,
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
use smallvec::SmallVec;

pub use lightningcss_derive::Visit;

bitflags! {
  /// What to visit.
  pub struct VisitTypes: u16 {
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
    /// Visit media queries.
    const MEDIA_QUERIES = 1 << 13;
    /// Visit supports conditions.
    const SUPPORTS_CONDITIONS = 1 << 14;
    /// Visit selectors.
    const SELECTORS = 1 << 15;
  }
}

/// Constructs a constant VisitTypes from flags.
#[macro_export]
macro_rules! visit_types {
  ($( $flag: ident )|+) => {
    VisitTypes::from_bits_truncate(0 $(| VisitTypes::$flag.bits())+)
  }
}

/// A trait for visiting or transforming rules.
pub trait Visitor<'i, T>: Sized {
  /// What to visit.
  const TYPES: VisitTypes;

  /// Visits a rule.
  fn visit_rule(&mut self, rule: &mut CssRule<'i, T>);

  /// Visits a property.
  fn visit_property(&mut self, property: &mut Property<'i>) {
    property.visit_children(self)
  }

  /// Visits a url.
  fn visit_url(&mut self, _url: &mut Url<'i>) {}

  /// Visits a color.
  #[allow(unused_variables)]
  fn visit_color(&mut self, color: &mut CssColor) {}

  /// Visits an image.
  fn visit_image(&mut self, image: &mut Image<'i>) {
    image.visit_children(self)
  }

  /// Visits a length.
  #[allow(unused_variables)]
  fn visit_length(&mut self, length: &mut LengthValue) {}

  /// Visits an angle.
  #[allow(unused_variables)]
  fn visit_angle(&mut self, angle: &mut Angle) {}

  /// Visits a ratio.
  #[allow(unused_variables)]
  fn visit_ratio(&mut self, ratio: &mut Ratio) {}

  /// Visits a resolution.
  #[allow(unused_variables)]
  fn visit_resolution(&mut self, resolution: &mut Resolution) {}

  /// Visits a time.
  #[allow(unused_variables)]
  fn visit_time(&mut self, time: &mut Time) {}

  /// Visits a custom ident.
  #[allow(unused_variables)]
  fn visit_custom_ident(&mut self, ident: &mut CustomIdent) {}

  /// Visits a dashed ident.
  #[allow(unused_variables)]
  fn visit_dashed_ident(&mut self, ident: &mut DashedIdent) {}

  /// Visits a variable reference.
  fn visit_variable(&mut self, var: &mut Variable<'i>) {
    var.visit_children(self)
  }

  /// Visits a media query.
  fn visit_media_query(&mut self, query: &mut MediaQuery<'i>) {
    query.visit_children(self)
  }

  /// Visits a supports condition.
  fn visit_supports_condition(&mut self, condition: &mut SupportsCondition<'i>) {
    condition.visit_children(self)
  }

  /// Visits a selector.
  #[allow(unused_variables)]
  fn visit_selector(&mut self, selector: &mut Selector<'i>) {}
}

/// A trait for visiting the children of a rule.
pub trait Visit<'i, T, V: Visitor<'i, T>> {
  /// The types of children that this container has.
  const CHILD_TYPES: VisitTypes;

  /// Visit self.
  fn visit(&mut self, visitor: &mut V) {
    self.visit_children(visitor)
  }

  /// Visit the children of this rule..
  fn visit_children(&mut self, visitor: &mut V);
}

impl<'i, T, V: Visitor<'i, T>, U: Visit<'i, T, V>> Visit<'i, T, V> for Option<U> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) {
    if let Some(v) = self {
      v.visit(visitor)
    }
  }

  fn visit_children(&mut self, visitor: &mut V) {
    if let Some(v) = self {
      v.visit_children(visitor)
    }
  }
}

impl<'i, T, V: Visitor<'i, T>, U: Visit<'i, T, V>> Visit<'i, T, V> for Box<U> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) {
    self.as_mut().visit(visitor)
  }

  fn visit_children(&mut self, visitor: &mut V) {
    self.as_mut().visit_children(visitor)
  }
}

impl<'i, T, V: Visitor<'i, T>, U: Visit<'i, T, V>> Visit<'i, T, V> for Vec<U> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) {
    for v in self {
      v.visit(visitor)
    }
  }

  fn visit_children(&mut self, visitor: &mut V) {
    for v in self {
      v.visit_children(visitor)
    }
  }
}

impl<'i, A: smallvec::Array<Item = U>, U: Visit<'i, T, V>, T, V: Visitor<'i, T>> Visit<'i, T, V> for SmallVec<A> {
  const CHILD_TYPES: VisitTypes = U::CHILD_TYPES;

  fn visit(&mut self, visitor: &mut V) {
    for v in self {
      v.visit(visitor)
    }
  }

  fn visit_children(&mut self, visitor: &mut V) {
    for v in self {
      v.visit_children(visitor)
    }
  }
}

macro_rules! impl_visit {
  ($t: ty) => {
    impl<'i, V: Visitor<'i, T>, T> Visit<'i, T, V> for $t {
      const CHILD_TYPES: VisitTypes = VisitTypes::empty();
      fn visit_children(&mut self, _: &mut V) {}
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
