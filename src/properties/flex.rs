use cssparser::*;
use crate::macros::*;
use crate::values::{
  length::{LengthPercentageOrAuto, LengthPercentage},
  percentage::Percentage
};
use crate::traits::{Parse, ToCss, PropertyHandler, FromStandard};
use super::{Property, PropertyId};
use crate::vendor_prefix::VendorPrefix;
use crate::declaration::DeclarationList;
use super::align::{JustifyContent, ContentDistribution, ContentPosition, AlignItems, SelfPosition, AlignSelf, AlignContent};
use crate::printer::Printer;
use crate::targets::Browsers;
use crate::prefixes::{Feature, is_flex_2009};
use crate::error::{ParserError, PrinterError};
use crate::logical::LogicalProperties;

// https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119/#propdef-flex-direction
enum_property!(FlexDirection,
  ("row", Row),
  ("row-reverse", RowReverse),
  ("column", Column),
  ("column-reverse", ColumnReverse)
);

impl Default for FlexDirection {
  fn default() -> FlexDirection {
    FlexDirection::Row
  }
}

// https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119/#flex-wrap-property
enum_property!(FlexWrap,
  ("nowrap", NoWrap),
  ("wrap", Wrap),
  ("wrap-reverse", WrapReverse)
);

impl Default for FlexWrap {
  fn default() -> FlexWrap {
    FlexWrap::NoWrap
  }
}

impl FromStandard<FlexWrap> for FlexWrap {
  fn from_standard(wrap: &FlexWrap) -> Option<FlexWrap> {
    Some(wrap.clone())
  }
}

/// https://www.w3.org/TR/2018/CR-css-flexbox-1-20181119/#flex-flow-property
#[derive(Debug, Clone, PartialEq)]
pub struct FlexFlow {
  pub direction: FlexDirection,
  pub wrap: FlexWrap
}

impl Parse for FlexFlow {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut direction = None;
    let mut wrap = None;
    loop {
      if direction.is_none() {
        if let Ok(value) = input.try_parse(FlexDirection::parse) {
          direction = Some(value);
          continue
        }
      }
      if wrap.is_none() {
        if let Ok(value) = input.try_parse(FlexWrap::parse) {
          wrap = Some(value);
          continue
        }
      }
      break
    }

    Ok(FlexFlow {
      direction: direction.unwrap_or_default(),
      wrap: wrap.unwrap_or_default()
    })
  }
}

impl ToCss for FlexFlow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let mut needs_space = false;
    if self.direction != FlexDirection::default() || self.wrap == FlexWrap::default() {
      self.direction.to_css(dest)?;
      needs_space = true;
    }

    if self.wrap != FlexWrap::default() {
      if needs_space {
        dest.write_str(" ")?;
      }
      self.wrap.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Flex {
  pub grow: f32,
  pub shrink: f32,
  pub basis: LengthPercentageOrAuto
}

impl Parse for Flex {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(Flex {
        grow: 0.0,
        shrink: 0.0,
        basis: LengthPercentageOrAuto::Auto
      })
    }

    let mut grow = None;
    let mut shrink = None;
    let mut basis = None;

    loop {
      if grow.is_none() {
        if let Ok(val) = input.try_parse(f32::parse) {
          grow = Some(val);
          shrink = input.try_parse(f32::parse).ok();
          continue
        }
      }

      if basis.is_none() {
        if let Ok(val) = input.try_parse(LengthPercentageOrAuto::parse) {
          basis = Some(val);
          continue
        }
      }

      break
    }

    Ok(Flex {
      grow: grow.unwrap_or(1.0),
      shrink: shrink.unwrap_or(1.0),
      basis: basis.unwrap_or(LengthPercentageOrAuto::LengthPercentage(LengthPercentage::Percentage(Percentage(0.0))))
    })
  }
}

impl ToCss for Flex {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    if self.grow == 0.0 && self.shrink == 0.0 && self.basis == LengthPercentageOrAuto::Auto {
      dest.write_str("none")?;
      return Ok(())
    }

    let is_basis_zero = match &self.basis {
      LengthPercentageOrAuto::LengthPercentage(lp) => *lp == 0.0,
      _ => false
    };

    if self.grow != 1.0 || self.shrink != 1.0 || is_basis_zero {
      self.grow.to_css(dest)?;
      if self.shrink != 1.0 {
        dest.write_str(" ")?;
        self.shrink.to_css(dest)?;
      }
    }

    if !is_basis_zero {
      if self.grow != 1.0 || self.shrink != 1.0 {
        dest.write_str(" ")?;
      }
      self.basis.to_css(dest)?;
    }

    Ok(())
  }
}

// Old flex (2009): https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/

enum_property!(BoxOrient,
  ("horizontal", Horizontal),
  ("vertical", Vertical),
  ("inline-axis", InlineAxis),
  ("block-axis", BlockAxis)
);

impl FlexDirection {
  fn to_2009(&self) -> (BoxOrient, BoxDirection) {
    match self {
      FlexDirection::Row => (BoxOrient::Horizontal, BoxDirection::Normal),
      FlexDirection::Column => (BoxOrient::Vertical, BoxDirection::Normal),
      FlexDirection::RowReverse => (BoxOrient::Horizontal, BoxDirection::Reverse),
      FlexDirection::ColumnReverse => (BoxOrient::Vertical, BoxDirection::Reverse)
    }
  }
}

enum_property!(BoxDirection,
  Normal,
  Reverse
);

enum_property!(BoxAlign,
  Start,
  End,
  Center,
  Baseline,
  Stretch
);

impl FromStandard<AlignItems> for BoxAlign {
  fn from_standard(align: &AlignItems) -> Option<BoxAlign> {
    match align {
      AlignItems::SelfPosition(None, sp) => {
        match sp {
          SelfPosition::Start | SelfPosition::FlexStart => Some(BoxAlign::Start),
          SelfPosition::End | SelfPosition::FlexEnd => Some(BoxAlign::End),
          SelfPosition::Center => Some(BoxAlign::Center),
          _ => None
        }
      }
      AlignItems::Stretch => Some(BoxAlign::Stretch),
      _ => None
    }
  }
}

enum_property!(BoxPack,
  Start,
  End,
  Center,
  Justify
);

impl FromStandard<JustifyContent> for BoxPack {
  fn from_standard(justify: &JustifyContent) -> Option<BoxPack> {
    match justify {
      JustifyContent::ContentDistribution(cd) => {
        match cd {
          ContentDistribution::SpaceBetween => Some(BoxPack::Justify),
          _ => None
        }
      }
      JustifyContent::ContentPosition(None, pos) => {
        match pos {
          ContentPosition::Start | ContentPosition::FlexStart => Some(BoxPack::Start),
          ContentPosition::End | ContentPosition::FlexEnd => Some(BoxPack::End),
          ContentPosition::Center => Some(BoxPack::Center)
        }
      }
      _ => None
    }
  }
}

enum_property!(BoxLines,
  Single,
  Multiple
);

impl FromStandard<FlexWrap> for BoxLines {
  fn from_standard(wrap: &FlexWrap) -> Option<BoxLines> {
    match wrap {
      FlexWrap::NoWrap => Some(BoxLines::Single),
      FlexWrap::Wrap => Some(BoxLines::Multiple),
      _ => None
    }
  }
}

type BoxOrdinalGroup = f32;
impl FromStandard<f32> for BoxOrdinalGroup {
  fn from_standard(order: &f32) -> Option<BoxOrdinalGroup> {
    Some(*order)
  }
}

// Old flex (2012): https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/

enum_property!(FlexPack,
  Start,
  End,
  Center,
  Justify,
  Distribute
);

impl FromStandard<JustifyContent> for FlexPack {
  fn from_standard(justify: &JustifyContent) -> Option<FlexPack> {
    match justify {
      JustifyContent::ContentDistribution(cd) => {
        match cd {
          ContentDistribution::SpaceBetween => Some(FlexPack::Justify),
          ContentDistribution::SpaceAround => Some(FlexPack::Distribute),
          _ => None
        }
      }
      JustifyContent::ContentPosition(None, pos) => {
        match pos {
          ContentPosition::Start | ContentPosition::FlexStart => Some(FlexPack::Start),
          ContentPosition::End | ContentPosition::FlexEnd => Some(FlexPack::End),
          ContentPosition::Center => Some(FlexPack::Center)
        }
      }
      _ => None
    }
  }
}

pub type FlexAlign = BoxAlign;

enum_property!(FlexItemAlign,
  Auto,
  Start,
  End,
  Center,
  Baseline,
  Stretch
);

impl FromStandard<AlignSelf> for FlexItemAlign {
  fn from_standard(justify: &AlignSelf) -> Option<FlexItemAlign> {
    match justify {
      AlignSelf::Auto => Some(FlexItemAlign::Auto),
      AlignSelf::Stretch => Some(FlexItemAlign::Stretch),
      AlignSelf::SelfPosition(None, pos) => {
        match pos {
          SelfPosition::Start | SelfPosition::FlexStart => Some(FlexItemAlign::Start),
          SelfPosition::End | SelfPosition::FlexEnd => Some(FlexItemAlign::End),
          SelfPosition::Center => Some(FlexItemAlign::Center),
          _ => None
        }
      }
      _ => None
    }
  }
}

enum_property!(FlexLinePack,
  Start,
  End,
  Center,
  Justify,
  Distribute,
  Stretch
);

impl FromStandard<AlignContent> for FlexLinePack {
  fn from_standard(justify: &AlignContent) -> Option<FlexLinePack> {
    match justify {
      AlignContent::ContentDistribution(cd) => {
        match cd {
          ContentDistribution::SpaceBetween => Some(FlexLinePack::Justify),
          ContentDistribution::SpaceAround => Some(FlexLinePack::Distribute),
          ContentDistribution::Stretch => Some(FlexLinePack::Stretch),
          _ => None
        }
      }
      AlignContent::ContentPosition(None, pos) => {
        match pos {
          ContentPosition::Start | ContentPosition::FlexStart => Some(FlexLinePack::Start),
          ContentPosition::End | ContentPosition::FlexEnd => Some(FlexLinePack::End),
          ContentPosition::Center => Some(FlexLinePack::Center)
        }
      }
      _ => None
    }
  }
}

#[derive(Default, Debug)]
pub(crate) struct FlexHandler {
  targets: Option<Browsers>,
  direction: Option<(FlexDirection, VendorPrefix)>,
  box_orient: Option<(BoxOrient, VendorPrefix)>,
  box_direction: Option<(BoxDirection, VendorPrefix)>,
  wrap: Option<(FlexWrap, VendorPrefix)>,
  box_lines: Option<(BoxLines, VendorPrefix)>,
  grow: Option<(f32, VendorPrefix)>,
  box_flex: Option<(f32, VendorPrefix)>,
  flex_positive: Option<(f32, VendorPrefix)>,
  shrink: Option<(f32, VendorPrefix)>,
  flex_negative: Option<(f32, VendorPrefix)>,
  basis: Option<(LengthPercentageOrAuto, VendorPrefix)>,
  preferred_size: Option<(LengthPercentageOrAuto, VendorPrefix)>,
  order: Option<(f32, VendorPrefix)>,
  box_ordinal_group: Option<(BoxOrdinalGroup, VendorPrefix)>,
  flex_order: Option<(f32, VendorPrefix)>,
  has_any: bool
}

impl FlexHandler {
  pub fn new(targets: Option<Browsers>) -> FlexHandler {
    FlexHandler {
      targets,
      ..FlexHandler::default()
    }
  }
}

impl PropertyHandler for FlexHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList, _: &mut LogicalProperties) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: ident) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest);
          }
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: ident) => {{
        maybe_flush!($prop, $val, $vp);

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
        } else {
          self.$prop = Some(($val.clone(), *$vp));
          self.has_any = true;
        }
      }};
    }

    match property {
      FlexDirection(val, vp) => {
        if self.targets.is_some() {
          self.box_direction = None;
          self.box_orient = None;
        }
        property!(direction, val, vp);
      },
      BoxOrient(val, vp) => property!(box_orient, val, vp),
      BoxDirection(val, vp) => property!(box_direction, val, vp),
      FlexWrap(val, vp) => {
        if self.targets.is_some() {
          self.box_lines = None;
        }
        property!(wrap, val, vp);
      },
      BoxLines(val, vp) => property!(box_lines, val, vp),
      FlexFlow(val, vp) => {
        if self.targets.is_some() {
          self.box_direction = None;
          self.box_orient = None;
        }
        property!(direction, &val.direction, vp);
        property!(wrap, &val.wrap, vp);
      }
      FlexGrow(val, vp) => {
        if self.targets.is_some() {
          self.box_flex = None;
          self.flex_positive = None;
        }
        property!(grow, val, vp);
      },
      BoxFlex(val, vp) => property!(box_flex, val, vp),
      FlexPositive(val, vp) => property!(flex_positive, val, vp),
      FlexShrink(val, vp) => {
        if self.targets.is_some() {
          self.flex_negative = None;
        }
        property!(shrink, val, vp);
      },
      FlexNegative(val, vp) => property!(flex_negative, val, vp),
      FlexBasis(val, vp) => {
        if self.targets.is_some() {
          self.preferred_size = None;
        }
        property!(basis, val, vp);
      }
      FlexPreferredSize(val, vp) => property!(preferred_size, val, vp),
      Flex(val, vp) => {
        if self.targets.is_some() {
          self.box_flex = None;
          self.flex_positive = None;
          self.flex_negative = None;
          self.preferred_size = None;
        }
        maybe_flush!(grow, &val.grow, vp);
        maybe_flush!(shrink, &val.shrink, vp);
        maybe_flush!(basis, &val.basis, vp);
        property!(grow, &val.grow, vp);
        property!(shrink, &val.shrink, vp);
        property!(basis, &val.basis, vp);
      }
      Order(val, vp) => {
        if self.targets.is_some() {
          self.box_ordinal_group = None;
          self.flex_order = None;
        }
        property!(order, val, vp);
      }
      BoxOrdinalGroup(val, vp) => property!(box_ordinal_group, val, vp),
      FlexOrder(val, vp) => property!(flex_order, val, vp),
      Unparsed(val) if is_flex_property(&val.property_id) => {
        self.flush(dest);
        dest.push(property.clone()) // TODO: prefix?
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList, _: &mut LogicalProperties) {
    self.flush(dest);
  }
}


impl FlexHandler {
  fn flush(&mut self, dest: &mut DeclarationList) {
    if !self.has_any {
      return
    }

    self.has_any = false;

    let mut direction = std::mem::take(&mut self.direction);
    let mut wrap = std::mem::take(&mut self.wrap);
    let mut grow = std::mem::take(&mut self.grow);
    let mut shrink = std::mem::take(&mut self.shrink);
    let mut basis = std::mem::take(&mut self.basis);
    let box_orient = std::mem::take(&mut self.box_orient);
    let box_direction = std::mem::take(&mut self.box_direction);
    let box_flex = std::mem::take(&mut self.box_flex);
    let box_ordinal_group = std::mem::take(&mut self.box_ordinal_group);
    let box_lines = std::mem::take(&mut self.box_lines);
    let flex_positive = std::mem::take(&mut self.flex_positive);
    let flex_negative = std::mem::take(&mut self.flex_negative);
    let preferred_size = std::mem::take(&mut self.preferred_size);
    let order = std::mem::take(&mut self.order);
    let flex_order = std::mem::take(&mut self.flex_order);

    macro_rules! single_property {
      ($prop: ident, $key: ident $(, 2012: $prop_2012: ident )? $(, 2009: $prop_2009: ident )?) => {
        if let Some((val, prefix)) = $key {
          if !prefix.is_empty() {
            let mut prefix = prefix;
            if prefix.contains(VendorPrefix::None) {
              if let Some(targets) = self.targets {
                prefix = Feature::$prop.prefixes_for(targets);

                // 2009 spec, implemented by webkit and firefox.
                $(
                  let mut prefixes_2009 = VendorPrefix::empty();
                  if is_flex_2009(targets) {
                    prefixes_2009 |= VendorPrefix::WebKit;
                  }
                  if prefix.contains(VendorPrefix::Moz) {
                    prefixes_2009 |= VendorPrefix::Moz;
                  }
                  if !prefixes_2009.is_empty() {
                    if let Some(v) = $prop_2009::from_standard(&val) {
                      dest.push(Property::$prop_2009(v, prefixes_2009));
                    }
                  }
                )?
                
                $(
                  let mut ms = true;
                  if prefix.contains(VendorPrefix::Ms) {
                    dest.push(Property::$prop_2012(val.clone(), VendorPrefix::Ms));
                    ms = false;
                  }
                  if !ms {
                    prefix.remove(VendorPrefix::Ms);
                  }
                )?

                // Firefox only implemented the 2009 spec prefixed.
                prefix.remove(VendorPrefix::Moz);
              }
            }
            dest.push(Property::$prop(val, prefix))
          }
        }
      };
    }

    macro_rules! legacy_property {
      ($prop: ident, $key: expr) => {
        if let Some((val, prefix)) = $key {
          if !prefix.is_empty() {
            dest.push(Property::$prop(val, prefix))
          }
        }
      };
    }

    // Legacy properties. These are only set if the final standard properties were unset.
    legacy_property!(BoxOrient, box_orient);
    legacy_property!(BoxDirection, box_direction);
    legacy_property!(BoxOrdinalGroup, box_ordinal_group);
    legacy_property!(BoxFlex, box_flex);
    legacy_property!(BoxLines, box_lines);
    legacy_property!(FlexPositive, flex_positive);
    legacy_property!(FlexNegative, flex_negative);
    legacy_property!(FlexPreferredSize, preferred_size.clone());
    legacy_property!(FlexOrder, flex_order.clone());

    if let Some((direction, _)) = direction {
      if let Some(targets) = self.targets {
        let prefixes = Feature::FlexDirection.prefixes_for(targets);
        let mut prefixes_2009 = VendorPrefix::empty();
        if is_flex_2009(targets) {
          prefixes_2009 |= VendorPrefix::WebKit;
        }
        if prefixes.contains(VendorPrefix::Moz) {
          prefixes_2009 |= VendorPrefix::Moz;
        }
        if !prefixes_2009.is_empty() {
          let (orient, dir) = direction.to_2009();
          dest.push(Property::BoxOrient(orient, prefixes_2009));
          dest.push(Property::BoxDirection(dir, prefixes_2009));
        }
      }
    }
    
    if let (Some((direction, dir_prefix)), Some((wrap, wrap_prefix))) = (&mut direction, &mut wrap) {      
      let intersection = *dir_prefix & *wrap_prefix;
      if !intersection.is_empty() {
        let mut prefix = intersection;
        if prefix.contains(VendorPrefix::None) {
          if let Some(targets) = self.targets {
            prefix = Feature::FlexFlow.prefixes_for(targets);
            // Firefox only implemented the 2009 spec prefixed.
            prefix.remove(VendorPrefix::Moz);
          }
        }
        dest.push(Property::FlexFlow(FlexFlow {
          direction: *direction,
          wrap: *wrap
        }, prefix));
        dir_prefix.remove(intersection);
        wrap_prefix.remove(intersection);
      }
    }

    single_property!(FlexDirection, direction);
    single_property!(FlexWrap, wrap, 2009: BoxLines);

    if let Some(targets) = self.targets {
      if let Some((grow, _)) = grow {
        let prefixes = Feature::FlexGrow.prefixes_for(targets);
        let mut prefixes_2009 = VendorPrefix::empty();
        if is_flex_2009(targets) {
          prefixes_2009 |= VendorPrefix::WebKit;
        }
        if prefixes.contains(VendorPrefix::Moz) {
          prefixes_2009 |= VendorPrefix::Moz;
        }
        if !prefixes_2009.is_empty() {
          dest.push(Property::BoxFlex(grow, prefixes_2009));
        }
      }
    }

    if let (Some((grow, grow_prefix)), Some((shrink, shrink_prefix)), Some((basis, basis_prefix))) = (&mut grow, &mut shrink, &mut basis) {
      let intersection = *grow_prefix & *shrink_prefix & *basis_prefix;
      if !intersection.is_empty() {
        let mut prefix = intersection;
        if prefix.contains(VendorPrefix::None) {
          if let Some(targets) = self.targets {
            prefix = Feature::Flex.prefixes_for(targets);
            // Firefox only implemented the 2009 spec prefixed.
            prefix.remove(VendorPrefix::Moz);
          }
        }
        dest.push(Property::Flex(Flex {
          grow: *grow,
          shrink: *shrink,
          basis: basis.clone()
        }, prefix));
        grow_prefix.remove(intersection);
        shrink_prefix.remove(intersection);
        basis_prefix.remove(intersection);
      }
    }

    single_property!(FlexGrow, grow, 2012: FlexPositive);
    single_property!(FlexShrink, shrink, 2012: FlexNegative);
    single_property!(FlexBasis, basis, 2012: FlexPreferredSize);
    single_property!(Order, order, 2012: FlexOrder, 2009: BoxOrdinalGroup);
  }
}

#[inline]
fn is_flex_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::FlexDirection(_) |
    PropertyId::BoxOrient(_) |
    PropertyId::BoxDirection(_) |
    PropertyId::FlexWrap(_) |
    PropertyId::BoxLines(_) |
    PropertyId::FlexFlow(_) |
    PropertyId::FlexGrow(_) |
    PropertyId::BoxFlex(_) |
    PropertyId::FlexPositive(_) |
    PropertyId::FlexShrink(_) |
    PropertyId::FlexNegative(_) |
    PropertyId::FlexBasis(_) |
    PropertyId::FlexPreferredSize(_) |
    PropertyId::Flex(_) |
    PropertyId::Order(_) |
    PropertyId::BoxOrdinalGroup(_) |
    PropertyId::FlexOrder(_) => true,
    _ => false
  }
}
