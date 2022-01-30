use crate::values::length::*;
use crate::values::size::Size2D;
use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::targets::Browsers;
use crate::prefixes::Feature;
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::declaration::DeclarationList;
use crate::values::rect::Rect;
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};
use crate::logical::{LogicalProperties, PropertyCategory};
use crate::compat;

#[derive(Debug, Clone, PartialEq)]
pub struct BorderRadius {
  pub top_left: Size2D<LengthPercentage>,
  pub top_right: Size2D<LengthPercentage>,
  pub bottom_left: Size2D<LengthPercentage>,
  pub bottom_right: Size2D<LengthPercentage>
}

impl Default for BorderRadius {
  fn default() -> BorderRadius {
    let zero = Size2D(LengthPercentage::zero(), LengthPercentage::zero());
    BorderRadius {
      top_left: zero.clone(),
      top_right: zero.clone(),
      bottom_left: zero.clone(),
      bottom_right: zero
    }
  }
}

impl<'i> Parse<'i> for BorderRadius {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let widths: Rect<LengthPercentage> = Rect::parse(input)?;
    let heights = if input.try_parse(|input| input.expect_delim('/')).is_ok() {
      Rect::parse(input)?
    } else {
      widths.clone()
    };

    Ok(BorderRadius {
      top_left: Size2D(widths.0, heights.0),
      top_right: Size2D(widths.1, heights.1),
      bottom_left: Size2D(widths.2, heights.2),
      bottom_right: Size2D(widths.3, heights.3)
    })
  }
}

impl ToCss for BorderRadius {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let widths = Rect::new(&self.top_left.0, &self.top_right.0, &self.bottom_left.0, &self.bottom_right.0);
    let heights = Rect::new(&self.top_left.1, &self.top_right.1, &self.bottom_left.1, &self.bottom_right.1);

    widths.to_css(dest)?;
    if widths != heights {
      dest.delim('/', true)?;
      heights.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Default, Debug)]
pub(crate) struct BorderRadiusHandler<'i> {
  targets: Option<Browsers>,
  top_left: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  top_right: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  bottom_left: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  bottom_right: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  start_start: Option<Property<'i>>,
  start_end: Option<Property<'i>>,
  end_start: Option<Property<'i>>,
  end_end: Option<Property<'i>>,
  category: PropertyCategory,
  has_any: bool
}

impl<'i> BorderRadiusHandler<'i> {
  pub fn new(targets: Option<Browsers>) -> Self {
    BorderRadiusHandler {
      targets,
      ..BorderRadiusHandler::default()
    }
  }
}

impl<'i> PropertyHandler<'i> for BorderRadiusHandler<'i> {
  fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, logical: &mut LogicalProperties) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest, logical);
          }
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: ident) => {{
        if self.category != PropertyCategory::Physical {
          self.flush(dest, logical);
        }

        maybe_flush!($prop, $val, $vp);

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
        } else {
          self.$prop = Some(($val.clone(), *$vp));
          self.has_any = true;
        }

        self.category = PropertyCategory::Physical;
      }};
    }

    macro_rules! logical_property {
      ($prop: ident) => {{
        if self.category != PropertyCategory::Logical {
          self.flush(dest, logical);
        }

        self.$prop = Some(property.clone());
        self.category = PropertyCategory::Logical;
        self.has_any = true;
      }};
    }

    match property {
      BorderTopLeftRadius(val, vp) => property!(top_left, val, vp),
      BorderTopRightRadius(val, vp) => property!(top_right, val, vp),
      BorderBottomLeftRadius(val, vp) => property!(bottom_left, val, vp),
      BorderBottomRightRadius(val, vp) => property!(bottom_right, val, vp),
      BorderStartStartRadius(_) => logical_property!(start_start),
      BorderStartEndRadius(_) => logical_property!(start_end),
      BorderEndStartRadius(_) => logical_property!(end_start),
      BorderEndEndRadius(_) => logical_property!(end_end),
      BorderRadius(val, vp) => {
        self.start_start = None;
        self.start_end = None;
        self.end_start = None;
        self.end_end = None;
        maybe_flush!(top_left, &val.top_left, vp);
        maybe_flush!(top_right, &val.top_right, vp);
        maybe_flush!(bottom_left, &val.bottom_left, vp);
        maybe_flush!(bottom_right, &val.bottom_right, vp);
        property!(top_left, &val.top_left, vp);
        property!(top_right, &val.top_right, vp);
        property!(bottom_left, &val.bottom_left, vp);
        property!(bottom_right, &val.bottom_right, vp);
      }
      Unparsed(val) if is_border_radius_property(&val.property_id) => {
        // Even if we weren't able to parse the value (e.g. due to var() references),
        // we can still add vendor prefixes to the property itself.
        match &val.property_id {
          PropertyId::BorderStartStartRadius => logical_property!(start_start),
          PropertyId::BorderStartEndRadius => logical_property!(start_end),
          PropertyId::BorderEndStartRadius => logical_property!(end_start),
          PropertyId::BorderEndEndRadius => logical_property!(end_end),
          _ => {
            self.flush(dest, logical);
            dest.push(Property::Unparsed(val.get_prefixed(self.targets, Feature::BorderRadius)));
          }
        }
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, logical: &mut LogicalProperties) {
    self.flush(dest, logical);
  }
}

impl<'i> BorderRadiusHandler<'i> {
  fn flush(&mut self, dest: &mut DeclarationList<'i>, logical: &mut LogicalProperties) {
    if !self.has_any {
      return
    }

    self.has_any = false;

    let mut top_left = std::mem::take(&mut self.top_left);
    let mut top_right = std::mem::take(&mut self.top_right);
    let mut bottom_left = std::mem::take(&mut self.bottom_left);
    let mut bottom_right = std::mem::take(&mut self.bottom_right);
    let start_start = std::mem::take(&mut self.start_start);
    let start_end = std::mem::take(&mut self.start_end);
    let end_start = std::mem::take(&mut self.end_start);
    let end_end = std::mem::take(&mut self.end_end);

    if let (Some((top_left, tl_prefix)), Some((top_right, tr_prefix)), Some((bottom_left, bl_prefix)), Some((bottom_right, br_prefix))) = (&mut top_left, &mut top_right, &mut bottom_left, &mut bottom_right) {
      let intersection = *tl_prefix & *tr_prefix & *bl_prefix & *br_prefix;
      if !intersection.is_empty() {
        let mut prefix = intersection;
        if prefix.contains(VendorPrefix::None) {
          if let Some(targets) = self.targets {
            prefix = Feature::BorderRadius.prefixes_for(targets)
          }
        }
        dest.push(Property::BorderRadius(BorderRadius {
          top_left: top_left.clone(),
          top_right: top_right.clone(),
          bottom_left: bottom_left.clone(),
          bottom_right: bottom_right.clone(),
        }, prefix));
        tl_prefix.remove(intersection);
        tr_prefix.remove(intersection);
        bl_prefix.remove(intersection);
        br_prefix.remove(intersection);
      }
    }

    macro_rules! single_property {
      ($prop: ident, $key: ident) => {
        if let Some((val, mut vp)) = $key {
          if !vp.is_empty() {
            if vp.contains(VendorPrefix::None) {
              if let Some(targets) = self.targets {
                vp = Feature::$prop.prefixes_for(targets)
              }
            }
            dest.push(Property::$prop(val, vp))
          }
        }
      };
    }

    let logical_supported = logical.is_supported(compat::Feature::LogicalBorderRadius);

    macro_rules! logical_property {
      ($prop: ident, $opposite_prop: ident, $key: ident, $opposite_key: ident, $ltr: ident, $rtl: ident) => {
        if logical_supported {
          if let Some(val) = $key {
            dest.push(val);
          }
          if let Some(val) = $opposite_key {
            dest.push(val);
          }
        } else if $key.is_some() || $opposite_key.is_some() {
          let vp = if let Some(targets) = self.targets {
            Feature::$ltr.prefixes_for(targets)
          } else {
            VendorPrefix::None
          };

          logical.add_inline(
            dest,
            PropertyId::$ltr(vp),
            PropertyId::$rtl(vp),
            $key,
            $opposite_key
          );
        }
      };
    }

    single_property!(BorderTopLeftRadius, top_left);
    single_property!(BorderTopRightRadius, top_right);
    single_property!(BorderBottomLeftRadius, bottom_left);
    single_property!(BorderBottomRightRadius, bottom_right);
    logical_property!(BorderStartStartRadius, BorderStartEndRadius, start_start, start_end, BorderTopLeftRadius, BorderTopRightRadius);
    logical_property!(BorderEndStartRadius, BorderEndEndRadius, end_start, end_end, BorderBottomLeftRadius, BorderBottomRightRadius);
  }
}

#[inline]
fn is_border_radius_property(property_id: &PropertyId) -> bool {
  if is_logical_border_radius_property(property_id) {
    return true
  }

  match property_id {
    PropertyId::BorderTopLeftRadius(_) |
    PropertyId::BorderTopRightRadius(_) |
    PropertyId::BorderBottomLeftRadius(_) |
    PropertyId::BorderBottomRightRadius(_) |
    PropertyId::BorderRadius(_) => true,
    _ => false
  }
}

#[inline]
fn is_logical_border_radius_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BorderStartStartRadius |
    PropertyId::BorderStartEndRadius |
    PropertyId::BorderEndStartRadius | 
    PropertyId::BorderEndEndRadius => true,
    _ => false
  }
}
