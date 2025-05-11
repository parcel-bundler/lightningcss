//! The CSS border radius property.

use crate::compat;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::logical::PropertyCategory;
use crate::macros::define_shorthand;
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::traits::{IsCompatible, Parse, PropertyHandler, Shorthand, ToCss, Zero};
use crate::values::length::*;
use crate::values::rect::Rect;
use crate::values::size::Size2D;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

define_shorthand! {
  /// A value for the [border-radius](https://www.w3.org/TR/css-backgrounds-3/#border-radius) property.
  pub struct BorderRadius(VendorPrefix) {
    /// The x and y radius values for the top left corner.
    top_left: BorderTopLeftRadius(Size2D<LengthPercentage>, VendorPrefix),
    /// The x and y radius values for the top right corner.
    top_right: BorderTopRightRadius(Size2D<LengthPercentage>, VendorPrefix),
    /// The x and y radius values for the bottom right corner.
    bottom_right: BorderBottomRightRadius(Size2D<LengthPercentage>, VendorPrefix),
    /// The x and y radius values for the bottom left corner.
    bottom_left: BorderBottomLeftRadius(Size2D<LengthPercentage>, VendorPrefix),
  }
}

impl Default for BorderRadius {
  fn default() -> BorderRadius {
    let zero = Size2D(LengthPercentage::zero(), LengthPercentage::zero());
    BorderRadius {
      top_left: zero.clone(),
      top_right: zero.clone(),
      bottom_right: zero.clone(),
      bottom_left: zero,
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
      bottom_right: Size2D(widths.2, heights.2),
      bottom_left: Size2D(widths.3, heights.3),
    })
  }
}

impl ToCss for BorderRadius {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let widths = Rect::new(
      &self.top_left.0,
      &self.top_right.0,
      &self.bottom_right.0,
      &self.bottom_left.0,
    );
    let heights = Rect::new(
      &self.top_left.1,
      &self.top_right.1,
      &self.bottom_right.1,
      &self.bottom_left.1,
    );

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
  top_left: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  top_right: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  bottom_right: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  bottom_left: Option<(Size2D<LengthPercentage>, VendorPrefix)>,
  start_start: Option<Property<'i>>,
  start_end: Option<Property<'i>>,
  end_end: Option<Property<'i>>,
  end_start: Option<Property<'i>>,
  category: PropertyCategory,
  has_any: bool,
}

impl<'i> PropertyHandler<'i> for BorderRadiusHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: expr) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest, context);
          }
        }

        if self.$prop.is_some() && matches!(context.targets.browsers, Some(targets) if !$val.is_compatible(targets)) {
          self.flush(dest, context);
        }
      }};
    }

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: ident) => {{
        if self.category != PropertyCategory::Physical {
          self.flush(dest, context);
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
          self.flush(dest, context);
        }

        self.$prop = Some(property.clone());
        self.category = PropertyCategory::Logical;
        self.has_any = true;
      }};
    }

    match property {
      BorderTopLeftRadius(val, vp) => property!(top_left, val, vp),
      BorderTopRightRadius(val, vp) => property!(top_right, val, vp),
      BorderBottomRightRadius(val, vp) => property!(bottom_right, val, vp),
      BorderBottomLeftRadius(val, vp) => property!(bottom_left, val, vp),
      BorderStartStartRadius(_) => logical_property!(start_start),
      BorderStartEndRadius(_) => logical_property!(start_end),
      BorderEndEndRadius(_) => logical_property!(end_end),
      BorderEndStartRadius(_) => logical_property!(end_start),
      BorderRadius(val, vp) => {
        self.start_start = None;
        self.start_end = None;
        self.end_end = None;
        self.end_start = None;
        maybe_flush!(top_left, &val.top_left, vp);
        maybe_flush!(top_right, &val.top_right, vp);
        maybe_flush!(bottom_right, &val.bottom_right, vp);
        maybe_flush!(bottom_left, &val.bottom_left, vp);
        property!(top_left, &val.top_left, vp);
        property!(top_right, &val.top_right, vp);
        property!(bottom_right, &val.bottom_right, vp);
        property!(bottom_left, &val.bottom_left, vp);
      }
      Unparsed(val) if is_border_radius_property(&val.property_id) => {
        // Even if we weren't able to parse the value (e.g. due to var() references),
        // we can still add vendor prefixes to the property itself.
        match &val.property_id {
          PropertyId::BorderStartStartRadius => logical_property!(start_start),
          PropertyId::BorderStartEndRadius => logical_property!(start_end),
          PropertyId::BorderEndEndRadius => logical_property!(end_end),
          PropertyId::BorderEndStartRadius => logical_property!(end_start),
          _ => {
            self.flush(dest, context);
            dest.push(Property::Unparsed(
              val.get_prefixed(context.targets, Feature::BorderRadius),
            ));
          }
        }
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
  }
}

impl<'i> BorderRadiusHandler<'i> {
  fn flush(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    let mut top_left = std::mem::take(&mut self.top_left);
    let mut top_right = std::mem::take(&mut self.top_right);
    let mut bottom_right = std::mem::take(&mut self.bottom_right);
    let mut bottom_left = std::mem::take(&mut self.bottom_left);
    let start_start = std::mem::take(&mut self.start_start);
    let start_end = std::mem::take(&mut self.start_end);
    let end_end = std::mem::take(&mut self.end_end);
    let end_start = std::mem::take(&mut self.end_start);

    if let (
      Some((top_left, tl_prefix)),
      Some((top_right, tr_prefix)),
      Some((bottom_right, br_prefix)),
      Some((bottom_left, bl_prefix)),
    ) = (&mut top_left, &mut top_right, &mut bottom_right, &mut bottom_left)
    {
      let intersection = *tl_prefix & *tr_prefix & *br_prefix & *bl_prefix;
      if !intersection.is_empty() {
        let prefix = context.targets.prefixes(intersection, Feature::BorderRadius);
        dest.push(Property::BorderRadius(
          BorderRadius {
            top_left: top_left.clone(),
            top_right: top_right.clone(),
            bottom_right: bottom_right.clone(),
            bottom_left: bottom_left.clone(),
          },
          prefix,
        ));
        tl_prefix.remove(intersection);
        tr_prefix.remove(intersection);
        br_prefix.remove(intersection);
        bl_prefix.remove(intersection);
      }
    }

    macro_rules! single_property {
      ($prop: ident, $key: ident) => {
        if let Some((val, mut vp)) = $key {
          if !vp.is_empty() {
            vp = context.targets.prefixes(vp, Feature::$prop);
            dest.push(Property::$prop(val, vp))
          }
        }
      };
    }

    let logical_supported = !context.should_compile_logical(compat::Feature::LogicalBorderRadius);

    macro_rules! logical_property {
      ($prop: ident, $key: ident, $ltr: ident, $rtl: ident) => {
        if let Some(val) = $key {
          if logical_supported {
            dest.push(val);
          } else {
            let vp = context.targets.prefixes(VendorPrefix::None, Feature::$ltr);
            match val {
              Property::BorderStartStartRadius(val)
              | Property::BorderStartEndRadius(val)
              | Property::BorderEndEndRadius(val)
              | Property::BorderEndStartRadius(val) => {
                context.add_logical_rule(Property::$ltr(val.clone(), vp), Property::$rtl(val, vp));
              }
              Property::Unparsed(val) => {
                context.add_logical_rule(
                  Property::Unparsed(val.with_property_id(PropertyId::$ltr(vp))),
                  Property::Unparsed(val.with_property_id(PropertyId::$rtl(vp))),
                );
              }
              _ => {}
            }
          }
        }
      };
    }

    single_property!(BorderTopLeftRadius, top_left);
    single_property!(BorderTopRightRadius, top_right);
    single_property!(BorderBottomRightRadius, bottom_right);
    single_property!(BorderBottomLeftRadius, bottom_left);
    logical_property!(
      BorderStartStartRadius,
      start_start,
      BorderTopLeftRadius,
      BorderTopRightRadius
    );
    logical_property!(
      BorderStartEndRadius,
      start_end,
      BorderTopRightRadius,
      BorderTopLeftRadius
    );
    logical_property!(
      BorderEndEndRadius,
      end_end,
      BorderBottomRightRadius,
      BorderBottomLeftRadius
    );
    logical_property!(
      BorderEndStartRadius,
      end_start,
      BorderBottomLeftRadius,
      BorderBottomRightRadius
    );
  }
}

#[inline]
fn is_border_radius_property(property_id: &PropertyId) -> bool {
  if is_logical_border_radius_property(property_id) {
    return true;
  }

  match property_id {
    PropertyId::BorderTopLeftRadius(_)
    | PropertyId::BorderTopRightRadius(_)
    | PropertyId::BorderBottomRightRadius(_)
    | PropertyId::BorderBottomLeftRadius(_)
    | PropertyId::BorderRadius(_) => true,
    _ => false,
  }
}

#[inline]
fn is_logical_border_radius_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BorderStartStartRadius
    | PropertyId::BorderStartEndRadius
    | PropertyId::BorderEndEndRadius
    | PropertyId::BorderEndStartRadius => true,
    _ => false,
  }
}
