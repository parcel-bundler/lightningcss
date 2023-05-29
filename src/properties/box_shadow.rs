//! The CSS box-shadow property.

use super::PropertyId;
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::properties::Property;
use crate::targets::Browsers;
use crate::traits::{IsCompatible, Parse, PropertyHandler, ToCss, Zero};
use crate::values::color::{ColorFallbackKind, CssColor};
use crate::values::length::Length;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;

/// A value for the [box-shadow](https://drafts.csswg.org/css-backgrounds/#box-shadow) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct BoxShadow {
  /// The color of the box shadow.
  pub color: CssColor,
  /// The x offset of the shadow.
  pub x_offset: Length,
  /// The y offset of the shadow.
  pub y_offset: Length,
  /// The blur radius of the shadow.
  pub blur: Length,
  /// The spread distance of the shadow.
  pub spread: Length,
  /// Whether the shadow is inset within the box.
  pub inset: bool,
}

impl<'i> Parse<'i> for BoxShadow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color = None;
    let mut lengths = None;
    let mut inset = false;

    loop {
      if !inset {
        if input.try_parse(|input| input.expect_ident_matching("inset")).is_ok() {
          inset = true;
          continue;
        }
      }

      if lengths.is_none() {
        let value = input.try_parse::<_, _, ParseError<ParserError<'i>>>(|input| {
          let horizontal = Length::parse(input)?;
          let vertical = Length::parse(input)?;
          let blur = input.try_parse(Length::parse).unwrap_or(Length::zero());
          let spread = input.try_parse(Length::parse).unwrap_or(Length::zero());
          Ok((horizontal, vertical, blur, spread))
        });

        if let Ok(value) = value {
          lengths = Some(value);
          continue;
        }
      }

      if color.is_none() {
        if let Ok(value) = input.try_parse(CssColor::parse) {
          color = Some(value);
          continue;
        }
      }

      break;
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(BoxShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2,
      spread: lengths.3,
      inset,
    })
  }
}

impl ToCss for BoxShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.inset {
      dest.write_str("inset ")?;
    }

    self.x_offset.to_css(dest)?;
    dest.write_char(' ')?;
    self.y_offset.to_css(dest)?;

    if self.blur != Length::zero() || self.spread != Length::zero() {
      dest.write_char(' ')?;
      self.blur.to_css(dest)?;

      if self.spread != Length::zero() {
        dest.write_char(' ')?;
        self.spread.to_css(dest)?;
      }
    }

    if self.color != CssColor::current_color() {
      dest.write_char(' ')?;
      self.color.to_css(dest)?;
    }

    Ok(())
  }
}

impl IsCompatible for BoxShadow {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.color.is_compatible(browsers)
      && self.x_offset.is_compatible(browsers)
      && self.y_offset.is_compatible(browsers)
      && self.blur.is_compatible(browsers)
      && self.spread.is_compatible(browsers)
  }
}

#[derive(Default)]
pub(crate) struct BoxShadowHandler {
  box_shadows: Option<(SmallVec<[BoxShadow; 1]>, VendorPrefix)>,
  flushed: bool,
}

impl<'i> PropertyHandler<'i> for BoxShadowHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    match property {
      Property::BoxShadow(box_shadows, prefix) => {
        if self.box_shadows.is_some()
          && matches!(context.targets.browsers, Some(browsers) if !box_shadows.is_compatible(browsers))
        {
          self.flush(dest, context);
        }

        if let Some((val, prefixes)) = &mut self.box_shadows {
          if val != box_shadows && !prefixes.contains(*prefix) {
            self.flush(dest, context);
            self.box_shadows = Some((box_shadows.clone(), *prefix));
          } else {
            *val = box_shadows.clone();
            *prefixes |= *prefix;
          }
        } else {
          self.box_shadows = Some((box_shadows.clone(), *prefix));
        }
      }
      Property::Unparsed(unparsed) if matches!(unparsed.property_id, PropertyId::BoxShadow(_)) => {
        self.flush(dest, context);

        let mut unparsed = unparsed.clone();
        context.add_unparsed_fallbacks(&mut unparsed);
        dest.push(Property::Unparsed(unparsed));
        self.flushed = true;
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
    self.flushed = false;
  }
}

impl BoxShadowHandler {
  fn flush<'i>(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if self.box_shadows.is_none() {
      return;
    }

    let box_shadows = std::mem::take(&mut self.box_shadows);

    if let Some((box_shadows, prefixes)) = box_shadows {
      if !self.flushed {
        let mut prefixes = context.targets.prefixes(prefixes, Feature::BoxShadow);
        let mut fallbacks = ColorFallbackKind::empty();
        for shadow in &box_shadows {
          fallbacks |= shadow.color.get_necessary_fallbacks(context.targets);
        }

        if fallbacks.contains(ColorFallbackKind::RGB) {
          let rgb = box_shadows
            .iter()
            .map(|shadow| BoxShadow {
              color: shadow.color.to_rgb(),
              ..shadow.clone()
            })
            .collect();
          dest.push(Property::BoxShadow(rgb, prefixes));
          if prefixes.contains(VendorPrefix::None) {
            prefixes = VendorPrefix::None;
          } else {
            // Only output RGB for prefixed property (e.g. -webkit-box-shadow)
            return;
          }
        }

        if fallbacks.contains(ColorFallbackKind::P3) {
          let p3 = box_shadows
            .iter()
            .map(|shadow| BoxShadow {
              color: shadow.color.to_p3(),
              ..shadow.clone()
            })
            .collect();
          dest.push(Property::BoxShadow(p3, VendorPrefix::None));
        }

        if fallbacks.contains(ColorFallbackKind::LAB) {
          let lab = box_shadows
            .iter()
            .map(|shadow| BoxShadow {
              color: shadow.color.to_lab(),
              ..shadow.clone()
            })
            .collect();
          dest.push(Property::BoxShadow(lab, VendorPrefix::None));
        } else {
          dest.push(Property::BoxShadow(box_shadows, prefixes))
        }
      } else {
        dest.push(Property::BoxShadow(box_shadows, prefixes))
      }
    }

    self.flushed = true;
  }
}
