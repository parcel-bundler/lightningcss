use cssparser::*;
use smallvec::SmallVec;
use crate::declaration::DeclarationList;
use crate::logical::LogicalProperties;
use crate::prefixes::Feature;
use crate::targets::Browsers;
use crate::values::length::Length;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::color::{CssColor, ColorFallbackKind};
use crate::printer::Printer;
use crate::error::{ParserError, PrinterError};
use crate::properties::Property;
use crate::vendor_prefix::VendorPrefix;

#[derive(Debug, Clone, PartialEq)]
pub struct BoxShadow {
  pub color: CssColor,
  pub x_offset: Length,
  pub y_offset: Length,
  pub blur: Length,
  pub spread: Length,
  pub inset: bool
}

impl<'i> Parse<'i> for BoxShadow {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color = None;
    let mut lengths = None;
    let mut inset = false;

    loop {
      if !inset {
        if input.try_parse(|input| input.expect_ident_matching("inset")).is_ok(){
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

      break
    }

    let lengths = lengths.ok_or(input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
    Ok(BoxShadow {
      color: color.unwrap_or(CssColor::current_color()),
      x_offset: lengths.0,
      y_offset: lengths.1,
      blur: lengths.2,
      spread: lengths.3,
      inset
    })
  }
}

impl ToCss for BoxShadow {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

#[derive(Default)]
pub(crate) struct BoxShadowHandler {
  targets: Option<Browsers>,
  box_shadows: Option<(SmallVec<[BoxShadow; 1]>, VendorPrefix)>
}

impl BoxShadowHandler {
  pub fn new(targets: Option<Browsers>) -> BoxShadowHandler {
    BoxShadowHandler {
      targets,
      ..BoxShadowHandler::default()
    }
  }
}

impl<'i> PropertyHandler<'i> for BoxShadowHandler {
  fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, logical: &mut LogicalProperties) -> bool {
    if let Property::BoxShadow(box_shadows, prefix) = property {
      if let Some((val, prefixes)) = &mut self.box_shadows {
        if val != box_shadows && !prefixes.contains(*prefix) {
          self.finalize(dest, logical);
          self.box_shadows = Some((box_shadows.clone(), *prefix));
        } else {
          *val = box_shadows.clone();
          *prefixes |= *prefix;
        }
      } else {
        self.box_shadows = Some((box_shadows.clone(), *prefix));
      }

      true
    } else {
      false
    }
  }

  fn finalize(&mut self, dest: &mut DeclarationList, _: &mut LogicalProperties) {
    let box_shadows = std::mem::take(&mut self.box_shadows);

    if let Some((box_shadows, prefixes)) = box_shadows {
      if let Some(targets) = self.targets {
        let mut prefixes = if prefixes.contains(VendorPrefix::None) {
          Feature::BoxShadow.prefixes_for(targets)
        } else {
          prefixes
        };
        
        let mut fallbacks = ColorFallbackKind::empty();
        for shadow in &box_shadows {
          fallbacks |= shadow.color.get_necessary_fallbacks(targets);
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
            return
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
  }
}
