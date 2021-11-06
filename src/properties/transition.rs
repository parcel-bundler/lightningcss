use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{ident::CustomIdent, time::Time, easing::EasingFunction};
use super::{Property, VendorPrefix};
use crate::printer::Printer;
use std::fmt::Write;
use itertools::izip;
use smallvec::SmallVec;

/// https://www.w3.org/TR/2018/WD-css-transitions-1-20181011/#transition-shorthand-property
#[derive(Debug, Clone, PartialEq)]
pub struct Transition {
  property: CustomIdent,
  duration: Time,
  delay: Time,
  timing_function: EasingFunction 
}

impl Parse for Transition {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut property = None;
    let mut duration = None;
    let mut delay = None;
    let mut timing_function = None;

    loop {
      if duration.is_none() {
        if let Ok(value) = input.try_parse(Time::parse) {
          duration = Some(value);
          continue
        }
      }

      if timing_function.is_none() {
        if let Ok(value) = input.try_parse(EasingFunction::parse) {
          timing_function = Some(value);
          continue
        }
      }

      if delay.is_none() {
        if let Ok(value) = input.try_parse(Time::parse) {
          delay = Some(value);
          continue
        }
      }

      if property.is_none() {
        if let Ok(value) = input.try_parse(CustomIdent::parse) {
          property = Some(value);
          continue
        }
      }

      break
    }

    Ok(Transition {
      property: property.unwrap_or(CustomIdent("all".into())),
      duration: duration.unwrap_or(Time::Seconds(0.0)),
      delay: delay.unwrap_or(Time::Seconds(0.0)),
      timing_function: timing_function.unwrap_or(EasingFunction::Ease)
    })
  }
}

impl ToCss for Transition {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.property.to_css(dest)?;
    if self.duration != 0.0 || self.delay != 0.0 {
      dest.write_char(' ')?;
      self.duration.to_css(dest)?;
    }

    if self.timing_function != EasingFunction::Ease && self.timing_function != EasingFunction::CubicBezier(0.25, 0.1, 0.25, 1.0) {
      dest.write_char(' ')?;
      self.timing_function.to_css(dest)?;
    }

    if self.delay != 0.0 {
      dest.write_char(' ')?;
      self.delay.to_css(dest)?;
    }

    Ok(())
  }
}

#[derive(Default)]
pub struct TransitionHandler {
  properties: Option<(SmallVec<[CustomIdent; 1]>, VendorPrefix)>,
  durations: Option<(SmallVec<[Time; 1]>, VendorPrefix)>,
  delays: Option<(SmallVec<[Time; 1]>, VendorPrefix)>,
  timing_functions: Option<(SmallVec<[EasingFunction; 1]>, VendorPrefix)>,
  decls: Vec<Property>
}

impl PropertyHandler for TransitionHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;

    macro_rules! property {
      ($prop: ident, $val: expr, $vp: ident) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush();
          }
        }

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
        } else {
          self.$prop = Some(($val.clone(), *$vp))
        }
      }};
    }

    match property {
      TransitionProperty(val, vp) => property!(properties, val, vp),
      TransitionDuration(val, vp) => property!(durations, val, vp),
      TransitionDelay(val, vp) => property!(delays, val, vp),
      TransitionTimingFunction(val, vp) => property!(timing_functions, val, vp),
      Transition(val, vp) => {
        let properties: SmallVec<[CustomIdent; 1]> = val.iter().map(|b| b.property.clone()).collect();
        property!(properties, &properties, vp);

        let durations: SmallVec<[Time; 1]> = val.iter().map(|b| b.duration.clone()).collect();
        property!(durations, &durations, vp);

        let delays: SmallVec<[Time; 1]> = val.iter().map(|b| b.delay.clone()).collect();
        property!(delays, &delays, vp);

        let timing_functions: SmallVec<[EasingFunction; 1]> = val.iter().map(|b| b.timing_function.clone()).collect();
        property!(timing_functions, &timing_functions, vp);
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self) -> Vec<Property> {
    self.flush();
    std::mem::take(&mut self.decls)
  }
}

impl TransitionHandler {
  fn flush(&mut self) {
    let mut properties = std::mem::take(&mut self.properties);
    let mut durations = std::mem::take(&mut self.durations);
    let mut delays = std::mem::take(&mut self.delays);
    let mut timing_functions = std::mem::take(&mut self.timing_functions);

    macro_rules! expand {
      ($prop: ident, $val: ident, $prefixes: ident) => {
        if $prefixes.contains(VendorPrefix::WebKit) {
          self.decls.push(Property::$prop($val.clone(), VendorPrefix::WebKit));
        }

        if $prefixes.contains(VendorPrefix::Moz) {
          self.decls.push(Property::$prop($val.clone(), VendorPrefix::Moz));
        }

        if $prefixes.contains(VendorPrefix::Ms) {
          self.decls.push(Property::$prop($val.clone(), VendorPrefix::Ms));
        }

        if $prefixes.contains(VendorPrefix::None) {
          self.decls.push(Property::$prop($val, VendorPrefix::None));
        }
      };
    }

    if let (Some((properties, property_prefixes)), Some((durations, duration_prefixes)), Some((delays, delay_prefixes)), Some((timing_functions, timing_prefixes))) = (&mut properties, &mut durations, &mut delays, &mut timing_functions) {
      // Only use shorthand syntax if the number of transitions matches on all properties.
      let len = properties.len();
      if durations.len() == len && delays.len() == len && timing_functions.len() == len {
        let transitions: SmallVec<[Transition; 1]> = izip!(properties, durations, delays, timing_functions).map(|(property, duration, delay, timing_function)| {
          Transition {
            property: property.clone(),
            duration: duration.clone(),
            delay: delay.clone(),
            timing_function: timing_function.clone()
          }
        }).collect();

        macro_rules! handle_prefix {
          ($prefix: ident) => {
            // If all properties have this vendor prefix, use the shorthand syntax.
            // Otherwise, we'll fall through below to individual properties.
            if property_prefixes.contains(VendorPrefix::$prefix) && duration_prefixes.contains(VendorPrefix::$prefix) && delay_prefixes.contains(VendorPrefix::$prefix) && timing_prefixes.contains(VendorPrefix::$prefix) {
              self.decls.push(Property::Transition(transitions.clone(), VendorPrefix::$prefix));
              property_prefixes.remove(VendorPrefix::$prefix);
              duration_prefixes.remove(VendorPrefix::$prefix);
              delay_prefixes.remove(VendorPrefix::$prefix);
              timing_prefixes.remove(VendorPrefix::$prefix);
            }
          };
        }

        handle_prefix!(WebKit);
        handle_prefix!(Moz);
        handle_prefix!(Ms);
        handle_prefix!(None);
      }
    }

    if let Some((properties, prefix)) = properties {
      expand!(TransitionProperty, properties, prefix);
    }

    if let Some((durations, prefix)) = durations {
      expand!(TransitionDuration, durations, prefix);
    }

    if let Some((delays, prefix)) = delays {
      expand!(TransitionDelay, delays, prefix);
    }

    if let Some((timing_functions, prefix)) = timing_functions {
      expand!(TransitionTimingFunction, timing_functions, prefix);
    }

    self.reset();
  }

  fn reset(&mut self) {
    self.properties = None;
    self.durations = None;
    self.delays = None;
    self.timing_functions = None;
  }
}
