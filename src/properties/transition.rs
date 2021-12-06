use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{ident::CustomIdent, time::Time, easing::EasingFunction};
use super::{Property, PropertyId};
use crate::vendor_prefix::VendorPrefix;
use crate::declaration::DeclarationList;
use crate::printer::Printer;
use itertools::izip;
use smallvec::SmallVec;
use crate::targets::Browsers;
use crate::prefixes::Feature;

/// https://www.w3.org/TR/2018/WD-css-transitions-1-20181011/#transition-shorthand-property
#[derive(Debug, Clone, PartialEq)]
pub struct Transition {
  pub property: CustomIdent,
  pub duration: Time,
  pub delay: Time,
  pub timing_function: EasingFunction 
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
pub(crate) struct TransitionHandler {
  targets: Option<Browsers>,
  properties: Option<(SmallVec<[CustomIdent; 1]>, VendorPrefix)>,
  durations: Option<(SmallVec<[Time; 1]>, VendorPrefix)>,
  delays: Option<(SmallVec<[Time; 1]>, VendorPrefix)>,
  timing_functions: Option<(SmallVec<[EasingFunction; 1]>, VendorPrefix)>
}

impl TransitionHandler {
  pub fn new(targets: Option<Browsers>) -> TransitionHandler {
    TransitionHandler {
      targets,
      ..TransitionHandler::default()
    }
  }
}

impl PropertyHandler for TransitionHandler {
  fn handle_property(&mut self, property: &Property, dest: &mut DeclarationList) -> bool {
    use Property::*;

    macro_rules! property {
      ($feature: ident, $prop: ident, $val: expr, $vp: ident) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush(dest);
          }
        }

        // Otherwise, update the value and add the prefix.
        if let Some((val, prefixes)) = &mut self.$prop {
          *val = $val.clone();
          *prefixes |= *$vp;
          if prefixes.contains(VendorPrefix::None) {
            if let Some(targets) = self.targets {
              *prefixes = Feature::$feature.prefixes_for(targets);
            }
          }
        } else {
          let prefixes = if $vp.contains(VendorPrefix::None) {
            if let Some(targets) = self.targets {
              Feature::$feature.prefixes_for(targets)
            } else {
              *$vp
            }
          } else {
            *$vp
          };
          self.$prop = Some(($val.clone(), prefixes))
        }
      }};
    }

    match property {
      TransitionProperty(val, vp) => property!(TransitionProperty, properties, val, vp),
      TransitionDuration(val, vp) => property!(TransitionDuration, durations, val, vp),
      TransitionDelay(val, vp) => property!(TransitionDelay, delays, val, vp),
      TransitionTimingFunction(val, vp) => property!(TransitionTimingFunction, timing_functions, val, vp),
      Transition(val, vp) => {
        let properties: SmallVec<[CustomIdent; 1]> = val.iter().map(|b| b.property.clone()).collect();
        property!(TransitionProperty, properties, &properties, vp);

        let durations: SmallVec<[Time; 1]> = val.iter().map(|b| b.duration.clone()).collect();
        property!(TransitionDuration, durations, &durations, vp);

        let delays: SmallVec<[Time; 1]> = val.iter().map(|b| b.delay.clone()).collect();
        property!(TransitionDelay, delays, &delays, vp);

        let timing_functions: SmallVec<[EasingFunction; 1]> = val.iter().map(|b| b.timing_function.clone()).collect();
        property!(TransitionTimingFunction, timing_functions, &timing_functions, vp);
      }
      Unparsed(val) if is_transition_property(&val.property_id) => {
        self.flush(dest);
        dest.push(Property::Unparsed(val.get_prefixed(self.targets, Feature::Transition)));
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList) {
    self.flush(dest);
  }
}

impl TransitionHandler {
  fn flush(&mut self, dest: &mut DeclarationList) {
    let mut properties = std::mem::take(&mut self.properties);
    let mut durations = std::mem::take(&mut self.durations);
    let mut delays = std::mem::take(&mut self.delays);
    let mut timing_functions = std::mem::take(&mut self.timing_functions);

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

        // Find the intersection of prefixes with the same value.
        // Remove that from the prefixes of each of the properties. The remaining
        // prefixes will be handled by outputing individual properties below.
        let intersection = *property_prefixes & *duration_prefixes & *delay_prefixes & *timing_prefixes;
        if !intersection.is_empty() {
          dest.push(Property::Transition(transitions.clone(), intersection));
          property_prefixes.remove(intersection);
          duration_prefixes.remove(intersection);
          delay_prefixes.remove(intersection);
          timing_prefixes.remove(intersection);
        }
      }
    }

    if let Some((properties, prefix)) = properties {
      if !prefix.is_empty() {
        dest.push(Property::TransitionProperty(properties, prefix));
      }
    }

    if let Some((durations, prefix)) = durations {
      if !prefix.is_empty() {
        dest.push(Property::TransitionDuration(durations, prefix));
      }
    }

    if let Some((delays, prefix)) = delays {
      if !prefix.is_empty() {
        dest.push(Property::TransitionDelay(delays, prefix));
      }
    }

    if let Some((timing_functions, prefix)) = timing_functions {
      if !prefix.is_empty() {
        dest.push(Property::TransitionTimingFunction(timing_functions, prefix));
      }
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

#[inline]
fn is_transition_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::TransitionProperty |
    PropertyId::TransitionDuration |
    PropertyId::TransitionDelay |
    PropertyId::TransitionTimingFunction |
    PropertyId::Transition => true,
    _ => false
  }
}
