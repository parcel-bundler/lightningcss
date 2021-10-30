use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{ident::CustomIdent, time::Time, easing::EasingFunction};
use super::Property;
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
  properties: Option<SmallVec<[CustomIdent; 1]>>,
  durations: Option<SmallVec<[Time; 1]>>,
  delays: Option<SmallVec<[Time; 1]>>,
  timing_functions: Option<SmallVec<[EasingFunction; 1]>> 
}

impl PropertyHandler for TransitionHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;
    match property {
      TransitionProperty(val) => self.properties = Some(val.clone()),
      TransitionDuration(val) => self.durations = Some(val.clone()),
      TransitionDelay(val) => self.delays = Some(val.clone()),
      TransitionTimingFunction(val) => self.timing_functions = Some(val.clone()),
      Transition(val) => {
        self.properties = Some(val.iter().map(|b| b.property.clone()).collect());
        self.durations = Some(val.iter().map(|b| b.duration.clone()).collect());
        self.delays = Some(val.iter().map(|b| b.delay.clone()).collect());
        self.timing_functions = Some(val.iter().map(|b| b.timing_function.clone()).collect());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self) -> Vec<Property> {
    let mut decls = vec![];

    let mut properties = std::mem::take(&mut self.properties);
    let mut durations = std::mem::take(&mut self.durations);
    let mut delays = std::mem::take(&mut self.delays);
    let mut timing_functions = std::mem::take(&mut self.timing_functions);

    if let (Some(properties), Some(durations), Some(delays), Some(timing_functions)) = (&mut properties, &mut durations, &mut delays, &mut timing_functions) {
      // Only use shorthand syntax if the number of transitions matches on all properties.
      let len = properties.len();
      if durations.len() == len && delays.len() == len && timing_functions.len() == len {
        let transitions = izip!(properties.drain(..), durations.drain(..), delays.drain(..), timing_functions.drain(..)).map(|(property, duration, delay, timing_function)| {
          Transition {
            property,
            duration,
            delay,
            timing_function
          }
        }).collect();
        decls.push(Property::Transition(transitions));
        return decls
      }
    }

    if let Some(properties) = properties {
      decls.push(Property::TransitionProperty(properties))
    }

    if let Some(durations) = durations {
      decls.push(Property::TransitionDuration(durations))
    }

    if let Some(delays) = delays {
      decls.push(Property::TransitionDelay(delays))
    }

    if let Some(timing_functions) = timing_functions {
      decls.push(Property::TransitionTimingFunction(timing_functions))
    }

    decls
  }
}
