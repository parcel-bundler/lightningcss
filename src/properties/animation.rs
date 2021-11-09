use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{time::Time, easing::EasingFunction};
use super::prefixes::{Feature, Browsers};
use crate::properties::{Property, VendorPrefix};
use crate::printer::Printer;
use std::fmt::Write;
use itertools::izip;
use crate::macros::*;
use smallvec::SmallVec;

/// https://drafts.csswg.org/css-animations/#animation-name
#[derive(Debug, Clone, PartialEq)]
pub enum AnimationName {
  None,
  String(String)
}

impl Parse for AnimationName {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("none")).is_ok() {
      return Ok(AnimationName::None)
    }

    let location = input.current_source_location();
    let name = match *input.next()? {
      Token::Ident(ref s) => s.as_ref(),
      Token::QuotedString(ref s) => s.as_ref(),
      ref t => return Err(location.new_unexpected_token_error(t.clone())),
    };
    Ok(AnimationName::String(name.into()))
  }
}

impl ToCss for AnimationName {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      AnimationName::None => dest.write_str("none"),
      AnimationName::String(s) => serialize_identifier(&s, dest)
    }
  }
}

/// https://drafts.csswg.org/css-animations/#animation-iteration-count
#[derive(Debug, Clone, PartialEq)]
pub enum AnimationIterationCount {
  Number(f32),
  Infinite
}

impl Parse for AnimationIterationCount {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    if input.try_parse(|input| input.expect_ident_matching("infinite")).is_ok() {
      return Ok(AnimationIterationCount::Infinite)
    }

    let number = f32::parse(input)?;
    return Ok(AnimationIterationCount::Number(number))
  }
}

impl ToCss for AnimationIterationCount {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      AnimationIterationCount::Number(val) => val.to_css(dest),
      AnimationIterationCount::Infinite => dest.write_str("infinite")
    }
  }
}

// https://drafts.csswg.org/css-animations/#animation-direction
enum_property!(AnimationDirection,
  ("normal", Normal),
  ("reverse", Reverse),
  ("alternate", Alternate),
  ("alternate-reverse", AlternateReverse)
);

// https://drafts.csswg.org/css-animations/#animation-play-state
enum_property!(AnimationPlayState,
  Running,
  Paused
);

// https://drafts.csswg.org/css-animations/#animation-fill-mode
enum_property!(AnimationFillMode,
  None,
  Forwards,
  Backwards,
  Both
);

/// https://drafts.csswg.org/css-animations/#animation
#[derive(Debug, Clone, PartialEq)]
pub struct Animation {
  name: AnimationName,
  duration: Time,
  timing_function: EasingFunction,
  iteration_count: AnimationIterationCount,
  direction: AnimationDirection,
  play_state: AnimationPlayState,
  delay: Time,
  fill_mode: AnimationFillMode
}

impl Parse for Animation {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    let mut name = None;
    let mut duration = None;
    let mut timing_function = None;
    let mut iteration_count = None;
    let mut direction = None;
    let mut play_state = None;
    let mut delay = None;
    let mut fill_mode = None;

    macro_rules! parse_prop {
      ($var: ident, $type: ident) => {
        if $var.is_none() {
          if let Ok(value) = input.try_parse($type::parse) {
            $var = Some(value);
            continue
          }
        }
      };
    }

    loop {
      parse_prop!(duration, Time);
      parse_prop!(timing_function, EasingFunction);
      parse_prop!(delay, Time);
      parse_prop!(iteration_count, AnimationIterationCount);
      parse_prop!(direction, AnimationDirection);
      parse_prop!(fill_mode, AnimationFillMode);
      parse_prop!(play_state, AnimationPlayState);
      parse_prop!(name, AnimationName);
      break
    }

    Ok(Animation {
      name: name.unwrap_or(AnimationName::None),
      duration: duration.unwrap_or(Time::Seconds(0.0)),
      timing_function: timing_function.unwrap_or(EasingFunction::Ease),
      iteration_count: iteration_count.unwrap_or(AnimationIterationCount::Number(1.0)),
      direction: direction.unwrap_or(AnimationDirection::Normal),
      play_state: play_state.unwrap_or(AnimationPlayState::Running),
      delay: delay.unwrap_or(Time::Seconds(0.0)),
      fill_mode: fill_mode.unwrap_or(AnimationFillMode::None),
    })
  }
}

impl ToCss for Animation {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    self.name.to_css(dest)?;
    match &self.name {
      AnimationName::None => return Ok(()),
      AnimationName::String(name) => {
        if self.duration != 0.0 || self.delay != 0.0 {
          dest.write_char(' ')?;
          self.duration.to_css(dest)?;
        }
    
        if (self.timing_function != EasingFunction::Ease && self.timing_function != EasingFunction::CubicBezier(0.25, 0.1, 0.25, 1.0)) || EasingFunction::is_ident(&name) {
          dest.write_char(' ')?;
          self.timing_function.to_css(dest)?;
        }
    
        if self.delay != 0.0 {
          dest.write_char(' ')?;
          self.delay.to_css(dest)?;
        }
    
        if self.iteration_count != AnimationIterationCount::Number(1.0) || name == "infinite" {
          dest.write_char(' ')?;
          self.iteration_count.to_css(dest)?;
        }
    
        if self.direction != AnimationDirection::Normal || AnimationDirection::from_str(&name).is_some() {
          dest.write_char(' ')?;
          self.direction.to_css(dest)?;
        }
    
        if self.fill_mode != AnimationFillMode::None || AnimationFillMode::from_str(&name).is_some() {
          dest.write_char(' ')?;
          self.fill_mode.to_css(dest)?;
        }
    
        if self.play_state != AnimationPlayState::Running || AnimationPlayState::from_str(&name).is_some() {
          dest.write_char(' ')?;
          self.play_state.to_css(dest)?;
        }
      }
    }

    Ok(())
  }
}

#[derive(Default)]
pub struct AnimationHandler {
  targets: Option<Browsers>,
  names: Option<(SmallVec<[AnimationName; 1]>, VendorPrefix)>,
  durations: Option<(SmallVec<[Time; 1]>, VendorPrefix)>,
  timing_functions: Option<(SmallVec<[EasingFunction; 1]>, VendorPrefix)>,
  iteration_counts: Option<(SmallVec<[AnimationIterationCount; 1]>, VendorPrefix)>,
  directions: Option<(SmallVec<[AnimationDirection; 1]>, VendorPrefix)>,
  play_states: Option<(SmallVec<[AnimationPlayState; 1]>, VendorPrefix)>,
  delays: Option<(SmallVec<[Time; 1]>, VendorPrefix)>,
  fill_modes: Option<(SmallVec<[AnimationFillMode; 1]>, VendorPrefix)>,
  decls: Vec<Property>
}

impl AnimationHandler {
  pub fn new(targets: Option<Browsers>) -> AnimationHandler {
    AnimationHandler {
      targets,
      ..AnimationHandler::default()
    }
  }
}

impl PropertyHandler for AnimationHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;

    macro_rules! maybe_flush {
      ($prop: ident, $val: expr, $vp: ident) => {{
        // If two vendor prefixes for the same property have different
        // values, we need to flush what we have immediately to preserve order.
        if let Some((val, prefixes)) = &self.$prop {
          if val != $val && !prefixes.contains(*$vp) {
            self.flush();
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
          self.$prop = Some(($val.clone(), *$vp))
        }
      }};
    }

    match property {
      AnimationName(val, vp) => property!(names, val, vp),
      AnimationDuration(val, vp) => property!(durations, val, vp),
      AnimationTimingFunction(val, vp) => property!(timing_functions, val, vp),
      AnimationIterationCount(val, vp) => property!(iteration_counts, val, vp),
      AnimationDirection(val, vp) => property!(directions, val, vp),
      AnimationPlayState(val, vp) => property!(play_states, val, vp),
      AnimationDelay(val, vp) => property!(delays, val, vp),
      AnimationFillMode(val, vp) => property!(fill_modes, val, vp),
      Animation(val, vp) => {
        let names = val.iter().map(|b| b.name.clone()).collect();
        maybe_flush!(names, &names, vp);

        let durations = val.iter().map(|b| b.duration.clone()).collect();
        maybe_flush!(durations, &durations, vp);

        let timing_functions = val.iter().map(|b| b.timing_function.clone()).collect();
        maybe_flush!(timing_functions, &timing_functions, vp);

        let iteration_counts = val.iter().map(|b| b.iteration_count.clone()).collect();
        maybe_flush!(iteration_counts, &iteration_counts, vp);

        let directions = val.iter().map(|b| b.direction.clone()).collect();
        maybe_flush!(directions, &directions, vp);

        let play_states = val.iter().map(|b| b.play_state.clone()).collect();
        maybe_flush!(play_states, &play_states, vp);

        let delays = val.iter().map(|b| b.delay.clone()).collect();
        maybe_flush!(delays, &delays, vp);

        let fill_modes = val.iter().map(|b| b.fill_mode.clone()).collect();
        maybe_flush!(fill_modes, &fill_modes, vp);

        property!(names, &names, vp);
        property!(durations, &durations, vp);
        property!(timing_functions, &timing_functions, vp);
        property!(iteration_counts, &iteration_counts, vp);
        property!(directions, &directions, vp);
        property!(play_states, &play_states, vp);
        property!(delays, &delays, vp);
        property!(fill_modes, &fill_modes, vp);
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

impl AnimationHandler {
  fn flush(&mut self) {
    let mut names = std::mem::take(&mut self.names);
    let mut durations = std::mem::take(&mut self.durations);
    let mut timing_functions = std::mem::take(&mut self.timing_functions);
    let mut iteration_counts = std::mem::take(&mut self.iteration_counts);
    let mut directions = std::mem::take(&mut self.directions);
    let mut play_states = std::mem::take(&mut self.play_states);
    let mut delays = std::mem::take(&mut self.delays);
    let mut fill_modes = std::mem::take(&mut self.fill_modes);

    if let (Some((names, names_vp)), Some((durations, durations_vp)), Some((timing_functions, timing_functions_vp)), Some((iteration_counts, iteration_counts_vp)), Some((directions, directions_vp)), Some((play_states, play_states_vp)), Some((delays, delays_vp)), Some((fill_modes, fill_modes_vp))) = (&mut names, &mut durations, &mut timing_functions, &mut iteration_counts, &mut directions, &mut play_states, &mut delays, &mut fill_modes) {
      // Only use shorthand syntax if the number of animations matches on all properties.
      let len = names.len();
      let intersection = *names_vp & *durations_vp & *timing_functions_vp & *iteration_counts_vp & *directions_vp & *play_states_vp & *delays_vp & *fill_modes_vp;
      if !intersection.is_empty() && durations.len() == len && timing_functions.len() == len && iteration_counts.len() == len && directions.len() == len && play_states.len() == len && delays.len() == len && fill_modes.len() == len {
        let animations = izip!(names.drain(..), durations.drain(..), timing_functions.drain(..), iteration_counts.drain(..), directions.drain(..), play_states.drain(..), delays.drain(..), fill_modes.drain(..)).map(|(name, duration, timing_function, iteration_count, direction, play_state, delay, fill_mode)| {
          Animation {
            name,
            duration,
            timing_function,
            iteration_count,
            direction,
            play_state,
            delay,
            fill_mode
          }
        }).collect();
        let mut prefix = intersection;
        if prefix.contains(VendorPrefix::None) {
          if let Some(targets) = self.targets {
            prefix = Feature::Animation.prefixes_for(targets)
          }
        }
        self.decls.push(Property::Animation(animations, prefix));
        names_vp.remove(intersection);
        durations_vp.remove(intersection);
        timing_functions_vp.remove(intersection);
        iteration_counts_vp.remove(intersection);
        directions_vp.remove(intersection);
        play_states_vp.remove(intersection);
        delays_vp.remove(intersection);
        fill_modes_vp.remove(intersection);
      }
    }

    macro_rules! prop {
      ($var: ident, $property: ident) => {
        if let Some((val, vp)) = $var {
          if !vp.is_empty() {
            let mut prefix = vp;
            if prefix.contains(VendorPrefix::None) {
              if let Some(targets) = self.targets {
                prefix = Feature::$property.prefixes_for(targets)
              }
            }
            self.decls.push(Property::$property(val, prefix))
          }
        }
      };
    }

    prop!(names, AnimationName);
    prop!(durations, AnimationDuration);
    prop!(timing_functions, AnimationTimingFunction);
    prop!(iteration_counts, AnimationIterationCount);
    prop!(directions, AnimationDirection);
    prop!(play_states, AnimationPlayState);
    prop!(delays, AnimationDelay);
    prop!(fill_modes, AnimationFillMode);
  }
}
