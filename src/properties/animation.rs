use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{time::Time, easing::EasingFunction};
use super::Property;
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
  names: Option<SmallVec<[AnimationName; 1]>>,
  durations: Option<SmallVec<[Time; 1]>>,
  timing_functions: Option<SmallVec<[EasingFunction; 1]>>,
  iteration_counts: Option<SmallVec<[AnimationIterationCount; 1]>>,
  directions: Option<SmallVec<[AnimationDirection; 1]>>,
  play_states: Option<SmallVec<[AnimationPlayState; 1]>>,
  delays: Option<SmallVec<[Time; 1]>>,
  fill_modes: Option<SmallVec<[AnimationFillMode; 1]>>
}

impl PropertyHandler for AnimationHandler {
  fn handle_property(&mut self, property: &Property) -> bool {
    use Property::*;
    match property {
      AnimationName(val) => self.names = Some(val.clone()),
      AnimationDuration(val) => self.durations = Some(val.clone()),
      AnimationTimingFunction(val) => self.timing_functions = Some(val.clone()),
      AnimationIterationCount(val) => self.iteration_counts = Some(val.clone()),
      AnimationDirection(val) => self.directions = Some(val.clone()),
      AnimationPlayState(val) => self.play_states = Some(val.clone()),
      AnimationDelay(val) => self.delays = Some(val.clone()),
      AnimationFillMode(val) => self.fill_modes = Some(val.clone()),
      Animation(val) => {
        self.names = Some(val.iter().map(|b| b.name.clone()).collect());
        self.durations = Some(val.iter().map(|b| b.duration.clone()).collect());
        self.timing_functions = Some(val.iter().map(|b| b.timing_function.clone()).collect());
        self.iteration_counts = Some(val.iter().map(|b| b.iteration_count.clone()).collect());
        self.directions = Some(val.iter().map(|b| b.direction.clone()).collect());
        self.play_states = Some(val.iter().map(|b| b.play_state.clone()).collect());
        self.delays = Some(val.iter().map(|b| b.delay.clone()).collect());
        self.fill_modes = Some(val.iter().map(|b| b.fill_mode.clone()).collect());
      }
      _ => return false
    }

    true
  }

  fn finalize(&mut self) -> Vec<Property> {
    let mut decls = vec![];

    let mut names = std::mem::take(&mut self.names);
    let mut durations = std::mem::take(&mut self.durations);
    let mut timing_functions = std::mem::take(&mut self.timing_functions);
    let mut iteration_counts = std::mem::take(&mut self.iteration_counts);
    let mut directions = std::mem::take(&mut self.directions);
    let mut play_states = std::mem::take(&mut self.play_states);
    let mut delays = std::mem::take(&mut self.delays);
    let mut fill_modes = std::mem::take(&mut self.fill_modes);

    if let (Some(names), Some(durations), Some(timing_functions), Some(iteration_counts), Some(directions), Some(play_states), Some(delays), Some(fill_modes)) = (&mut names, &mut durations, &mut timing_functions, &mut iteration_counts, &mut directions, &mut play_states, &mut delays, &mut fill_modes) {
      // Only use shorthand syntax if the number of animations matches on all properties.
      let len = names.len();
      if durations.len() == len && timing_functions.len() == len && iteration_counts.len() == len && directions.len() == len && play_states.len() == len && delays.len() == len && fill_modes.len() == len {
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
        decls.push(Property::Animation(animations));
        return decls
      }
    }

    macro_rules! prop {
      ($var: ident, $property: ident) => {
        if let Some($var) = $var {
          decls.push(Property::$property($var))
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

    decls
  }
}
