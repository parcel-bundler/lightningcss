use cssparser::*;
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::values::{ident::CustomIdent, time::Time, easing::EasingFunction};
use super::Property;
use crate::printer::Printer;
use std::fmt::Write;

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
    if self.duration != 0.0 {
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
