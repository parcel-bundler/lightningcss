//! Mathematical calculation functions and expressions.

use crate::compat::Feature;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::printer::Printer;
use crate::traits::private::AddInternal;
use crate::traits::{Parse, ToCss};
use cssparser::*;

use super::angle::Angle;
use super::number::CSSNumber;

/// A CSS [math function](https://www.w3.org/TR/css-values-4/#math-function).
///
/// Math functions may be used in most properties and values that accept numeric
/// values, including lengths, percentages, angles, times, etc.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum MathFunction<V> {
  /// The [`calc()`](https://www.w3.org/TR/css-values-4/#calc-func) function.
  Calc(Calc<V>),
  /// The [`min()`](https://www.w3.org/TR/css-values-4/#funcdef-min) function.
  Min(Vec<Calc<V>>),
  /// The [`max()`](https://www.w3.org/TR/css-values-4/#funcdef-max) function.
  Max(Vec<Calc<V>>),
  /// The [`clamp()`](https://www.w3.org/TR/css-values-4/#funcdef-clamp) function.
  Clamp(Calc<V>, Calc<V>, Calc<V>),
  /// The [`round()`](https://www.w3.org/TR/css-values-4/#funcdef-round) function.
  Round(RoundingStrategy, Calc<V>, Calc<V>),
  /// The [`rem()`](https://www.w3.org/TR/css-values-4/#funcdef-rem) function.
  Rem(Calc<V>, Calc<V>),
  /// The [`mod()`](https://www.w3.org/TR/css-values-4/#funcdef-mod) function.
  Mod(Calc<V>, Calc<V>),
}

enum_property! {
  /// A [rounding strategy](https://www.w3.org/TR/css-values-4/#typedef-rounding-strategy),
  /// as used in the `round()` function.
  pub enum RoundingStrategy {
    /// Round to the nearest integer.
    "nearest": Nearest,
    /// Round up (ceil).
    "up": Up,
    /// Round down (floor).
    "down": Down,
    /// Round toward zero (truncate).
    "to-zero": ToZero,
  }
}

impl Default for RoundingStrategy {
  fn default() -> Self {
    RoundingStrategy::Nearest
  }
}

/// A trait for values that can be rounded.
pub trait Round {
  /// Rounds a value to a multiple of `to` using the given rounding strategy.
  fn round(&self, to: &Self, strategy: RoundingStrategy) -> Self;
}

/// A trait for values that can potentially be rounded (e.g. if they have the same unit).
pub trait TryRound: Sized {
  /// Rounds a value to a multiple of `to` using the given rounding strategy, if possible.
  fn try_round(&self, to: &Self, strategy: RoundingStrategy) -> Option<Self>;
}

impl<T: Round> TryRound for T {
  fn try_round(&self, to: &Self, strategy: RoundingStrategy) -> Option<Self> {
    Some(self.round(to, strategy))
  }
}

/// A trait for values that potentially support a remainder (e.g. if they have the same unit).
pub trait TryRem: Sized {
  /// Returns the remainder between two values, if possible.
  fn try_rem(&self, rhs: &Self) -> Option<Self>;
}

impl<T: std::ops::Rem<T, Output = T> + Clone> TryRem for T {
  fn try_rem(&self, rhs: &Self) -> Option<Self> {
    Some(self.clone() % rhs.clone())
  }
}

impl<V: ToCss + std::cmp::PartialOrd<f32> + std::ops::Mul<f32, Output = V> + Clone + std::fmt::Debug> ToCss
  for MathFunction<V>
{
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      MathFunction::Calc(calc) => {
        dest.write_str("calc(")?;
        calc.to_css(dest)?;
        dest.write_char(')')
      }
      MathFunction::Min(args) => {
        dest.write_str("min(")?;
        let mut first = true;
        for arg in args {
          if first {
            first = false;
          } else {
            dest.delim(',', false)?;
          }
          arg.to_css(dest)?;
        }
        dest.write_char(')')
      }
      MathFunction::Max(args) => {
        dest.write_str("max(")?;
        let mut first = true;
        for arg in args {
          if first {
            first = false;
          } else {
            dest.delim(',', false)?;
          }
          arg.to_css(dest)?;
        }
        dest.write_char(')')
      }
      MathFunction::Clamp(a, b, c) => {
        // If clamp() is unsupported by targets, output min()/max()
        if let Some(targets) = dest.targets {
          if !Feature::Clamp.is_compatible(targets) {
            dest.write_str("max(")?;
            a.to_css(dest)?;
            dest.delim(',', false)?;
            dest.write_str("min(")?;
            b.to_css(dest)?;
            dest.delim(',', false)?;
            c.to_css(dest)?;
            dest.write_str("))")?;
            return Ok(());
          }
        }

        dest.write_str("clamp(")?;
        a.to_css(dest)?;
        dest.delim(',', false)?;
        b.to_css(dest)?;
        dest.delim(',', false)?;
        c.to_css(dest)?;
        dest.write_char(')')
      }
      MathFunction::Round(strategy, a, b) => {
        dest.write_str("round(")?;
        if *strategy != RoundingStrategy::default() {
          strategy.to_css(dest)?;
          dest.delim(',', false)?;
        }
        a.to_css(dest)?;
        dest.delim(',', false)?;
        b.to_css(dest)?;
        dest.write_char(')')
      }
      MathFunction::Rem(a, b) => {
        dest.write_str("rem(")?;
        a.to_css(dest)?;
        dest.delim(',', false)?;
        b.to_css(dest)?;
        dest.write_char(')')
      }
      MathFunction::Mod(a, b) => {
        dest.write_str("mod(")?;
        a.to_css(dest)?;
        dest.delim(',', false)?;
        b.to_css(dest)?;
        dest.write_char(')')
      }
    }
  }
}

/// A mathematical expression used within the [`calc()`](https://www.w3.org/TR/css-values-4/#calc-func) function.
///
/// This type supports generic value types. Values such as [Length](super::length::Length), [Percentage](super::percentage::Percentage),
/// [Time](super::time::Time), and [Angle](super::angle::Angle) support `calc()` expressions.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
pub enum Calc<V> {
  /// A literal value.
  Value(Box<V>),
  /// A literal number.
  Number(CSSNumber),
  /// A sum of two calc expressions.
  Sum(Box<Calc<V>>, Box<Calc<V>>),
  /// A product of a number and another calc expression.
  Product(CSSNumber, Box<Calc<V>>),
  /// A math function, such as `calc()`, `min()`, or `max()`.
  Function(Box<MathFunction<V>>),
}

enum_property! {
  /// A mathematical constant.
  pub enum Constant {
    /// The base of the natural logarithm
    "e": E,
    /// The ratio of a circle’s circumference to its diameter
    "pi": Pi,
    /// infinity
    "infinity": Infinity,
    /// -infinity
    "-infinity": NegativeInfinity,
    /// Not a number.
    "nan": Nan,
  }
}

impl Into<f32> for Constant {
  fn into(self) -> f32 {
    use std::f32::consts;
    use Constant::*;
    match self {
      E => consts::E,
      Pi => consts::PI,
      Infinity => f32::INFINITY,
      NegativeInfinity => -f32::INFINITY,
      Nan => f32::NAN,
    }
  }
}

impl<
    'i,
    V: Parse<'i>
      + std::ops::Mul<f32, Output = V>
      + AddInternal
      + TryRound
      + TryRem
      + std::cmp::PartialOrd<V>
      + Into<Calc<V>>
      + From<Calc<V>>
      + TryFrom<Angle>
      + Clone
      + std::fmt::Debug,
  > Parse<'i> for Calc<V>
{
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let f = input.expect_function()?;
    match_ignore_ascii_case! { &f,
      "calc" => {
        let calc = input.parse_nested_block(Calc::parse_sum)?;
        match calc {
          Calc::Value(_) | Calc::Number(_) => Ok(calc),
          _ => Ok(Calc::Function(Box::new(MathFunction::Calc(calc))))
        }
      },
      "min" => {
        let mut args = input.parse_nested_block(|input| input.parse_comma_separated(Calc::parse_sum))?;
        let mut reduced = Calc::reduce_args(&mut args, std::cmp::Ordering::Less);
        if reduced.len() == 1 {
          return Ok(reduced.remove(0))
        }
        Ok(Calc::Function(Box::new(MathFunction::Min(reduced))))
      },
      "max" => {
        let mut args = input.parse_nested_block(|input| input.parse_comma_separated(Calc::parse_sum))?;
        let mut reduced = Calc::reduce_args(&mut args, std::cmp::Ordering::Greater);
        if reduced.len() == 1 {
          return Ok(reduced.remove(0))
        }
        Ok(Calc::Function(Box::new(MathFunction::Max(reduced))))
      },
      "clamp" => {
        let (mut min, mut center, mut max) = input.parse_nested_block(|input| {
          let min = Some(Calc::parse_sum(input)?);
          input.expect_comma()?;
          let center: Calc<V> = Calc::parse_sum(input)?;
          input.expect_comma()?;
          let max = Some(Calc::parse_sum(input)?);
          Ok((min, center, max))
        })?;

        // According to the spec, the minimum should "win" over the maximum if they are in the wrong order.
        let cmp = if let (Some(Calc::Value(max_val)), Calc::Value(center_val)) = (&max, &center) {
          center_val.partial_cmp(&max_val)
        } else {
          None
        };

        // If center is known to be greater than the maximum, replace it with maximum and remove the max argument.
        // Otherwise, if center is known to be less than the maximum, remove the max argument.
        match cmp {
          Some(std::cmp::Ordering::Greater) => {
            center = std::mem::take(&mut max).unwrap();
          }
          Some(_) => {
            max = None;
          }
          None => {}
        }

        let cmp = if let (Some(Calc::Value(min_val)), Calc::Value(center_val)) = (&min, &center) {
          center_val.partial_cmp(&min_val)
        } else {
          None
        };

        // If center is known to be less than the minimum, replace it with minimum and remove the min argument.
        // Otherwise, if center is known to be greater than the minimum, remove the min argument.
        match cmp {
          Some(std::cmp::Ordering::Less) => {
            center = std::mem::take(&mut min).unwrap();
          }
          Some(_) => {
            min = None;
          }
          None => {}
        }

        // Generate clamp(), min(), max(), or value depending on which arguments are left.
        match (min, max) {
          (None, None) => Ok(center),
          (Some(min), None) => Ok(Calc::Function(Box::new(MathFunction::Max(vec![min, center])))),
          (None, Some(max)) => Ok(Calc::Function(Box::new(MathFunction::Min(vec![center, max])))),
          (Some(min), Some(max)) => Ok(Calc::Function(Box::new(MathFunction::Clamp(min, center, max))))
        }
      },
      "round" => {
        input.parse_nested_block(|input| {
          let strategy = if let Ok(s) = input.try_parse(RoundingStrategy::parse) {
            input.expect_comma()?;
            s
          } else {
            RoundingStrategy::default()
          };

          let a: Calc<V> = Calc::parse_sum(input)?;
          input.expect_comma()?;
          let b: Calc<V> = Calc::parse_sum(input)?;

          if let (Calc::Value(a), Calc::Value(b)) = (&a, &b) {
            if let Some(rounded) = a.try_round(&**b, strategy) {
              return Ok(Calc::Value(Box::new(rounded)))
            }
          }

          Ok(Calc::Function(Box::new(MathFunction::Round(strategy, a, b))))
        })
      },
      "rem" => {
        input.parse_nested_block(|input| {
          let a: Calc<V> = Calc::parse_sum(input)?;
          input.expect_comma()?;
          let b: Calc<V> = Calc::parse_sum(input)?;

          if let (Calc::Value(a), Calc::Value(b)) = (&a, &b) {
            if let Some(rem) = a.try_rem(&**b) {
              return Ok(Calc::Value(Box::new(rem)))
            }
          }

          Ok(Calc::Function(Box::new(MathFunction::Rem(a, b))))
        })
      },
      "mod" => {
        input.parse_nested_block(|input| {
          let a: Calc<V> = Calc::parse_sum(input)?;
          input.expect_comma()?;
          let b: Calc<V> = Calc::parse_sum(input)?;

          if let (Calc::Value(a), Calc::Value(b)) = (&a, &b) {
            // ((a % b) + b) % b
            if let Some(rem) = a.try_rem(&**b) {
              let rem = rem.add((**b).clone()).try_rem(b).unwrap();
              return Ok(Calc::Value(Box::new(rem)))
            }
          }

          Ok(Calc::Function(Box::new(MathFunction::Rem(a, b))))
        })
      },
      "sin" => Self::parse_trig(input, f32::sin, false),
      "cos" => Self::parse_trig(input, f32::cos, false),
      "tan" => Self::parse_trig(input, f32::tan, false),
      "asin" => Self::parse_trig(input, f32::asin, true),
      "acos" => Self::parse_trig(input, f32::acos, true),
      "atan" => Self::parse_trig(input, f32::atan, true),
       _ => Err(location.new_unexpected_token_error(Token::Ident(f.clone()))),
    }
  }
}

impl<
    'i,
    V: Parse<'i>
      + std::ops::Mul<f32, Output = V>
      + AddInternal
      + TryRound
      + TryRem
      + std::cmp::PartialOrd<V>
      + Into<Calc<V>>
      + From<Calc<V>>
      + TryFrom<Angle>
      + Clone
      + std::fmt::Debug,
  > Calc<V>
{
  fn parse_sum<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut cur: Calc<V> = Calc::parse_product(input)?;
    loop {
      let start = input.state();
      match input.next_including_whitespace() {
        Ok(&Token::WhiteSpace(_)) => {
          if input.is_exhausted() {
            break; // allow trailing whitespace
          }
          match *input.next()? {
            Token::Delim('+') => {
              let next = Calc::parse_product(input)?;
              cur = cur.add(next);
            }
            Token::Delim('-') => {
              let mut rhs = Calc::parse_product(input)?;
              rhs = rhs * -1.0;
              cur = cur.add(rhs);
            }
            ref t => {
              let t = t.clone();
              return Err(input.new_unexpected_token_error(t));
            }
          }
        }
        _ => {
          input.reset(&start);
          break;
        }
      }
    }
    Ok(cur)
  }

  fn parse_product<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut node = Calc::parse_value(input)?;
    loop {
      let start = input.state();
      match input.next() {
        Ok(&Token::Delim('*')) => {
          // At least one of the operands must be a number.
          let rhs = Self::parse_value(input)?;
          if let Calc::Number(val) = rhs {
            node = node * val;
          } else if let Calc::Number(val) = node {
            node = rhs;
            node = node * val;
          } else {
            return Err(input.new_unexpected_token_error(Token::Delim('*')));
          }
        }
        Ok(&Token::Delim('/')) => {
          let rhs = Self::parse_value(input)?;
          if let Calc::Number(val) = rhs {
            if val != 0.0 {
              node = node * (1.0 / val);
              continue;
            }
          }
          return Err(input.new_custom_error(ParserError::InvalidValue));
        }
        _ => {
          input.reset(&start);
          break;
        }
      }
    }
    Ok(node)
  }

  fn parse_value<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Parse nested calc() and other math functions.
    if let Ok(calc) = input.try_parse(Self::parse) {
      match calc {
        Calc::Function(f) => {
          return Ok(match *f {
            MathFunction::Calc(c) => c,
            _ => Calc::Function(f),
          })
        }
        c => return Ok(c),
      }
    }

    if input.try_parse(|input| input.expect_parenthesis_block()).is_ok() {
      return input.parse_nested_block(Calc::parse_sum);
    }

    if let Ok(num) = input.try_parse(|input| input.expect_number()) {
      return Ok(Calc::Number(num));
    }

    if let Ok(constant) = input.try_parse(Constant::parse) {
      return Ok(Calc::Number(constant.into()));
    }

    if let Ok(value) = input.try_parse(V::parse) {
      return Ok(Calc::Value(Box::new(value)));
    }

    Err(input.new_error_for_next_token())
  }

  fn reduce_args(args: &mut Vec<Calc<V>>, cmp: std::cmp::Ordering) -> Vec<Calc<V>> {
    // Reduces the arguments of a min() or max() expression, combining compatible values.
    // e.g. min(1px, 1em, 2px, 3in) => min(1px, 1em)
    let mut reduced: Vec<Calc<V>> = vec![];
    for arg in args.drain(..) {
      let mut found = None;
      match &arg {
        Calc::Value(val) => {
          for b in reduced.iter_mut() {
            if let Calc::Value(v) = b {
              match val.partial_cmp(v) {
                Some(ord) if ord == cmp => {
                  found = Some(Some(b));
                  break;
                }
                Some(_) => {
                  found = Some(None);
                  break;
                }
                None => {}
              }
            }
          }
        }
        _ => {}
      }
      if let Some(r) = found {
        if let Some(r) = r {
          *r = arg
        }
      } else {
        reduced.push(arg)
      }
    }
    reduced
  }

  fn parse_trig<'t, F: FnOnce(f32) -> f32>(
    input: &mut Parser<'i, 't>,
    f: F,
    to_angle: bool,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    input.parse_nested_block(|input| {
      let v: Calc<Angle> = Calc::parse_sum(input)?;
      let rad = match v {
        Calc::Value(angle) if !to_angle => f(angle.to_radians()),
        Calc::Number(v) => f(v),
        _ => return Err(input.new_custom_error(ParserError::InvalidValue)),
      };

      if to_angle && !rad.is_nan() {
        if let Ok(v) = V::try_from(Angle::Rad(rad)) {
          return Ok(Calc::Value(Box::new(v)));
        } else {
          return Err(input.new_custom_error(ParserError::InvalidValue));
        }
      } else {
        Ok(Calc::Number(rad))
      }
    })
  }
}

impl<V: std::ops::Mul<f32, Output = V>> std::ops::Mul<f32> for Calc<V> {
  type Output = Self;

  fn mul(self, other: f32) -> Self {
    if other == 1.0 {
      return self;
    }

    match self {
      Calc::Value(v) => Calc::Value(Box::new(*v * other)),
      Calc::Number(n) => Calc::Number(n * other),
      Calc::Sum(a, b) => Calc::Sum(Box::new(*a * other), Box::new(*b * other)),
      Calc::Product(num, calc) => {
        let num = num * other;
        if num == 1.0 {
          return *calc;
        }
        Calc::Product(num, calc)
      }
      Calc::Function(f) => match *f {
        MathFunction::Calc(c) => Calc::Function(Box::new(MathFunction::Calc(c * other))),
        _ => Calc::Product(other, Box::new(Calc::Function(f))),
      },
    }
  }
}

impl<V: AddInternal + std::convert::Into<Calc<V>> + std::convert::From<Calc<V>> + std::fmt::Debug> AddInternal
  for Calc<V>
{
  fn add(self, other: Calc<V>) -> Calc<V> {
    match (self, other) {
      (Calc::Value(a), Calc::Value(b)) => (a.add(*b)).into(),
      (Calc::Number(a), Calc::Number(b)) => Calc::Number(a + b),
      (Calc::Value(a), b) => (a.add(V::from(b))).into(),
      (a, Calc::Value(b)) => (V::from(a).add(*b)).into(),
      (Calc::Function(a), b) => Calc::Sum(Box::new(Calc::Function(a)), Box::new(b)),
      (a, Calc::Function(b)) => Calc::Sum(Box::new(a), Box::new(Calc::Function(b))),
      (a, b) => V::from(a).add(V::from(b)).into(),
    }
  }
}

impl<V: std::cmp::PartialEq<f32>> std::cmp::PartialEq<f32> for Calc<V> {
  fn eq(&self, other: &f32) -> bool {
    match self {
      Calc::Value(a) => **a == *other,
      Calc::Number(a) => *a == *other,
      _ => false,
    }
  }
}

impl<V: std::cmp::PartialOrd<f32>> std::cmp::PartialOrd<f32> for Calc<V> {
  fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
    match self {
      Calc::Value(a) => a.partial_cmp(other),
      Calc::Number(a) => a.partial_cmp(other),
      _ => None,
    }
  }
}

impl<V: ToCss + std::cmp::PartialOrd<f32> + std::ops::Mul<f32, Output = V> + Clone + std::fmt::Debug> ToCss
  for Calc<V>
{
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let was_in_calc = dest.in_calc;
    dest.in_calc = true;

    let res = match self {
      Calc::Value(v) => v.to_css(dest),
      Calc::Number(n) => n.to_css(dest),
      Calc::Sum(a, b) => {
        a.to_css(dest)?;
        // Whitespace is always required.
        let b = &**b;
        if *b < 0.0 {
          dest.write_str(" - ")?;
          let b = b.clone() * -1.0;
          b.to_css(dest)
        } else {
          dest.write_str(" + ")?;
          b.to_css(dest)
        }
      }
      Calc::Product(num, calc) => {
        if num.abs() < 1.0 {
          let div = 1.0 / num;
          calc.to_css(dest)?;
          dest.delim('/', true)?;
          div.to_css(dest)
        } else {
          num.to_css(dest)?;
          dest.delim('*', true)?;
          calc.to_css(dest)
        }
      }
      Calc::Function(f) => f.to_css(dest),
    };

    dest.in_calc = was_in_calc;
    res
  }
}
