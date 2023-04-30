//! Generic values for two component properties.

use crate::error::{ParserError, PrinterError};
use crate::printer::Printer;
use crate::traits::{IsCompatible, Parse, ToCss};
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

/// A generic value that represents a value with two components, e.g. a border radius.
///
/// When serialized, only a single component will be written if both are equal.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Size2D<T>(pub T, pub T);

impl<'i, T> Parse<'i> for Size2D<T>
where
  T: Parse<'i> + Clone,
{
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let first = T::parse(input)?;
    let second = input.try_parse(T::parse).unwrap_or_else(|_| first.clone());
    Ok(Size2D(first, second))
  }
}

impl<T> ToCss for Size2D<T>
where
  T: ToCss + PartialEq,
{
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.0.to_css(dest)?;
    if self.1 != self.0 {
      dest.write_str(" ")?;
      self.1.to_css(dest)?;
    }
    Ok(())
  }
}

impl<T: IsCompatible> IsCompatible for Size2D<T> {
  fn is_compatible(&self, browsers: crate::targets::Browsers) -> bool {
    self.0.is_compatible(browsers) && self.1.is_compatible(browsers)
  }
}
