use cssparser::*;

pub trait Parse: Sized {
  /// Parse a value of this type.
  ///
  /// Returns an error on failure.
  fn parse<'i, 't>(
      input: &mut Parser<'i, 't>,
  ) -> Result<Self, ParseError<'i, ()>>;
}
