use parcel_selectors::parser::SelectorParseErrorKind;

#[derive(Debug)]
pub enum ParserError<'i> {
  SelectorError(SelectorParseErrorKind<'i>),
  InvalidDeclaration,
  InvalidPageSelector,
  InvalidValue,
  InvalidMediaQuery,
  InvalidNesting
}

impl<'i> From<SelectorParseErrorKind<'i>> for ParserError<'i> {
  fn from(err: SelectorParseErrorKind<'i>) -> ParserError<'i> {
    ParserError::SelectorError(err)
  }
}

pub enum PrinterError {

}
