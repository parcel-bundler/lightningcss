use crate::properties::custom::Token;
use crate::rules::Location;
use crate::values::string::CowArcStr;
use cssparser::{BasicParseErrorKind, ParseError, ParseErrorKind};
use parcel_selectors::parser::SelectorParseErrorKind;
use serde::Serialize;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Error<T> {
  pub kind: T,
  pub loc: Option<ErrorLocation>,
}

impl<T: fmt::Display> fmt::Display for Error<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.kind.fmt(f)?;
    if let Some(loc) = &self.loc {
      write!(f, " at {}", loc)?;
    }
    Ok(())
  }
}

impl<T: fmt::Display + fmt::Debug> std::error::Error for Error<T> {}

#[derive(Debug, PartialEq, Clone)]
pub struct ErrorLocation {
  pub filename: String,
  pub line: u32,
  pub column: u32,
}

impl ErrorLocation {
  pub fn from(loc: Location, filename: String) -> Self {
    ErrorLocation {
      filename,
      line: loc.line,
      column: loc.column,
    }
  }
}

impl fmt::Display for ErrorLocation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}:{}", self.filename, self.line, self.column)
  }
}

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ParserError<'i> {
  AtRuleBodyInvalid,
  AtRuleInvalid(CowArcStr<'i>),
  EndOfInput,
  InvalidDeclaration,
  InvalidMediaQuery,
  InvalidNesting,
  InvalidPageSelector,
  InvalidValue,
  QualifiedRuleInvalid,
  SelectorError(SelectorError<'i>),
  UnexpectedImportRule,
  UnexpectedNamespaceRule,
  UnexpectedToken(#[serde(skip)] Token<'i>),
}

impl<'i> fmt::Display for ParserError<'i> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use ParserError::*;
    match self {
      AtRuleBodyInvalid => write!(f, "Invalid @ rule body"),
      AtRuleInvalid(name) => write!(f, "Unknown at rule: @{}", name),
      EndOfInput => write!(f, "Unexpected end of input"),
      InvalidDeclaration => write!(f, "Invalid declaration"),
      InvalidMediaQuery => write!(f, "Invalid media query"),
      InvalidNesting => write!(f, "Invalid nesting"),
      InvalidPageSelector => write!(f, "Invalid page selector"),
      InvalidValue => write!(f, "Invalid value"),
      QualifiedRuleInvalid => write!(f, "Invalid qualified rule"),
      SelectorError(s) => s.fmt(f),
      UnexpectedImportRule => write!(
        f,
        "@import rules must precede all rules aside from @charset and @layer statements"
      ),
      UnexpectedNamespaceRule => write!(
        f,
        "@namespaces rules must precede all rules aside from @charset, @import, and @layer statements"
      ),
      UnexpectedToken(token) => write!(f, "Unexpected token {:?}", token),
    }
  }
}

impl<'i> Error<ParserError<'i>> {
  pub fn from(err: ParseError<'i, ParserError<'i>>, filename: String) -> Error<ParserError<'i>> {
    let kind = match err.kind {
      ParseErrorKind::Basic(b) => match &b {
        BasicParseErrorKind::UnexpectedToken(t) => ParserError::UnexpectedToken(t.into()),
        BasicParseErrorKind::EndOfInput => ParserError::EndOfInput,
        BasicParseErrorKind::AtRuleInvalid(a) => ParserError::AtRuleInvalid(a.into()),
        BasicParseErrorKind::AtRuleBodyInvalid => ParserError::AtRuleBodyInvalid,
        BasicParseErrorKind::QualifiedRuleInvalid => ParserError::QualifiedRuleInvalid,
      },
      ParseErrorKind::Custom(c) => c,
    };

    Error {
      kind,
      loc: Some(ErrorLocation {
        filename,
        line: err.location.line,
        column: err.location.column,
      }),
    }
  }
}

impl<'i> From<SelectorParseErrorKind<'i>> for ParserError<'i> {
  fn from(err: SelectorParseErrorKind<'i>) -> ParserError<'i> {
    ParserError::SelectorError(err.into())
  }
}

impl<'i> ParserError<'i> {
  #[deprecated(note = "use `ParserError::to_string()` or `fmt::Display` instead")]
  pub fn reason(&self) -> String {
    self.to_string()
  }
}

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum SelectorError<'i> {
  BadValueInAttr(#[serde(skip)] Token<'i>),
  ClassNeedsIdent(#[serde(skip)] Token<'i>),
  DanglingCombinator,
  EmptySelector,
  ExpectedBarInAttr(#[serde(skip)] Token<'i>),
  ExpectedNamespace(CowArcStr<'i>),
  ExplicitNamespaceUnexpectedToken(#[serde(skip)] Token<'i>),
  InvalidPseudoClassAfterPseudoElement,
  InvalidPseudoClassAfterWebKitScrollbar,
  InvalidPseudoClassBeforeWebKitScrollbar,
  InvalidQualNameInAttr(#[serde(skip)] Token<'i>),
  InvalidState,
  MissingNestingPrefix,
  MissingNestingSelector,
  NoQualifiedNameInAttributeSelector(#[serde(skip)] Token<'i>),
  PseudoElementExpectedIdent(#[serde(skip)] Token<'i>),
  UnexpectedIdent(CowArcStr<'i>),
  UnexpectedTokenInAttributeSelector(#[serde(skip)] Token<'i>),
  UnsupportedPseudoClassOrElement(CowArcStr<'i>),
}

impl<'i> fmt::Display for SelectorError<'i> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use SelectorError::*;
    match self {
      InvalidState => write!(f, "Invalid state"),
      BadValueInAttr(token) => write!(f, "Invalid value in attribute selector: {:?}", token),
      ClassNeedsIdent(token) => write!(f, "Expected identifier in class selector, got {:?}", token),
      DanglingCombinator => write!(f, "Invalid dangling combinator in selector"),
      EmptySelector => write!(f, "Invalid empty selector"),
      ExpectedBarInAttr(name) => write!(f, "Expected | in attribute, got {:?}", name),
      ExpectedNamespace(name) => write!(f, "Expected namespace: {}", name),
      ExplicitNamespaceUnexpectedToken(token) => write!(f, "Unexpected token in namespace selector: {:?}", token),
      InvalidPseudoClassAfterPseudoElement => write!(f, "Invalid pseudo class after pseudo element, only user action pseudo classes (e.g. :hover, :active) are allowed"),
      InvalidPseudoClassAfterWebKitScrollbar => write!(f, "Invalid pseudo class after ::-webkit-scrollbar pseudo element"),
      InvalidPseudoClassBeforeWebKitScrollbar => write!(f, "Pseudo class must be prefixed by a ::-webkit-scrollbar pseudo element"),
      InvalidQualNameInAttr(token) => write!(f, "Invalid qualified name in attribute selector: {:?}", token),
      MissingNestingPrefix => write!(f, "A nesting selector (&) is required as a prefix of each selector in a nested style rule"),
      MissingNestingSelector => write!(f, "A nesting selector (&) is required in each selector of a @nest rule"),
      NoQualifiedNameInAttributeSelector(token) => write!(f, "No qualified name in attribute selector: {:?}.", token),
      PseudoElementExpectedIdent(token) => write!(f, "Invalid token in pseudo element: {:?}", token),
      UnexpectedIdent(name) => write!(f, "Unexpected identifier: {}", name),
      UnexpectedTokenInAttributeSelector(token) => write!(f, "Unexpected token in attribute selector: {:?}", token),
      UnsupportedPseudoClassOrElement(name) => write!(f, "Unsupported pseudo class or element: {}", name),
    }
  }
}

impl<'i> From<SelectorParseErrorKind<'i>> for SelectorError<'i> {
  fn from(err: SelectorParseErrorKind<'i>) -> Self {
    match &err {
      SelectorParseErrorKind::NoQualifiedNameInAttributeSelector(t) => {
        SelectorError::NoQualifiedNameInAttributeSelector(t.into())
      }
      SelectorParseErrorKind::EmptySelector => SelectorError::EmptySelector,
      SelectorParseErrorKind::DanglingCombinator => SelectorError::DanglingCombinator,
      SelectorParseErrorKind::InvalidPseudoClassBeforeWebKitScrollbar => {
        SelectorError::InvalidPseudoClassBeforeWebKitScrollbar
      }
      SelectorParseErrorKind::InvalidPseudoClassAfterWebKitScrollbar => {
        SelectorError::InvalidPseudoClassAfterWebKitScrollbar
      }
      SelectorParseErrorKind::InvalidPseudoClassAfterPseudoElement => {
        SelectorError::InvalidPseudoClassAfterPseudoElement
      }
      SelectorParseErrorKind::InvalidState => SelectorError::InvalidState,
      SelectorParseErrorKind::MissingNestingSelector => SelectorError::MissingNestingSelector,
      SelectorParseErrorKind::MissingNestingPrefix => SelectorError::MissingNestingPrefix,
      SelectorParseErrorKind::UnexpectedTokenInAttributeSelector(t) => {
        SelectorError::UnexpectedTokenInAttributeSelector(t.into())
      }
      SelectorParseErrorKind::PseudoElementExpectedIdent(t) => SelectorError::PseudoElementExpectedIdent(t.into()),
      SelectorParseErrorKind::UnsupportedPseudoClassOrElement(t) => {
        SelectorError::UnsupportedPseudoClassOrElement(t.into())
      }
      SelectorParseErrorKind::UnexpectedIdent(t) => SelectorError::UnexpectedIdent(t.into()),
      SelectorParseErrorKind::ExpectedNamespace(t) => SelectorError::ExpectedNamespace(t.into()),
      SelectorParseErrorKind::ExpectedBarInAttr(t) => SelectorError::ExpectedBarInAttr(t.into()),
      SelectorParseErrorKind::BadValueInAttr(t) => SelectorError::BadValueInAttr(t.into()),
      SelectorParseErrorKind::InvalidQualNameInAttr(t) => SelectorError::InvalidQualNameInAttr(t.into()),
      SelectorParseErrorKind::ExplicitNamespaceUnexpectedToken(t) => {
        SelectorError::ExplicitNamespaceUnexpectedToken(t.into())
      }
      SelectorParseErrorKind::ClassNeedsIdent(t) => SelectorError::ClassNeedsIdent(t.into()),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct ErrorWithLocation<T> {
  pub kind: T,
  pub loc: Location,
}

impl<T: fmt::Display> fmt::Display for ErrorWithLocation<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.kind.fmt(f)
  }
}

impl<T: fmt::Display + fmt::Debug> std::error::Error for ErrorWithLocation<T> {}

pub type MinifyError = ErrorWithLocation<MinifyErrorKind>;

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type")]
pub enum MinifyErrorKind {
  CircularCustomMedia { name: String },
  CustomMediaNotDefined { name: String },
  UnsupportedCustomMediaBooleanLogic { custom_media_loc: Location },
}

impl fmt::Display for MinifyErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use MinifyErrorKind::*;
    match self {
      CircularCustomMedia { name } => write!(f, "Circular custom media query {} detected", name),
      CustomMediaNotDefined { name } => write!(f, "Custom media query {} is not defined", name),
      UnsupportedCustomMediaBooleanLogic { .. } => write!(
        f,
        "Boolean logic with media types in @custom-media rules is not supported by Parcel CSS"
      ),
    }
  }
}

impl MinifyErrorKind {
  #[deprecated(note = "use `MinifyErrorKind::to_string()` or `fmt::Display` instead")]
  pub fn reason(&self) -> String {
    self.to_string()
  }
}

pub type PrinterError = Error<PrinterErrorKind>;

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type")]
pub enum PrinterErrorKind {
  AmbiguousUrlInCustomProperty { url: String },
  FmtError,
  InvalidComposesNesting,
  InvalidComposesSelector,
}

impl From<fmt::Error> for PrinterError {
  fn from(_: fmt::Error) -> PrinterError {
    PrinterError {
      kind: PrinterErrorKind::FmtError,
      loc: None,
    }
  }
}

impl fmt::Display for PrinterErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use PrinterErrorKind::*;
    match self {
      AmbiguousUrlInCustomProperty { url } => write!(f, "Ambiguous url('{}') in custom property. Relative paths are resolved from the location the var() is used, not where the custom property is defined. Use an absolute URL instead", url),
      FmtError => write!(f, "Printer error"),
      InvalidComposesNesting => write!(f, "The `composes` property cannot be used within nested rules"),
      InvalidComposesSelector => write!(f, "The `composes` property cannot be used with a simple class selector"),
    }
  }
}

impl PrinterErrorKind {
  #[deprecated(note = "use `PrinterErrorKind::to_string()` or `fmt::Display` instead")]
  pub fn reason(&self) -> String {
    self.to_string()
  }
}
