use parcel_selectors::parser::SelectorParseErrorKind;
use cssparser::{ParseError, ParseErrorKind, BasicParseErrorKind};
use crate::rules::Location;
use crate::properties::custom::Token;
use crate::values::string::CowArcStr;

#[derive(Debug)]
pub struct Error<T> {
  pub kind: T,
  pub loc: Location
}

#[derive(Debug)]
pub enum ParserError<'i> {
  /// An unexpected token was encountered.
  UnexpectedToken(Token<'i>),
  /// The end of the input was encountered unexpectedly.
  EndOfInput,
  /// An `@` rule was encountered that was invalid.
  AtRuleInvalid(CowArcStr<'i>),
  /// The body of an '@' rule was invalid.
  AtRuleBodyInvalid,
  /// A qualified rule was encountered that was invalid.
  QualifiedRuleInvalid,
  SelectorError(SelectorError<'i>),
  InvalidDeclaration,
  InvalidPageSelector,
  InvalidValue,
  InvalidMediaQuery,
  InvalidNesting
}

impl<'i> Error<ParserError<'i>> {
  pub fn from(err: ParseError<'i, ParserError<'i>>, source_index: u32) -> Error<ParserError<'i>> {
    let kind = match err.kind {
      ParseErrorKind::Basic(b) => {
        match &b {
          BasicParseErrorKind::UnexpectedToken(t) => ParserError::UnexpectedToken(t.into()),
          BasicParseErrorKind::EndOfInput => ParserError::EndOfInput,
          BasicParseErrorKind::AtRuleInvalid(a) => ParserError::AtRuleInvalid(a.into()),
          BasicParseErrorKind::AtRuleBodyInvalid => ParserError::AtRuleBodyInvalid,
          BasicParseErrorKind::QualifiedRuleInvalid => ParserError::QualifiedRuleInvalid,
        }
      }
      ParseErrorKind::Custom(c) => c
    };

    Error {
      kind,
      loc: Location {
        source_index,
        line: err.location.line,
        column: err.location.column
      }
    }
  }
}

impl<'i> From<ParseError<'i, ParserError<'i>>> for Error<ParserError<'i>> {
  fn from(err: ParseError<'i, ParserError<'i>>) -> Error<ParserError<'i>> {
    Self::from(err, 0)
  }
}

impl<'i> From<SelectorParseErrorKind<'i>> for ParserError<'i> {
  fn from(err: SelectorParseErrorKind<'i>) -> ParserError<'i> {
    ParserError::SelectorError(err.into())
  }
}

impl<'i> ParserError<'i> {
  pub fn reason(&self) -> String {
    match self {
      ParserError::AtRuleBodyInvalid => "Invalid at rule body".into(),
      ParserError::EndOfInput => "Unexpected end of input".into(),
      ParserError::AtRuleInvalid(name) => format!("Unknown at rule: @{}", name),
      ParserError::QualifiedRuleInvalid => "Invalid qualified rule".into(),
      ParserError::UnexpectedToken(token) => format!("Unexpected token {:?}", token),
      ParserError::InvalidDeclaration => "Invalid declaration".into(),
      ParserError::InvalidMediaQuery => "Invalid media query".into(),
      ParserError::InvalidNesting => "Invalid nesting".into(),
      ParserError::InvalidPageSelector => "Invalid page selector".into(),
      ParserError::InvalidValue => "Invalid value".into(),
      ParserError::SelectorError(s) => s.reason()
    }
  }
}

#[derive(Debug)]
pub enum SelectorError<'i> {
  NoQualifiedNameInAttributeSelector(Token<'i>),
  EmptySelector,
  DanglingCombinator,
  NonCompoundSelector,
  NonPseudoElementAfterSlotted,
  InvalidPseudoElementAfterSlotted,
  InvalidPseudoElementInsideWhere,
  InvalidState,
  MissingNestingSelector,
  MissingNestingPrefix,
  UnexpectedTokenInAttributeSelector(Token<'i>),
  PseudoElementExpectedColon(Token<'i>),
  PseudoElementExpectedIdent(Token<'i>),
  NoIdentForPseudo(Token<'i>),
  UnsupportedPseudoClassOrElement(CowArcStr<'i>),
  UnexpectedIdent(CowArcStr<'i>),
  ExpectedNamespace(CowArcStr<'i>),
  ExpectedBarInAttr(Token<'i>),
  BadValueInAttr(Token<'i>),
  InvalidQualNameInAttr(Token<'i>),
  ExplicitNamespaceUnexpectedToken(Token<'i>),
  ClassNeedsIdent(Token<'i>),
}

impl<'i> From<SelectorParseErrorKind<'i>> for SelectorError<'i> {
  fn from(err: SelectorParseErrorKind<'i>) -> Self {
    match &err {
      SelectorParseErrorKind::NoQualifiedNameInAttributeSelector(t) => SelectorError::NoQualifiedNameInAttributeSelector(t.into()),
      SelectorParseErrorKind::EmptySelector => SelectorError::EmptySelector,
      SelectorParseErrorKind::DanglingCombinator => SelectorError::DanglingCombinator,
      SelectorParseErrorKind::NonCompoundSelector => SelectorError::NonCompoundSelector,
      SelectorParseErrorKind::NonPseudoElementAfterSlotted => SelectorError::NonPseudoElementAfterSlotted,
      SelectorParseErrorKind::InvalidPseudoElementAfterSlotted => SelectorError::InvalidPseudoElementAfterSlotted,
      SelectorParseErrorKind::InvalidPseudoElementInsideWhere => SelectorError::InvalidPseudoElementInsideWhere,
      SelectorParseErrorKind::InvalidState => SelectorError::InvalidState,
      SelectorParseErrorKind::MissingNestingSelector => SelectorError::MissingNestingSelector,
      SelectorParseErrorKind::MissingNestingPrefix => SelectorError::MissingNestingPrefix,
      SelectorParseErrorKind::UnexpectedTokenInAttributeSelector(t) => SelectorError::UnexpectedTokenInAttributeSelector(t.into()),
      SelectorParseErrorKind::PseudoElementExpectedColon(t) => SelectorError::PseudoElementExpectedColon(t.into()),
      SelectorParseErrorKind::PseudoElementExpectedIdent(t) => SelectorError::PseudoElementExpectedIdent(t.into()),
      SelectorParseErrorKind::NoIdentForPseudo(t) => SelectorError::NoIdentForPseudo(t.into()),
      SelectorParseErrorKind::UnsupportedPseudoClassOrElement(t) => SelectorError::UnsupportedPseudoClassOrElement(t.into()),
      SelectorParseErrorKind::UnexpectedIdent(t) => SelectorError::UnexpectedIdent(t.into()),
      SelectorParseErrorKind::ExpectedNamespace(t) => SelectorError::ExpectedNamespace(t.into()),
      SelectorParseErrorKind::ExpectedBarInAttr(t) => SelectorError::ExpectedBarInAttr(t.into()),
      SelectorParseErrorKind::BadValueInAttr(t) => SelectorError::BadValueInAttr(t.into()),
      SelectorParseErrorKind::InvalidQualNameInAttr(t) => SelectorError::InvalidQualNameInAttr(t.into()),
      SelectorParseErrorKind::ExplicitNamespaceUnexpectedToken(t) => SelectorError::ExplicitNamespaceUnexpectedToken(t.into()),
      SelectorParseErrorKind::ClassNeedsIdent(t) => SelectorError::ClassNeedsIdent(t.into()),
    }
  }
}

impl<'i> SelectorError<'i> {
  fn reason(&self) -> String {
    use SelectorError::*;
    match self {
      NoQualifiedNameInAttributeSelector(token) => format!("No qualified name in attribute selector: {:?}.", token),
      EmptySelector => "Invalid empty selector.".into(),
      DanglingCombinator => "Invalid dangling combinator in selector.".into(),
      MissingNestingSelector => "A nesting selector (&) is required in each selector of a @nest rule.".into(),
      MissingNestingPrefix => "A nesting selector (&) is required as a prefix of each selector in a nested style rule.".into(),
      UnexpectedTokenInAttributeSelector(token) => format!("Unexpected token in attribute selector: {:?}", token),
      PseudoElementExpectedIdent(token) => format!("Invalid token in pseudo element: {:?}", token),
      UnsupportedPseudoClassOrElement(name) => format!("Unsupported pseudo class or element: {}", name),
      UnexpectedIdent(name) => format!("Unexpected identifier: {}", name),
      ExpectedNamespace(name) => format!("Expected namespace: {}", name),
      ExpectedBarInAttr(name) => format!("Expected | in attribute, got {:?}", name),
      BadValueInAttr(token) => format!("Invalid value in attribute selector: {:?}", token),
      InvalidQualNameInAttr(token) => format!("Invalid qualified name in attribute selector: {:?}", token),
      ExplicitNamespaceUnexpectedToken(token) => format!("Unexpected token in namespace selector: {:?}", token),
      ClassNeedsIdent(token) => format!("Expected identifier in class selector, got {:?}", token),
      err => format!("Error parsing selector: {:?}", err)
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum MinifyError {
  UnsupportedCustomMediaBooleanLogic {
    media_loc: Location,
    custom_media_loc: Location
  },
  CustomMediaNotDefined {
    name: String,
    loc: Location
  },
  CircularCustomMedia {
    name: String,
    loc: Location
  }
}

impl MinifyError {
  pub fn reason(&self) -> String {
    match self {
      MinifyError::UnsupportedCustomMediaBooleanLogic {..} => "Boolean logic with media types in @custom-media rules is not supported by Parcel CSS.".into(),
      MinifyError::CustomMediaNotDefined { name, .. } => format!("Custom media query {} is not defined.", name),
      MinifyError::CircularCustomMedia { name, .. } => format!("Circular custom media query {} detected.", name)
    }
  }

  pub fn loc(&self) -> Location {
    match self {
      MinifyError::UnsupportedCustomMediaBooleanLogic { media_loc, .. } => *media_loc,
      MinifyError::CustomMediaNotDefined { loc, .. } => *loc,
      MinifyError::CircularCustomMedia { loc, .. } => *loc
    }
  }
}


#[derive(Debug)]
pub enum PrinterError {
  FmtError,
  InvalidComposesSelector(Location),
  InvalidComposesNesting(Location)
}

impl From<std::fmt::Error> for PrinterError {
  fn from(_: std::fmt::Error) -> PrinterError {
    PrinterError::FmtError
  }
}

impl PrinterError {
  pub fn reason(&self) -> String {
    match self {
      PrinterError::InvalidComposesSelector(_) => "The `composes` property can only be used within a simple class selector.".into(),
      PrinterError::InvalidComposesNesting(_) => "The `composes` property cannot be used within nested rules.".into(),
      PrinterError::FmtError => "Printer error".into()
    }
  }
}
