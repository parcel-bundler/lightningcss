use crate::properties::custom::Token;
use crate::rules::Location;
use crate::values::string::CowArcStr;
use cssparser::{BasicParseErrorKind, ParseError, ParseErrorKind};
use parcel_selectors::parser::SelectorParseErrorKind;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone)]
pub struct Error<T> {
  pub kind: T,
  pub loc: Option<ErrorLocation>,
}

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

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum ParserError<'i> {
  /// An unexpected token was encountered.
  UnexpectedToken(#[serde(skip)] Token<'i>),
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
  InvalidNesting,
  UnexpectedImportRule,
  UnexpectedNamespaceRule,
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
      ParserError::UnexpectedImportRule => {
        "@import rules must precede all rules aside from @charset and @layer statements".into()
      }
      ParserError::UnexpectedNamespaceRule => {
        "@namespaces rules must precede all rules aside from @charset, @import, and @layer statements".into()
      }
      ParserError::SelectorError(s) => s.reason(),
    }
  }
}

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum SelectorError<'i> {
  NoQualifiedNameInAttributeSelector(#[serde(skip)] Token<'i>),
  EmptySelector,
  DanglingCombinator,
  NonCompoundSelector,
  NonPseudoElementAfterSlotted,
  InvalidPseudoElementAfterSlotted,
  InvalidPseudoElementInsideWhere,
  InvalidPseudoClassBeforeWebKitScrollbar,
  InvalidPseudoClassAfterWebKitScrollbar,
  InvalidPseudoClassAfterPseudoElement,
  InvalidState,
  MissingNestingSelector,
  MissingNestingPrefix,
  UnexpectedTokenInAttributeSelector(#[serde(skip)] Token<'i>),
  PseudoElementExpectedColon(#[serde(skip)] Token<'i>),
  PseudoElementExpectedIdent(#[serde(skip)] Token<'i>),
  NoIdentForPseudo(#[serde(skip)] Token<'i>),
  UnsupportedPseudoClassOrElement(CowArcStr<'i>),
  UnexpectedIdent(CowArcStr<'i>),
  ExpectedNamespace(CowArcStr<'i>),
  ExpectedBarInAttr(#[serde(skip)] Token<'i>),
  BadValueInAttr(#[serde(skip)] Token<'i>),
  InvalidQualNameInAttr(#[serde(skip)] Token<'i>),
  ExplicitNamespaceUnexpectedToken(#[serde(skip)] Token<'i>),
  ClassNeedsIdent(#[serde(skip)] Token<'i>),
}

impl<'i> From<SelectorParseErrorKind<'i>> for SelectorError<'i> {
  fn from(err: SelectorParseErrorKind<'i>) -> Self {
    match &err {
      SelectorParseErrorKind::NoQualifiedNameInAttributeSelector(t) => {
        SelectorError::NoQualifiedNameInAttributeSelector(t.into())
      }
      SelectorParseErrorKind::EmptySelector => SelectorError::EmptySelector,
      SelectorParseErrorKind::DanglingCombinator => SelectorError::DanglingCombinator,
      SelectorParseErrorKind::NonCompoundSelector => SelectorError::NonCompoundSelector,
      SelectorParseErrorKind::NonPseudoElementAfterSlotted => SelectorError::NonPseudoElementAfterSlotted,
      SelectorParseErrorKind::InvalidPseudoElementAfterSlotted => SelectorError::InvalidPseudoElementAfterSlotted,
      SelectorParseErrorKind::InvalidPseudoElementInsideWhere => SelectorError::InvalidPseudoElementInsideWhere,
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
      SelectorParseErrorKind::PseudoElementExpectedColon(t) => SelectorError::PseudoElementExpectedColon(t.into()),
      SelectorParseErrorKind::PseudoElementExpectedIdent(t) => SelectorError::PseudoElementExpectedIdent(t.into()),
      SelectorParseErrorKind::NoIdentForPseudo(t) => SelectorError::NoIdentForPseudo(t.into()),
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
      InvalidPseudoClassBeforeWebKitScrollbar => "Pseudo class must be prefixed by a ::-webkit-scrollbar pseudo element".into(),
      InvalidPseudoClassAfterWebKitScrollbar => "Invalid pseudo class after ::-webkit-scrollbar pseudo element".into(),
      InvalidPseudoClassAfterPseudoElement => "Invalid pseudo class after pseudo element. Only user action pseudo classes (e.g. :hover, :active) are allowed.".into(),
      err => format!("Error parsing selector: {:?}", err)
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct ErrorWithLocation<T> {
  pub kind: T,
  pub loc: Location,
}

pub type MinifyError = ErrorWithLocation<MinifyErrorKind>;

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type")]
pub enum MinifyErrorKind {
  UnsupportedCustomMediaBooleanLogic { custom_media_loc: Location },
  CustomMediaNotDefined { name: String },
  CircularCustomMedia { name: String },
}

impl MinifyErrorKind {
  pub fn reason(&self) -> String {
    match self {
      MinifyErrorKind::UnsupportedCustomMediaBooleanLogic { .. } => {
        "Boolean logic with media types in @custom-media rules is not supported by Parcel CSS.".into()
      }
      MinifyErrorKind::CustomMediaNotDefined { name, .. } => {
        format!("Custom media query {} is not defined.", name)
      }
      MinifyErrorKind::CircularCustomMedia { name, .. } => {
        format!("Circular custom media query {} detected.", name)
      }
    }
  }
}

pub type PrinterError = Error<PrinterErrorKind>;

#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "type")]
pub enum PrinterErrorKind {
  FmtError,
  InvalidComposesSelector,
  InvalidComposesNesting,
  AmbiguousUrlInCustomProperty { url: String },
}

impl From<std::fmt::Error> for PrinterError {
  fn from(_: std::fmt::Error) -> PrinterError {
    PrinterError {
      kind: PrinterErrorKind::FmtError,
      loc: None,
    }
  }
}

impl PrinterErrorKind {
  pub fn reason(&self) -> String {
    match self {
      PrinterErrorKind::InvalidComposesSelector => "The `composes` property can only be used within a simple class selector.".into(),
      PrinterErrorKind::InvalidComposesNesting => "The `composes` property cannot be used within nested rules.".into(),
      PrinterErrorKind::FmtError => "Printer error".into(),
      PrinterErrorKind::AmbiguousUrlInCustomProperty { .. } => "Ambiguous url() in custom property. Relative paths are resolved from the location the var() is used, not where the custom property is defined. Use an absolute URL instead.".into()
    }
  }
}
