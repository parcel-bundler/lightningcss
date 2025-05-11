//! Error types.

use crate::properties::custom::Token;
use crate::rules::Location;
use crate::values::string::CowArcStr;
use cssparser::{BasicParseErrorKind, ParseError, ParseErrorKind};
use parcel_selectors::parser::SelectorParseErrorKind;
#[cfg(any(feature = "serde", feature = "nodejs"))]
use serde::Serialize;
#[cfg(feature = "into_owned")]
use static_self::IntoOwned;
use std::fmt;

/// An error with a source location.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(serde::Serialize))]
#[cfg_attr(any(feature = "serde"), derive(serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Error<T> {
  /// The type of error that occurred.
  pub kind: T,
  /// The location where the error occurred.
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

/// A line and column location within a source file.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(serde::Serialize))]
#[cfg_attr(any(feature = "serde"), derive(serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ErrorLocation {
  /// The filename in which the error occurred.
  pub filename: String,
  /// The line number, starting from 0.
  pub line: u32,
  /// The column number, starting from 1.
  pub column: u32,
}

impl ErrorLocation {
  /// Create a new error location from a source location and filename.
  pub fn new(loc: Location, filename: String) -> Self {
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

/// A parser error.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(tag = "type", content = "value"))]
pub enum ParserError<'i> {
  /// An at rule body was invalid.
  AtRuleBodyInvalid,
  /// An at rule prelude was invalid
  AtRulePreludeInvalid,
  /// An unknown or unsupported at rule was encountered.
  AtRuleInvalid(CowArcStr<'i>),
  /// Unexpectedly encountered the end of input data.
  EndOfInput,
  /// A declaration was invalid.
  InvalidDeclaration,
  /// A media query was invalid.
  InvalidMediaQuery,
  /// Invalid CSS nesting.
  InvalidNesting,
  /// The @nest rule is deprecated.
  DeprecatedNestRule,
  /// The @value rule (of CSS modules) is deprecated.
  DeprecatedCssModulesValueRule,
  /// An invalid selector in an `@page` rule.
  InvalidPageSelector,
  /// An invalid value was encountered.
  InvalidValue,
  /// Invalid qualified rule.
  QualifiedRuleInvalid,
  /// A selector was invalid.
  SelectorError(SelectorError<'i>),
  /// An `@import` rule was encountered after any rule besides `@charset` or `@layer`.
  UnexpectedImportRule,
  /// A `@namespace` rule was encountered after any rules besides `@charset`, `@import`, or `@layer`.
  UnexpectedNamespaceRule,
  /// An unexpected token was encountered.
  UnexpectedToken(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// Maximum nesting depth was reached.
  MaximumNestingDepth,
}

impl<'i> fmt::Display for ParserError<'i> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use ParserError::*;
    match self {
      AtRuleBodyInvalid => write!(f, "Invalid @ rule body"),
      AtRulePreludeInvalid => write!(f, "Invalid @ rule prelude"),
      AtRuleInvalid(name) => write!(f, "Unknown at rule: @{}", name),
      EndOfInput => write!(f, "Unexpected end of input"),
      InvalidDeclaration => write!(f, "Invalid declaration"),
      InvalidMediaQuery => write!(f, "Invalid media query"),
      InvalidNesting => write!(f, "Invalid nesting"),
      DeprecatedNestRule => write!(f, "The @nest rule is deprecated"),
      DeprecatedCssModulesValueRule => write!(f, "The @value rule is deprecated"),
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
      MaximumNestingDepth => write!(f, "Overflowed the maximum nesting depth"),
    }
  }
}

impl<'i> Error<ParserError<'i>> {
  /// Creates an error from a cssparser error.
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

  /// Consumes the value and returns an owned clone.
  #[cfg(feature = "into_owned")]
  pub fn into_owned<'x>(self) -> Error<ParserError<'static>> {
    Error {
      kind: self.kind.into_owned(),
      loc: self.loc,
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
  #[allow(missing_docs)]
  pub fn reason(&self) -> String {
    self.to_string()
  }
}

/// A selector parsing error.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(tag = "type", content = "value"))]
pub enum SelectorError<'i> {
  /// An unexpected token was found in an attribute selector.
  BadValueInAttr(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// An unexpected token was found in a class selector.
  ClassNeedsIdent(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// A dangling combinator was found.
  DanglingCombinator,
  /// An empty selector.
  EmptySelector,
  /// A `|` was expected in an attribute selector.
  ExpectedBarInAttr(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// A namespace was expected.
  ExpectedNamespace(CowArcStr<'i>),
  /// An unexpected token was encountered in a namespace.
  ExplicitNamespaceUnexpectedToken(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// An invalid pseudo class was encountered after a pseudo element.
  InvalidPseudoClassAfterPseudoElement,
  /// An invalid pseudo class was encountered after a `-webkit-scrollbar` pseudo element.
  InvalidPseudoClassAfterWebKitScrollbar,
  /// A `-webkit-scrollbar` state was encountered before a `-webkit-scrollbar` pseudo element.
  InvalidPseudoClassBeforeWebKitScrollbar,
  /// Invalid qualified name in attribute selector.
  InvalidQualNameInAttr(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// The current token is not allowed in this state.
  InvalidState,
  /// The selector is required to have the `&` nesting selector at the start.
  MissingNestingPrefix,
  /// The selector is missing a `&` nesting selector.
  MissingNestingSelector,
  /// No qualified name in attribute selector.
  NoQualifiedNameInAttributeSelector(
    #[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>,
  ),
  /// An Invalid token was encountered in a pseudo element.
  PseudoElementExpectedIdent(#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>),
  /// An unexpected identifier was encountered.
  UnexpectedIdent(CowArcStr<'i>),
  /// An unexpected token was encountered inside an attribute selector.
  UnexpectedTokenInAttributeSelector(
    #[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>,
  ),

  /// An unsupported pseudo class was encountered.
  UnsupportedPseudoClass(CowArcStr<'i>),

  /// An unsupported pseudo element was encountered.
  UnsupportedPseudoElement(CowArcStr<'i>),

  /// Ambiguous CSS module class.
  AmbiguousCssModuleClass(CowArcStr<'i>),

  /// An unexpected token was encountered after a pseudo element.
  UnexpectedSelectorAfterPseudoElement(
    #[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(skip))] Token<'i>,
  ),
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
      MissingNestingPrefix => write!(f, "A nested rule must start with a nesting selector (&) as prefix of each selector, or start with @nest"),
      MissingNestingSelector => write!(f, "A nesting selector (&) is required in each selector of a @nest rule"),
      NoQualifiedNameInAttributeSelector(token) => write!(f, "No qualified name in attribute selector: {:?}.", token),
      PseudoElementExpectedIdent(token) => write!(f, "Invalid token in pseudo element: {:?}", token),
      UnexpectedIdent(name) => write!(f, "Unexpected identifier: {}", name),
      UnexpectedTokenInAttributeSelector(token) => write!(f, "Unexpected token in attribute selector: {:?}", token),
      UnsupportedPseudoClass(name) =>write!(f, "'{name}' is not recognized as a valid pseudo-class. Did you mean '::{name}' (pseudo-element) or is this a typo?"),
      UnsupportedPseudoElement(name) => write!(f, "'{name}' is not recognized as a valid pseudo-element. Did you mean ':{name}' (pseudo-class) or is this a typo?"),
      AmbiguousCssModuleClass(_) => write!(f, "Ambiguous CSS module class not supported"),
      UnexpectedSelectorAfterPseudoElement(token) => {
        write!(
          f,
          "Pseudo-elements like '::before' or '::after' can't be followed by selectors like '{token:?}'"
        )
      },
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
      SelectorParseErrorKind::UnsupportedPseudoClass(t) => SelectorError::UnsupportedPseudoClass(t.into()),
      SelectorParseErrorKind::UnsupportedPseudoElement(t) => SelectorError::UnsupportedPseudoElement(t.into()),
      SelectorParseErrorKind::UnexpectedIdent(t) => SelectorError::UnexpectedIdent(t.into()),
      SelectorParseErrorKind::ExpectedNamespace(t) => SelectorError::ExpectedNamespace(t.into()),
      SelectorParseErrorKind::ExpectedBarInAttr(t) => SelectorError::ExpectedBarInAttr(t.into()),
      SelectorParseErrorKind::BadValueInAttr(t) => SelectorError::BadValueInAttr(t.into()),
      SelectorParseErrorKind::InvalidQualNameInAttr(t) => SelectorError::InvalidQualNameInAttr(t.into()),
      SelectorParseErrorKind::ExplicitNamespaceUnexpectedToken(t) => {
        SelectorError::ExplicitNamespaceUnexpectedToken(t.into())
      }
      SelectorParseErrorKind::ClassNeedsIdent(t) => SelectorError::ClassNeedsIdent(t.into()),
      SelectorParseErrorKind::AmbiguousCssModuleClass(name) => SelectorError::AmbiguousCssModuleClass(name.into()),
      SelectorParseErrorKind::UnexpectedSelectorAfterPseudoElement(t) => {
        SelectorError::UnexpectedSelectorAfterPseudoElement(t.into())
      }
    }
  }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ErrorWithLocation<T> {
  pub kind: T,
  pub loc: Location,
}

impl<T: fmt::Display> fmt::Display for ErrorWithLocation<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.kind.fmt(f)
  }
}

impl<T: fmt::Display + fmt::Debug> std::error::Error for ErrorWithLocation<T> {}

pub(crate) type MinifyError = ErrorWithLocation<MinifyErrorKind>;

/// A transformation error.
#[derive(Debug, PartialEq)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(tag = "type"))]
pub enum MinifyErrorKind {
  /// A circular `@custom-media` rule was detected.
  CircularCustomMedia {
    /// The name of the `@custom-media` rule that was referenced circularly.
    name: String,
  },
  /// Attempted to reference a custom media rule that doesn't exist.
  CustomMediaNotDefined {
    /// The name of the `@custom-media` rule that was not defined.
    name: String,
  },
  /// Boolean logic with media types in @custom-media rules is not supported.
  UnsupportedCustomMediaBooleanLogic {
    /// The source location of the `@custom-media` rule with unsupported boolean logic.
    custom_media_loc: Location,
  },
  /// A CSS module selector did not contain at least one class or id selector.
  ImpureCSSModuleSelector,
}

impl fmt::Display for MinifyErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use MinifyErrorKind::*;
    match self {
      CircularCustomMedia { name } => write!(f, "Circular custom media query {} detected", name),
      CustomMediaNotDefined { name } => write!(f, "Custom media query {} is not defined", name),
      UnsupportedCustomMediaBooleanLogic { .. } => write!(
        f,
        "Boolean logic with media types in @custom-media rules is not supported by Lightning CSS"
      ),
      ImpureCSSModuleSelector => write!(
        f,
        "A selector in CSS modules should contain at least one class or ID selector"
      ),
    }
  }
}

impl MinifyErrorKind {
  #[deprecated(note = "use `MinifyErrorKind::to_string()` or `fmt::Display` instead")]
  #[allow(missing_docs)]
  pub fn reason(&self) -> String {
    self.to_string()
  }
}

/// A printer error.
pub type PrinterError = Error<PrinterErrorKind>;

/// A printer error type.
#[derive(Debug, PartialEq)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(tag = "type"))]
pub enum PrinterErrorKind {
  /// An ambiguous relative `url()` was encountered in a custom property declaration.
  AmbiguousUrlInCustomProperty {
    /// The ambiguous URL.
    url: String,
  },
  /// A [std::fmt::Error](std::fmt::Error) was encountered in the underlying destination.
  FmtError,
  /// The CSS modules `composes` property cannot be used within nested rules.
  InvalidComposesNesting,
  /// The CSS modules `composes` property can only be used with a simple class selector.
  InvalidComposesSelector,
  /// The CSS modules pattern must end with `[local]` for use in CSS grid.
  InvalidCssModulesPatternInGrid,
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
      InvalidCssModulesPatternInGrid => write!(f, "The CSS modules `pattern` config must end with `[local]` for use in CSS grid line names."),
    }
  }
}

impl PrinterErrorKind {
  #[deprecated(note = "use `PrinterErrorKind::to_string()` or `fmt::Display` instead")]
  #[allow(missing_docs)]
  pub fn reason(&self) -> String {
    self.to_string()
  }
}
