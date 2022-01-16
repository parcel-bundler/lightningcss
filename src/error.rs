use parcel_selectors::parser::SelectorParseErrorKind;
use cssparser::SourceLocation;

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

impl<'i> ParserError<'i> {
  pub fn reason(&self) -> String {
    match self {
      ParserError::InvalidDeclaration => "Invalid declaration".into(),
      ParserError::InvalidMediaQuery => "Invalid media query".into(),
      ParserError::InvalidNesting => "Invalid nesting".into(),
      ParserError::InvalidPageSelector => "Invalid page selector".into(),
      ParserError::InvalidValue => "Invalid value".into(),
      ParserError::SelectorError(s) => {
        use parcel_selectors::parser::SelectorParseErrorKind::*;
        match s {
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
  }
}

#[derive(Debug, PartialEq)]
pub enum MinifyError {
  UnsupportedCustomMediaBooleanLogic {
    media_loc: SourceLocation,
    custom_media_loc: SourceLocation
  },
  CustomMediaNotDefined {
    name: String,
    loc: SourceLocation
  },
  CircularCustomMedia {
    name: String,
    loc: SourceLocation
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

  pub fn loc(&self) -> SourceLocation {
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
  InvalidComposesSelector(SourceLocation),
  InvalidComposesNesting(SourceLocation)
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
