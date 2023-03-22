use lightningcss::{
  error::{Error, ErrorLocation, ParserError},
  rules::Location,
  stylesheet::{ParserOptions, StyleSheet},
};

#[test]
fn invalid_hsla_value() {
  let input = r#"
    .corrupt {
      color: hsla(120, 62.32%;
    }
  "#;

  let parsed = StyleSheet::parse(input, ParserOptions::default()).unwrap_err();

  let expected = Error {
    kind: ParserError::InvalidValue,
    loc: Some(ErrorLocation::new(
      Location {
        line: 0,
        column: 23,
        source_index: 0,
      },
      "".to_string(),
    )),
  };

  assert_eq!(parsed.to_string(), expected.to_string());
}
