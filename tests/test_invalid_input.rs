use lightningcss::{
  error::{Error, ParserError},
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
    loc: None
  };

  assert_eq!(parsed.to_string(), expected.to_string());
}
