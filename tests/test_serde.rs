#[cfg(feature = "serde")]
use lightningcss::stylesheet::{ParserOptions, StyleSheet};

#[cfg(feature = "serde")]
#[test]
fn test_serde() {
  let code = r#"
    .foo {
      color: red;
    }
  "#;
  let (json, stylesheet) = {
    let stylesheet = StyleSheet::parse(code, ParserOptions::default()).unwrap();
    let json = serde_json::to_string(&stylesheet).unwrap();
    (json, stylesheet)
  };

  let deserialized: StyleSheet = serde_json::from_str(&json).unwrap();
  assert_eq!(&deserialized.rules, &stylesheet.rules);
}
