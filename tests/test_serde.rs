#[cfg(feature = "serde")]
use lightningcss::properties::{Property, PropertyId};
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

#[cfg(feature = "serde")]
#[test]
fn test_serde_var() {
  let code = r#"
    .foo {
      color: var(--color);
      & .bar {
        background: var(--background, red);
      }
    }

    @layer {
      .baz {
        color: red;
      }
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

#[cfg(feature = "serde")]
#[test]
fn test_serde_var_property() {
  let property = Property::parse_string(PropertyId::Color, "var(--color)", ParserOptions::default()).unwrap();
  let json = serde_json::to_string(&property).unwrap();
  let deserialized: Property = serde_json::from_str(&json).unwrap();
  assert_eq!(serde_json::to_string(&deserialized).unwrap(), json);
}
