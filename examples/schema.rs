fn main() {
  #[cfg(feature = "jsonschema")]
  {
    let schema = schemars::schema_for!(rspack_lightningcss::stylesheet::StyleSheet);
    let output = serde_json::to_string_pretty(&schema).unwrap();
    let _ = std::fs::write("node/ast.json", output);
  }
}
