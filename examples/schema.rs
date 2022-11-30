use lightningcss::stylesheet::StyleSheet;
use schemars::schema_for;

fn main() {
  let schema = schema_for!(StyleSheet);
  let output = serde_json::to_string_pretty(&schema).unwrap();
  std::fs::write("selector.json", output);
}
