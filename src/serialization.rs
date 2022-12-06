#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ValueWrapper<T> {
  value: T,
}

impl<'de, T> ValueWrapper<T> {
  pub fn serialize<S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
    T: serde::Serialize,
  {
    let wrapper = ValueWrapper { value };
    serde::Serialize::serialize(&wrapper, serializer)
  }

  pub fn deserialize<D>(deserializer: D) -> Result<T, D::Error>
  where
    D: serde::Deserializer<'de>,
    T: serde::Deserialize<'de>,
  {
    let v: ValueWrapper<T> = serde::Deserialize::deserialize(deserializer)?;
    Ok(v.value)
  }
}
