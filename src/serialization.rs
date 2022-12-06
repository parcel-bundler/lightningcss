#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ValueWrapper<T> {
  value: T,
}

impl<'de, T: serde::Serialize + serde::Deserialize<'de>> ValueWrapper<T> {
  pub fn serialize<S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let wrapper = ValueWrapper { value };
    serde::Serialize::serialize(&wrapper, serializer)
  }

  pub fn deserialize<D>(deserializer: D) -> Result<T, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let v: ValueWrapper<T> = serde::Deserialize::deserialize(deserializer)?;
    Ok(v.value)
  }
}
