#![allow(non_snake_case)]

macro_rules! wrapper {
  ($name: ident, $value: ident $(, $t: ty)?) => {
    #[derive(serde::Serialize, serde::Deserialize)]
    #[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
    pub struct $name<T $(= $t)?> {
      $value: T,
    }

    impl<'de, T> $name<T> {
      pub fn serialize<S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
      where
        S: serde::Serializer,
        T: serde::Serialize,
      {
        let wrapper = $name { $value: value };
        serde::Serialize::serialize(&wrapper, serializer)
      }

      pub fn deserialize<D>(deserializer: D) -> Result<T, D::Error>
      where
        D: serde::Deserializer<'de>,
        T: serde::Deserialize<'de>,
      {
        let v: $name<T> = serde::Deserialize::deserialize(deserializer)?;
        Ok(v.$value)
      }
    }
  };
}

wrapper!(ValueWrapper, value);
wrapper!(PrefixWrapper, vendorPrefix, crate::vendor_prefix::VendorPrefix);

/// serde-content deserializes a unit (`null` in JS or JSON) as `Some` where an `Option` is expected,
/// unlike serde's private `Content` it replaced, so turn units into `None` before deserializing.
pub(crate) fn null_to_none(value: serde_content::Value<'_>) -> serde_content::Value<'_> {
  use serde_content::Value;
  match value {
    Value::Unit => Value::Option(None),
    Value::Option(Some(value)) => Value::Option(Some(Box::new(null_to_none(*value)))),
    Value::Seq(values) => Value::Seq(values.into_iter().map(null_to_none).collect()),
    Value::Map(entries) => Value::Map(entries.into_iter().map(|(k, v)| (k, null_to_none(v))).collect()),
    value => value,
  }
}
