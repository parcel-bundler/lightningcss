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
