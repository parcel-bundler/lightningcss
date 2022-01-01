use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use data_encoding::{Specification, Encoding};
use lazy_static::lazy_static;
use crate::properties::css_modules::{Composes, ComposesFrom};
use parcel_selectors::SelectorList;
use crate::selector::Selectors;
use serde::Serialize;
use crate::error::PrinterError;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
#[serde(tag = "type", content = "value", rename_all = "lowercase")]
pub enum CssModuleExport {
  Local(String),
  Dependency {
    name: String,
    specifier: String
  }
}

pub type CssModuleExports = HashMap<String, Vec<CssModuleExport>>;

lazy_static! {
  static ref ENCODER: Encoding = {
    let mut spec = Specification::new();
    spec.symbols.push_str("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_-");
    spec.encoding().unwrap()
  };
}

pub(crate) struct CssModule<'a> {
  pub hash: &'a str,
  pub exports: &'a mut CssModuleExports
}

impl<'a> CssModule<'a> {
  pub fn add_export(&mut self, name: String, export: CssModuleExport) {
    match self.exports.entry(name) {
      std::collections::hash_map::Entry::Occupied(mut entry) => {
        if !entry.get().contains(&export) {
          entry.get_mut().push(export);
        }
      }
      std::collections::hash_map::Entry::Vacant(entry) => {
        let mut items = Vec::new();
        if !items.contains(&export) {
          items.push(export);
        }
        entry.insert(items);
      }
    }
  }

  pub fn add_local(&mut self, exported: &str, local: &str) {
    let local = CssModuleExport::Local(format!("{}_{}", local, self.hash));
    self.add_export(exported.into(), local);
  }

  pub fn add_global(&mut self, exported: &str, global: &str) {
    self.add_export(exported.into(), CssModuleExport::Local(global.into()))
  }

  pub fn add_dependency(&mut self, exported: &str, name: &str, specifier: &str) {
    let dependency = CssModuleExport::Dependency {
      name: name.into(),
      specifier: specifier.into()
    };
    self.add_export(exported.into(), dependency)
  }

  pub fn handle_composes(&mut self, selectors: &SelectorList<Selectors>, composes: &Composes) -> Result<(), PrinterError> {
    for sel in &selectors.0 {
      if sel.len() == 1 {
        match sel.iter_raw_match_order().next().unwrap() {
          parcel_selectors::parser::Component::Class(ref id) => {
            for name in &composes.names {
              match &composes.from {
                None => self.add_local(&id.0, &name.0),
                Some(ComposesFrom::Global) => self.add_global(&id.0, &name.0),
                Some(ComposesFrom::File(file)) => self.add_dependency(&id.0, &name.0, &file)
              }
            }
            continue;
          }
          _ => {}
        }
      }

      // The composes property can only be used within a simple class selector.
      return Err(PrinterError::InvalidComposesSelector(composes.loc))
    }

    Ok(())
  }
}

pub(crate) fn hash(s: &str) -> String {
  let mut hasher = DefaultHasher::new();
  s.hash(&mut hasher);
  let hash = hasher.finish() as u32;
  
  ENCODER.encode(&hash.to_le_bytes())
}
