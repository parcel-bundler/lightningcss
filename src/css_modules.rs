use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use data_encoding::{Specification, Encoding};
use lazy_static::lazy_static;
use crate::rules::Location;
use crate::properties::css_modules::{Composes, ComposesFrom};
use parcel_selectors::SelectorList;
use crate::selector::Selectors;
use serde::Serialize;
use crate::error::PrinterError;

#[derive(PartialEq, Debug, Clone, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum CssModuleReference {
  Local {
    name: String,
  },
  Global {
    name: String
  },
  Dependency {
    name: String,
    specifier: String
  }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CssModuleExport {
  pub name: String,
  pub composes: Vec<CssModuleReference>,
  pub is_referenced: bool
}

pub type CssModuleExports = HashMap<String, CssModuleExport>;

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
  pub fn add_local(&mut self, exported: &str, local: &str) {
    let hash = &self.hash;
    self.exports.entry(exported.into())
      .or_insert_with(|| CssModuleExport {
        name: format!("{}_{}", local, hash),
        composes: vec![],
        is_referenced: false
      });
  }

  pub fn reference(&mut self, name: &str) {
    match self.exports.entry(name.into()) {
      std::collections::hash_map::Entry::Occupied(mut entry) => {
        entry.get_mut().is_referenced = true;
      }
      std::collections::hash_map::Entry::Vacant(entry) => {
        entry.insert(CssModuleExport {
          name: format!("{}_{}", name, self.hash),
          composes: vec![],
          is_referenced: true
        });
      }
    }
  }

  pub fn handle_composes(&mut self, selectors: &SelectorList<Selectors>, composes: &Composes, source_index: u32) -> Result<(), PrinterError> {
    for sel in &selectors.0 {
      if sel.len() == 1 {
        match sel.iter_raw_match_order().next().unwrap() {
          parcel_selectors::parser::Component::Class(ref id) => {
            for name in &composes.names {
              let reference = match &composes.from {
                None => CssModuleReference::Local { name: format!("{}_{}", name.0, self.hash) },
                Some(ComposesFrom::Global) => CssModuleReference::Global { name: name.0.as_ref().into() },
                Some(ComposesFrom::File(file)) => CssModuleReference::Dependency {
                  name: name.0.to_string(),
                  specifier: file.to_string()
                }
              };

              let export = self.exports.get_mut(&id.0.as_ref().to_owned()).unwrap();
              if !export.composes.contains(&reference) {
                export.composes.push(reference);
              }
            }
            continue;
          }
          _ => {}
        }
      }

      // The composes property can only be used within a simple class selector.
      return Err(PrinterError::InvalidComposesSelector(Location {
        source_index,
        line: composes.loc.line,
        column: composes.loc.column
      }))
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
