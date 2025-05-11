//! CSS module exports.
//!
//! [CSS modules](https://github.com/css-modules/css-modules) are a way of locally scoping names in a
//! CSS file. This includes class names, ids, keyframe animation names, and any other places where the
//! [CustomIdent](super::values::ident::CustomIdent) type is used.
//!
//! CSS modules can be enabled using the `css_modules` option when parsing a style sheet. When the
//! style sheet is printed, hashes will be added to any declared names, and references to those names
//! will be updated accordingly. A map of the original names to compiled (hashed) names will be returned.

use crate::error::PrinterErrorKind;
use crate::properties::css_modules::{Composes, Specifier};
use crate::selector::SelectorList;
use data_encoding::{Encoding, Specification};
use lazy_static::lazy_static;
use pathdiff::diff_paths;
#[cfg(any(feature = "serde", feature = "nodejs"))]
use serde::Serialize;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::path::Path;

/// Configuration for CSS modules.
#[derive(Clone, Debug)]
pub struct Config<'i> {
  /// The name pattern to use when renaming class names and other identifiers.
  /// Default is `[hash]_[local]`.
  pub pattern: Pattern<'i>,
  /// Whether to rename dashed identifiers, e.g. custom properties.
  pub dashed_idents: bool,
  /// Whether to scope animation names.
  /// Default is `true`.
  pub animation: bool,
  /// Whether to scope grid names.
  /// Default is `true`.
  pub grid: bool,
  /// Whether to scope custom identifiers
  /// Default is `true`.
  pub custom_idents: bool,
  /// Whether to scope container names.
  /// Default is `true`.
  pub container: bool,
  /// Whether to check for pure CSS modules.
  pub pure: bool,
}

impl<'i> Default for Config<'i> {
  fn default() -> Self {
    Config {
      pattern: Default::default(),
      dashed_idents: Default::default(),
      animation: true,
      grid: true,
      container: true,
      custom_idents: true,
      pure: false,
    }
  }
}

/// A CSS modules class name pattern.
#[derive(Clone, Debug)]
pub struct Pattern<'i> {
  /// The list of segments in the pattern.
  pub segments: SmallVec<[Segment<'i>; 2]>,
}

impl<'i> Default for Pattern<'i> {
  fn default() -> Self {
    Pattern {
      segments: smallvec![Segment::Hash, Segment::Literal("_"), Segment::Local],
    }
  }
}

/// An error that occurred while parsing a CSS modules name pattern.
#[derive(Debug)]
pub enum PatternParseError {
  /// An unknown placeholder segment was encountered at the given index.
  UnknownPlaceholder(String, usize),
  /// An opening bracket with no following closing bracket was found at the given index.
  UnclosedBrackets(usize),
}

impl std::fmt::Display for PatternParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use PatternParseError::*;
    match self {
      UnknownPlaceholder(p, i) => write!(
        f,
        "Error parsing CSS modules pattern: unknown placeholder \"{}\" at index {}",
        p, i
      ),
      UnclosedBrackets(i) => write!(f, "Error parsing CSS modules pattern: unclosed brackets at index {}", i),
    }
  }
}

impl std::error::Error for PatternParseError {}

impl<'i> Pattern<'i> {
  /// Parse a pattern from a string.
  pub fn parse(mut input: &'i str) -> Result<Self, PatternParseError> {
    let mut segments = SmallVec::new();
    let mut start_idx: usize = 0;
    while !input.is_empty() {
      if input.starts_with('[') {
        if let Some(end_idx) = input.find(']') {
          let segment = match &input[0..=end_idx] {
            "[name]" => Segment::Name,
            "[local]" => Segment::Local,
            "[hash]" => Segment::Hash,
            "[content-hash]" => Segment::ContentHash,
            s => return Err(PatternParseError::UnknownPlaceholder(s.into(), start_idx)),
          };
          segments.push(segment);
          start_idx += end_idx + 1;
          input = &input[end_idx + 1..];
        } else {
          return Err(PatternParseError::UnclosedBrackets(start_idx));
        }
      } else {
        let end_idx = input.find('[').unwrap_or_else(|| input.len());
        segments.push(Segment::Literal(&input[0..end_idx]));
        start_idx += end_idx;
        input = &input[end_idx..];
      }
    }

    Ok(Pattern { segments })
  }

  /// Whether the pattern contains any `[content-hash]` segments.
  pub fn has_content_hash(&self) -> bool {
    self.segments.iter().any(|s| matches!(s, Segment::ContentHash))
  }

  /// Write the substituted pattern to a destination.
  pub fn write<W, E>(
    &self,
    hash: &str,
    path: &Path,
    local: &str,
    content_hash: &str,
    mut write: W,
  ) -> Result<(), E>
  where
    W: FnMut(&str) -> Result<(), E>,
  {
    for segment in &self.segments {
      match segment {
        Segment::Literal(s) => {
          write(s)?;
        }
        Segment::Name => {
          let stem = path.file_stem().unwrap().to_str().unwrap();
          if stem.contains('.') {
            write(&stem.replace('.', "-"))?;
          } else {
            write(stem)?;
          }
        }
        Segment::Local => {
          write(local)?;
        }
        Segment::Hash => {
          write(hash)?;
        }
        Segment::ContentHash => {
          write(content_hash)?;
        }
      }
    }
    Ok(())
  }

  #[inline]
  fn write_to_string(
    &self,
    mut res: String,
    hash: &str,
    path: &Path,
    local: &str,
    content_hash: &str,
  ) -> Result<String, std::fmt::Error> {
    self.write(hash, path, local, content_hash, |s| res.write_str(s))?;
    Ok(res)
  }
}

/// A segment in a CSS modules class name pattern.
///
/// See [Pattern](Pattern).
#[derive(Clone, Debug)]
pub enum Segment<'i> {
  /// A literal string segment.
  Literal(&'i str),
  /// The base file name.
  Name,
  /// The original class name.
  Local,
  /// A hash of the file name.
  Hash,
  /// A hash of the file contents.
  ContentHash,
}

/// A referenced name within a CSS module, e.g. via the `composes` property.
///
/// See [CssModuleExport](CssModuleExport).
#[derive(PartialEq, Debug, Clone)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(
  any(feature = "serde", feature = "nodejs"),
  serde(tag = "type", rename_all = "lowercase")
)]
pub enum CssModuleReference {
  /// A local reference.
  Local {
    /// The local (compiled) name for the reference.
    name: String,
  },
  /// A global reference.
  Global {
    /// The referenced global name.
    name: String,
  },
  /// A reference to an export in a different file.
  Dependency {
    /// The name to reference within the dependency.
    name: String,
    /// The dependency specifier for the referenced file.
    specifier: String,
  },
}

/// An exported value from a CSS module.
#[derive(PartialEq, Debug, Clone)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(rename_all = "camelCase"))]
pub struct CssModuleExport {
  /// The local (compiled) name for this export.
  pub name: String,
  /// Other names that are composed by this export.
  pub composes: Vec<CssModuleReference>,
  /// Whether the export is referenced in this file.
  pub is_referenced: bool,
}

/// A map of exported names to values.
pub type CssModuleExports = HashMap<String, CssModuleExport>;

/// A map of placeholders to references.
pub type CssModuleReferences = HashMap<String, CssModuleReference>;

lazy_static! {
  static ref ENCODER: Encoding = {
    let mut spec = Specification::new();
    spec
      .symbols
      .push_str("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_-");
    spec.encoding().unwrap()
  };
}

pub(crate) struct CssModule<'a, 'b, 'c> {
  pub config: &'a Config<'b>,
  pub sources: Vec<&'c Path>,
  pub hashes: Vec<String>,
  pub content_hashes: &'a Option<Vec<String>>,
  pub exports_by_source_index: Vec<CssModuleExports>,
  pub references: &'a mut HashMap<String, CssModuleReference>,
}

impl<'a, 'b, 'c> CssModule<'a, 'b, 'c> {
  pub fn new(
    config: &'a Config<'b>,
    sources: &'c Vec<String>,
    project_root: Option<&'c str>,
    references: &'a mut HashMap<String, CssModuleReference>,
    content_hashes: &'a Option<Vec<String>>,
  ) -> Self {
    let project_root = project_root.map(|p| Path::new(p));
    let sources: Vec<&Path> = sources.iter().map(|filename| Path::new(filename)).collect();
    let hashes = sources
      .iter()
      .map(|path| {
        // Make paths relative to project root so hashes are stable.
        let source = match project_root {
          Some(project_root) if path.is_absolute() => {
            diff_paths(path, project_root).map_or(Cow::Borrowed(*path), Cow::Owned)
          }
          _ => Cow::Borrowed(*path),
        };
        hash(
          &source.to_string_lossy(),
          matches!(config.pattern.segments[0], Segment::Hash),
        )
      })
      .collect();
    Self {
      config,
      exports_by_source_index: sources.iter().map(|_| HashMap::new()).collect(),
      sources,
      hashes,
      content_hashes,
      references,
    }
  }

  pub fn add_local(&mut self, exported: &str, local: &str, source_index: u32) {
    self.exports_by_source_index[source_index as usize]
      .entry(exported.into())
      .or_insert_with(|| CssModuleExport {
        name: self
          .config
          .pattern
          .write_to_string(
            String::new(),
            &self.hashes[source_index as usize],
            &self.sources[source_index as usize],
            local,
            if let Some(content_hashes) = &self.content_hashes {
              &content_hashes[source_index as usize]
            } else {
              ""
            },
          )
          .unwrap(),
        composes: vec![],
        is_referenced: false,
      });
  }

  pub fn add_dashed(&mut self, local: &str, source_index: u32) {
    self.exports_by_source_index[source_index as usize]
      .entry(local.into())
      .or_insert_with(|| CssModuleExport {
        name: self
          .config
          .pattern
          .write_to_string(
            "--".into(),
            &self.hashes[source_index as usize],
            &self.sources[source_index as usize],
            &local[2..],
            if let Some(content_hashes) = &self.content_hashes {
              &content_hashes[source_index as usize]
            } else {
              ""
            },
          )
          .unwrap(),
        composes: vec![],
        is_referenced: false,
      });
  }

  pub fn reference(&mut self, name: &str, source_index: u32) {
    match self.exports_by_source_index[source_index as usize].entry(name.into()) {
      std::collections::hash_map::Entry::Occupied(mut entry) => {
        entry.get_mut().is_referenced = true;
      }
      std::collections::hash_map::Entry::Vacant(entry) => {
        entry.insert(CssModuleExport {
          name: self
            .config
            .pattern
            .write_to_string(
              String::new(),
              &self.hashes[source_index as usize],
              &self.sources[source_index as usize],
              name,
              if let Some(content_hashes) = &self.content_hashes {
                &content_hashes[source_index as usize]
              } else {
                ""
              },
            )
            .unwrap(),
          composes: vec![],
          is_referenced: true,
        });
      }
    }
  }

  pub fn reference_dashed(&mut self, name: &str, from: &Option<Specifier>, source_index: u32) -> Option<String> {
    let (reference, key) = match from {
      Some(Specifier::Global) => return Some(name[2..].into()),
      Some(Specifier::File(file)) => (
        CssModuleReference::Dependency {
          name: name.to_string(),
          specifier: file.to_string(),
        },
        file.as_ref(),
      ),
      Some(Specifier::SourceIndex(source_index)) => {
        return Some(
          self
            .config
            .pattern
            .write_to_string(
              String::new(),
              &self.hashes[*source_index as usize],
              &self.sources[*source_index as usize],
              &name[2..],
              if let Some(content_hashes) = &self.content_hashes {
                &content_hashes[*source_index as usize]
              } else {
                ""
              },
            )
            .unwrap(),
        )
      }
      None => {
        // Local export. Mark as used.
        match self.exports_by_source_index[source_index as usize].entry(name.into()) {
          std::collections::hash_map::Entry::Occupied(mut entry) => {
            entry.get_mut().is_referenced = true;
          }
          std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(CssModuleExport {
              name: self
                .config
                .pattern
                .write_to_string(
                  "--".into(),
                  &self.hashes[source_index as usize],
                  &self.sources[source_index as usize],
                  &name[2..],
                  if let Some(content_hashes) = &self.content_hashes {
                    &content_hashes[source_index as usize]
                  } else {
                    ""
                  },
                )
                .unwrap(),
              composes: vec![],
              is_referenced: true,
            });
          }
        }
        return None;
      }
    };

    let hash = hash(
      &format!("{}_{}_{}", self.hashes[source_index as usize], name, key),
      false,
    );
    let name = format!("--{}", hash);

    self.references.insert(name.clone(), reference);
    Some(hash)
  }

  pub fn handle_composes(
    &mut self,
    selectors: &SelectorList,
    composes: &Composes,
    source_index: u32,
  ) -> Result<(), PrinterErrorKind> {
    for sel in &selectors.0 {
      if sel.len() == 1 {
        match sel.iter_raw_match_order().next().unwrap() {
          parcel_selectors::parser::Component::Class(ref id) => {
            for name in &composes.names {
              let reference = match &composes.from {
                None => CssModuleReference::Local {
                  name: self
                    .config
                    .pattern
                    .write_to_string(
                      String::new(),
                      &self.hashes[source_index as usize],
                      &self.sources[source_index as usize],
                      name.0.as_ref(),
                      if let Some(content_hashes) = &self.content_hashes {
                        &content_hashes[source_index as usize]
                      } else {
                        ""
                      },
                    )
                    .unwrap(),
                },
                Some(Specifier::SourceIndex(dep_source_index)) => {
                  if let Some(entry) =
                    self.exports_by_source_index[*dep_source_index as usize].get(&name.0.as_ref().to_owned())
                  {
                    let name = entry.name.clone();
                    let composes = entry.composes.clone();
                    let export = self.exports_by_source_index[source_index as usize]
                      .get_mut(&id.0.as_ref().to_owned())
                      .unwrap();

                    export.composes.push(CssModuleReference::Local { name });
                    export.composes.extend(composes);
                  }
                  continue;
                }
                Some(Specifier::Global) => CssModuleReference::Global {
                  name: name.0.as_ref().into(),
                },
                Some(Specifier::File(file)) => CssModuleReference::Dependency {
                  name: name.0.to_string(),
                  specifier: file.to_string(),
                },
              };

              let export = self.exports_by_source_index[source_index as usize]
                .get_mut(&id.0.as_ref().to_owned())
                .unwrap();
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
      return Err(PrinterErrorKind::InvalidComposesSelector);
    }

    Ok(())
  }
}

pub(crate) fn hash(s: &str, at_start: bool) -> String {
  let mut hasher = DefaultHasher::new();
  s.hash(&mut hasher);
  let hash = hasher.finish() as u32;

  let hash = ENCODER.encode(&hash.to_le_bytes());
  if at_start && matches!(hash.as_bytes()[0], b'0'..=b'9') {
    format!("_{}", hash)
  } else {
    hash
  }
}
