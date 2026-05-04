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
use data_encoding::{Encoding, Specification, BASE64_NOPAD};
use lazy_static::lazy_static;
use md4::{Digest as Md4Digest, Md4};
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
use xxhash_rust::xxh64::xxh64;

/// Configuration for CSS modules.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct Config {
  /// The name pattern to use when renaming class names and other identifiers.
  /// Default is `[hash]_[local]`.
  pub pattern: Pattern,
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

impl Default for Config {
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
#[derive(Clone, Debug, PartialEq)]
pub struct Pattern {
  /// The list of segments in the pattern.
  pub segments: SmallVec<[Segment; 2]>,
}

impl Default for Pattern {
  fn default() -> Self {
    Pattern {
      segments: smallvec![
        Segment::Hash {
          algo: None,
          digest: None,
          length: None,
        },
        Segment::Literal(Cow::Borrowed("_")),
        Segment::Local
      ],
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

impl Pattern {
  /// Parse a pattern from a string.
  ///
  /// Supported placeholders are:
  /// - `[name]`, `[local]`, `[content-hash]`, `[hash]`
  /// - `[<algo>:hash:<digest>:<length>]` (webpack-compatible). Any of `algo`, `digest`,
  ///   and `length` can be omitted, e.g. `[hash:base64:5]`, `[md4:hash]`, `[hash:8]`.
  ///   Recognized algorithms: `md4`, `xxhash64`. Recognized digests: `hex`, `base64`.
  ///   When the `hash` keyword is bare (`[hash]`) the legacy lightningcss hash is used;
  ///   otherwise [hash_with_options](hash_with_options) applies.
  pub fn parse(mut input: &str) -> Result<Self, PatternParseError> {
    let mut segments = SmallVec::new();
    let mut start_idx: usize = 0;
    while !input.is_empty() {
      if input.starts_with('[') {
        if let Some(end_idx) = input.find(']') {
          let raw = &input[0..=end_idx];
          let segment = match raw {
            "[name]" => Segment::Name,
            "[local]" => Segment::Local,
            "[content-hash]" => Segment::ContentHash,
            _ => Self::parse_hash_segment(raw, start_idx)?,
          };
          segments.push(segment);
          start_idx += end_idx + 1;
          input = &input[end_idx + 1..];
        } else {
          return Err(PatternParseError::UnclosedBrackets(start_idx));
        }
      } else {
        let end_idx = input.find('[').unwrap_or_else(|| input.len());
        segments.push(Segment::Literal(Cow::Owned(input[0..end_idx].to_owned())));
        start_idx += end_idx;
        input = &input[end_idx..];
      }
    }

    Ok(Pattern { segments })
  }

  /// Parse a `[hash]` placeholder, including webpack-style `[<algo>:hash:<digest>:<length>]`.
  /// `raw` includes the surrounding brackets. Returns an error for any other placeholder.
  fn parse_hash_segment(raw: &str, start_idx: usize) -> Result<Segment, PatternParseError> {
    let inner = &raw[1..raw.len() - 1];
    let parts: Vec<&str> = inner.split(':').collect();
    let unknown = || PatternParseError::UnknownPlaceholder(raw.into(), start_idx);
    let hash_pos = parts
      .iter()
      .position(|p| p.eq_ignore_ascii_case("hash"))
      .ok_or_else(unknown)?;
    if hash_pos > 1 {
      // At most one part may precede `hash` (the algo).
      return Err(unknown());
    }
    let algo = if hash_pos == 1 {
      Some(match parts[0].to_ascii_lowercase().as_str() {
        "md4" => HashAlgorithm::Md4,
        "xxhash64" => HashAlgorithm::Xxhash64,
        _ => return Err(unknown()),
      })
    } else {
      None
    };
    let after = &parts[hash_pos + 1..];
    let (digest, length) = match after {
      [] => (None, None),
      [a] => {
        if let Ok(n) = a.parse::<usize>() {
          (None, Some(n))
        } else {
          (Some(parse_digest(a).ok_or_else(unknown)?), None)
        }
      }
      [a, b] => {
        let d = parse_digest(a).ok_or_else(unknown)?;
        let n = b.parse::<usize>().map_err(|_| unknown())?;
        (Some(d), Some(n))
      }
      _ => return Err(unknown()),
    };
    if algo.is_none() && digest.is_none() && length.is_none() {
      // Bare `[hash]` keeps the legacy code path.
      Ok(Segment::Hash {
        algo: None,
        digest: None,
        length: None,
      })
    } else {
      Ok(Segment::Hash { algo, digest, length })
    }
  }

  /// Whether the pattern contains any `[content-hash]` segments.
  pub fn has_content_hash(&self) -> bool {
    self.segments.iter().any(|s| matches!(s, Segment::ContentHash))
  }

  /// Write the substituted pattern to a destination.
  ///
  /// `hash_input` is the raw string used as input to compute hashes for `[hash]` segments
  /// (typically the project-root-relative source path). For legacy `[hash]` segments (no
  /// algo/digest/length specified) the existing siphash + custom-base64 algorithm is used,
  /// preserving byte compatibility with previous lightningcss output. Segments with any
  /// option specified use [hash_with_options](hash_with_options).
  pub fn write<W, E>(
    &self,
    hash_input: &str,
    path: &Path,
    local: &str,
    content_hash: &str,
    mut write: W,
  ) -> Result<(), E>
  where
    W: FnMut(&str) -> Result<(), E>,
  {
    for (idx, segment) in self.segments.iter().enumerate() {
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
        Segment::Hash { algo, digest, length } => {
          if algo.is_none() && digest.is_none() && length.is_none() {
            let h = hash(hash_input, idx == 0);
            write(&h)?;
          } else {
            let h = hash_with_options(
              hash_input.as_bytes(),
              algo.unwrap_or(HashAlgorithm::Xxhash64),
              digest.unwrap_or(DigestType::Hex),
              *length,
            );
            write(&h)?;
          }
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
    hash_input: &str,
    path: &Path,
    local: &str,
    content_hash: &str,
  ) -> Result<String, std::fmt::Error> {
    self.write(hash_input, path, local, content_hash, |s| res.write_str(s))?;
    Ok(res)
  }
}

/// A segment in a CSS modules class name pattern.
///
/// See [Pattern](Pattern).
#[derive(Clone, Debug, PartialEq)]
pub enum Segment {
  /// A literal string segment.
  Literal(Cow<'static, str>),
  /// The base file name.
  Name,
  /// The original class name.
  Local,
  /// A hash of the file name.
  ///
  /// When all of `algo`, `digest`, and `length` are `None`, the legacy lightningcss
  /// hash (siphash + custom base64) is used and the result is prefixed with `_` if
  /// it starts with a digit and the segment is at the start of the pattern. When any
  /// is `Some`, [hash_with_options](hash_with_options) is used with `Xxhash64` as the
  /// default algorithm and `Hex` as the default digest.
  Hash {
    /// The hash algorithm to use, or `None` for the legacy default.
    algo: Option<HashAlgorithm>,
    /// The digest encoding, or `None` to default to `Hex` when any option is set.
    digest: Option<DigestType>,
    /// The maximum encoded length in characters, or `None` for full digest.
    length: Option<usize>,
  },
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

pub(crate) struct CssModule<'a, 'c> {
  pub config: &'a Config,
  pub sources: Vec<&'c Path>,
  /// Raw input strings used to compute `[hash]` segments. One per source, holding the
  /// project-root-relative path (or the raw path when no project_root is set). Hashing
  /// happens at write time inside [Pattern::write] so per-segment options can apply.
  pub hash_inputs: Vec<String>,
  pub content_hashes: &'a Option<Vec<String>>,
  pub exports_by_source_index: Vec<CssModuleExports>,
  pub references: &'a mut HashMap<String, CssModuleReference>,
}

impl<'a, 'c> CssModule<'a, 'c> {
  pub fn new(
    config: &'a Config,
    sources: &'c Vec<String>,
    project_root: Option<&'c str>,
    references: &'a mut HashMap<String, CssModuleReference>,
    content_hashes: &'a Option<Vec<String>>,
  ) -> Self {
    let project_root = project_root.map(|p| Path::new(p));
    let sources: Vec<&Path> = sources.iter().map(|filename| Path::new(filename)).collect();
    let hash_inputs = sources
      .iter()
      .map(|path| {
        // Make paths relative to project root so hashes are stable.
        let source = match project_root {
          Some(project_root) if path.is_absolute() => {
            diff_paths(path, project_root).map_or(Cow::Borrowed(*path), Cow::Owned)
          }
          _ => Cow::Borrowed(*path),
        };
        source.to_string_lossy().into_owned()
      })
      .collect();
    Self {
      config,
      exports_by_source_index: sources.iter().map(|_| HashMap::new()).collect(),
      sources,
      hash_inputs,
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
            &self.hash_inputs[source_index as usize],
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
            &self.hash_inputs[source_index as usize],
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
              &self.hash_inputs[source_index as usize],
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
              &self.hash_inputs[*source_index as usize],
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
                  &self.hash_inputs[source_index as usize],
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

    // Reuse the legacy filename hash as a stable short id here, preserving
    // backward-compatible output for dashed (custom property) cross-file references.
    let source_id = hash(&self.hash_inputs[source_index as usize], false);
    let hash = hash(&format!("{}_{}_{}", source_id, name, key), false);
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
                      &self.hash_inputs[source_index as usize],
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

/// The algorithm used to hash a CSS module name input.
///
/// Used in [Segment::Hash](Segment::Hash) and [Segment::ContentHash](Segment::ContentHash) to
/// override the default lightningcss hash algorithm. When unspecified, the default algorithm
/// (an internal SipHash variant) is used, which preserves byte compatibility with previous
/// lightningcss output.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize, serde::Deserialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(rename_all = "lowercase"))]
pub enum HashAlgorithm {
  /// MD4. Matches webpack's `md4` hash function for css-loader/postcss-modules parity.
  Md4,
  /// xxHash64. Matches webpack's default `xxhash64` hash function.
  Xxhash64,
}

fn parse_digest(s: &str) -> Option<DigestType> {
  match s.to_ascii_lowercase().as_str() {
    "hex" => Some(DigestType::Hex),
    "base64" => Some(DigestType::Base64),
    _ => None,
  }
}

/// The digest encoding used when stringifying a hash for inclusion in a CSS module name.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize, serde::Deserialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(rename_all = "lowercase"))]
pub enum DigestType {
  /// Hexadecimal (lowercase, `[0-9a-f]`).
  Hex,
  /// Standard base64 alphabet (`[A-Za-z0-9+/]`, no padding). Matches Node's `hash.digest("base64")`
  /// output without the trailing `=`. Use this for css-loader/postcss-modules parity.
  Base64,
}

/// Compute the hash of `input` using `algo`, encode it with `digest`, and truncate to `length`
/// bytes (UTF-8) if specified. The output is suitable for inclusion in a scoped CSS module name.
///
/// `length` truncates the encoded string, not the raw digest, matching webpack's
/// `loader-utils.getHashDigest(content, algo, digest, maxLength)`.
pub(crate) fn hash_with_options(
  input: &[u8],
  algo: HashAlgorithm,
  digest: DigestType,
  length: Option<usize>,
) -> String {
  let raw: Vec<u8> = match algo {
    HashAlgorithm::Md4 => Md4::digest(input).to_vec(),
    HashAlgorithm::Xxhash64 => xxh64(input, 0).to_be_bytes().to_vec(),
  };
  let encoded = match digest {
    DigestType::Hex => {
      let mut s = String::with_capacity(raw.len() * 2);
      for b in &raw {
        let _ = write!(s, "{:02x}", b);
      }
      s
    }
    DigestType::Base64 => BASE64_NOPAD.encode(&raw),
  };
  match length {
    Some(n) if n < encoded.len() => encoded[..n].to_string(),
    _ => encoded,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn md4_base64_truncated_matches_webpack() {
    // Reproduces `loader-utils.getHashDigest(content, "md4", "base64", 5)` from a captured
    // Vite/postcss-modules build with hashPrefix="\0\0\0\0", file="src/styles/Alpha.module.css",
    // local="foo".
    let input = b"\x00\x00\x00\x00src/styles/Alpha.module.css\x00foo";
    let got = hash_with_options(input, HashAlgorithm::Md4, DigestType::Base64, Some(5));
    assert_eq!(got, "YTbdH");
  }

  #[test]
  fn md4_base64_truncated_matches_webpack_with_slash_in_digest() {
    let input = b"\x00\x00\x00\x00src/styles/Alpha.module.css\x00cls_4";
    let got = hash_with_options(input, HashAlgorithm::Md4, DigestType::Base64, Some(5));
    // Note: raw digest contains `/` (standard base64 alphabet); post-processing of `/` -> `-`
    // happens later (in the fork). Here we only verify the digest itself.
    assert_eq!(got, "LOY/5");
  }

  #[test]
  fn md4_hex_full() {
    let got = hash_with_options(b"abc", HashAlgorithm::Md4, DigestType::Hex, None);
    assert_eq!(got, "a448017aaf21d8525fc10ae87aa6729d");
  }

  #[test]
  fn xxhash64_hex() {
    // xxh64 of "abc" with seed 0 = 0x44bc2cf5ad770999
    let got = hash_with_options(b"abc", HashAlgorithm::Xxhash64, DigestType::Hex, None);
    assert_eq!(got, "44bc2cf5ad770999");
  }

  #[test]
  fn length_truncates_encoded_not_raw() {
    let full = hash_with_options(b"abc", HashAlgorithm::Md4, DigestType::Hex, None);
    let trunc = hash_with_options(b"abc", HashAlgorithm::Md4, DigestType::Hex, Some(8));
    assert_eq!(trunc, &full[..8]);
  }

  #[test]
  fn base64_uses_standard_alphabet() {
    // Inputs picked to make the digest contain both `+` and `/`, confirming the standard
    // base64 alphabet (post-processing happens later in the css-loader-compat layer).
    let input = b"\x00\x00\x00\x00src/styles/Alpha.module.css\x00cls_48";
    let got = hash_with_options(input, HashAlgorithm::Md4, DigestType::Base64, Some(5));
    assert_eq!(got, "/ta+0");
  }

  fn first_hash(p: &Pattern) -> &Segment {
    p.segments
      .iter()
      .find(|s| matches!(s, Segment::Hash { .. }))
      .unwrap()
  }

  fn parse_hash(s: &str) -> (Option<HashAlgorithm>, Option<DigestType>, Option<usize>) {
    let p = Pattern::parse(s).unwrap();
    match first_hash(&p) {
      Segment::Hash { algo, digest, length } => (*algo, *digest, *length),
      _ => unreachable!(),
    }
  }

  #[test]
  fn parse_bare_hash_keeps_legacy() {
    let p = Pattern::parse("[hash]").unwrap();
    assert!(matches!(
      first_hash(&p),
      Segment::Hash {
        algo: None,
        digest: None,
        length: None
      }
    ));
  }

  #[test]
  fn parse_full_webpack_pattern() {
    assert_eq!(
      parse_hash("[md4:hash:base64:5]"),
      (Some(HashAlgorithm::Md4), Some(DigestType::Base64), Some(5))
    );
    assert_eq!(
      parse_hash("[xxhash64:hash:hex:12]"),
      (Some(HashAlgorithm::Xxhash64), Some(DigestType::Hex), Some(12))
    );
  }

  #[test]
  fn parse_omitted_fields() {
    assert_eq!(parse_hash("[hash:base64]"), (None, Some(DigestType::Base64), None));
    assert_eq!(parse_hash("[hash:5]"), (None, None, Some(5)));
    assert_eq!(parse_hash("[hash:base64:5]"), (None, Some(DigestType::Base64), Some(5)));
    assert_eq!(parse_hash("[md4:hash]"), (Some(HashAlgorithm::Md4), None, None));
    assert_eq!(parse_hash("[md4:hash:5]"), (Some(HashAlgorithm::Md4), None, Some(5)));
    assert_eq!(
      parse_hash("[md4:hash:base64]"),
      (Some(HashAlgorithm::Md4), Some(DigestType::Base64), None)
    );
  }

  #[test]
  fn parse_is_case_insensitive() {
    assert_eq!(
      parse_hash("[MD4:HASH:BASE64:5]"),
      (Some(HashAlgorithm::Md4), Some(DigestType::Base64), Some(5))
    );
  }

  #[test]
  fn parse_rejects_unknown_algo_and_digest() {
    assert!(Pattern::parse("[sha1:hash:hex:8]").is_err());
    assert!(Pattern::parse("[md4:hash:base32:5]").is_err());
    // Two parts before "hash":
    assert!(Pattern::parse("[md4:extra:hash:5]").is_err());
    // No "hash" keyword:
    assert!(Pattern::parse("[md4:base64:5]").is_err());
  }

  #[test]
  fn write_uses_options_to_match_webpack_output() {
    // Reproduce the [name]__[local]__[md4:hash:base64:5] pattern from a captured Vite build,
    // using the digest only (no css-loader content composition / post-process). The hash
    // segment alone must produce "YTbdH" given the same input bytes.
    let pattern = Pattern::parse("[md4:hash:base64:5]").unwrap();
    let mut out = String::new();
    let path = std::path::Path::new("src/styles/Alpha.module.css");
    pattern
      .write(
        "\x00\x00\x00\x00src/styles/Alpha.module.css\x00foo",
        path,
        "foo",
        "",
        |s| {
          out.push_str(s);
          Ok::<_, std::fmt::Error>(())
        },
      )
      .unwrap();
    assert_eq!(out, "YTbdH");
  }
}
