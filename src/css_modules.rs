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
  /// A prefix prepended to the hash input for every source. Matches Vite/postcss-modules'
  /// `hashPrefix` option and css-loader's `hashSalt`; setting it to `"\x00\x00\x00\x00"`
  /// makes the hashed bytes match css-loader's tier-0 salt input, which is how a Vite or
  /// postcss-modules build can produce the same scoped-name hashes as webpack/css-loader.
  ///
  /// Applies to both default `[hash]` and `[<algo>:hash:<digest>:<length>]` segments. Has
  /// no effect on `[content-hash]`. Default is `None` (no prefix; Lightning CSS output is
  /// unchanged from prior versions).
  pub hash_prefix: Option<Cow<'static, str>>,
  /// When `true`, the local class/ident name is appended to the hash input separated by
  /// a NUL byte: `<prefix><relative-path>\0<local>`. This matches the per-local hashing
  /// done by css-loader and postcss-modules — without it, every export from a given file
  /// shares one hash. Required to reproduce legacy css-loader/postcss-modules scoped names.
  ///
  /// Default is `false` (preserves lightningcss's per-file hashing behavior).
  pub hash_local_name: bool,
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
      hash_prefix: None,
      hash_local_name: false,
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
  /// - `[<algo>:hash:<digest>:<length>]` (legacy webpack/css-loader-compatible). Any of `algo`, `digest`,
  ///   and `length` can be omitted, e.g. `[hash:base64:5]`, `[md4:hash]`, `[hash:8]`.
  ///   Recognized algorithms: `md4`, `xxhash64`. Recognized digests: `hex`, `base64`.
  ///   When the `hash` keyword is bare (`[hash]`) the default Lightning CSS hash is used;
  ///   otherwise [hash_with_options](hash_with_options) applies, and the rendered scoped name is
  ///   post-processed to match legacy css-loader/postcss-modules identifier output.
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
      // Bare `[hash]` keeps Lightning CSS's default hash path.
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

  /// Whether this pattern uses the extended legacy webpack/css-loader-compatible hash
  /// syntax. These patterns need css-loader's scoped-name post-processing because digest
  /// encodings such as base64 may include bytes that are not valid in literal CSS idents.
  pub fn uses_legacy_compat_hash(&self) -> bool {
    self.segments.iter().any(|s| match s {
      Segment::Hash { algo, digest, length } => algo.is_some() || digest.is_some() || length.is_some(),
      _ => false,
    })
  }

  /// Write the substituted pattern to a destination.
  ///
  /// `hash_input` is the raw string used as input to compute hashes for `[hash]` segments
  /// (typically the project-root-relative source path). For default `[hash]` segments (no
  /// algo/digest/length specified) the existing siphash + custom-base64 algorithm is used,
  /// preserving byte compatibility with previous Lightning CSS output. Segments with any
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
  pub(crate) fn write_to_string(
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
  /// When all of `algo`, `digest`, and `length` are `None`, the default Lightning CSS
  /// hash (siphash + custom base64) is used and the result is prefixed with `_` if
  /// it starts with a digit and the segment is at the start of the pattern. When any
  /// is `Some`, [hash_with_options](hash_with_options) is used with `Xxhash64` as the
  /// default algorithm and `Hex` as the default digest.
  Hash {
    /// The hash algorithm to use, or `None` for the default Lightning CSS hash.
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
    let prefix = config.hash_prefix.as_deref().unwrap_or("");
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
        let rel = source.to_string_lossy();
        if prefix.is_empty() {
          rel.into_owned()
        } else {
          format!("{}{}", prefix, rel)
        }
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

  /// Build the hash input for a `[hash]` segment, optionally appending the local class
  /// name when [Config::hash_local_name] is enabled.
  pub(crate) fn hash_input_for(&self, source_index: u32, local: &str) -> Cow<'_, str> {
    let base = &self.hash_inputs[source_index as usize];
    if self.config.hash_local_name {
      Cow::Owned(format!("{}\x00{}", base, local))
    } else {
      Cow::Borrowed(base)
    }
  }

  /// Apply the legacy css-loader/postcss-modules post-processing pipeline to a rendered
  /// scoped name when the pattern uses extended hash syntax; otherwise return `s`
  /// unchanged.
  pub(crate) fn maybe_escape(&self, s: String) -> String {
    if self.config.pattern.uses_legacy_compat_hash() {
      escape_scoped_name(&s)
    } else {
      s
    }
  }

  pub fn add_local(&mut self, exported: &str, name: String, source_index: u32) {
    self.exports_by_source_index[source_index as usize]
      .entry(exported.into())
      .or_insert_with(|| CssModuleExport {
        name,
        composes: vec![],
        is_referenced: false,
      });
  }

  pub fn add_dashed(&mut self, local: &str, name: String, source_index: u32) {
    self.exports_by_source_index[source_index as usize]
      .entry(local.into())
      .or_insert_with(|| CssModuleExport {
        name,
        composes: vec![],
        is_referenced: false,
      });
  }

  pub fn reference(&mut self, name: &str, source_index: u32) {
    let hash_input = self.hash_input_for(source_index, name).into_owned();
    let should_escape = self.config.pattern.uses_legacy_compat_hash();
    match self.exports_by_source_index[source_index as usize].entry(name.into()) {
      std::collections::hash_map::Entry::Occupied(mut entry) => {
        entry.get_mut().is_referenced = true;
      }
      std::collections::hash_map::Entry::Vacant(entry) => {
        let body = self
          .config
          .pattern
          .write_to_string(
            String::new(),
            &hash_input,
            &self.sources[source_index as usize],
            name,
            if let Some(content_hashes) = &self.content_hashes {
              &content_hashes[source_index as usize]
            } else {
              ""
            },
          )
          .unwrap();
        let name = if should_escape { escape_scoped_name(&body) } else { body };
        entry.insert(CssModuleExport {
          name,
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
        let hash_input = self.hash_input_for(*source_index, &name[2..]).into_owned();
        let body = self
          .config
          .pattern
          .write_to_string(
            String::new(),
            &hash_input,
            &self.sources[*source_index as usize],
            &name[2..],
            if let Some(content_hashes) = &self.content_hashes {
              &content_hashes[*source_index as usize]
            } else {
              ""
            },
          )
          .unwrap();
        return Some(self.maybe_escape(body));
      }
      None => {
        // Local export. Mark as used.
        if let Some(entry) = self.exports_by_source_index[source_index as usize].get_mut(name) {
          entry.is_referenced = true;
        } else {
          let hash_input = self.hash_input_for(source_index, &name[2..]).into_owned();
          let body = self
            .config
            .pattern
            .write_to_string(
              String::new(),
              &hash_input,
              &self.sources[source_index as usize],
              &name[2..],
              if let Some(content_hashes) = &self.content_hashes {
                &content_hashes[source_index as usize]
              } else {
                ""
              },
            )
            .unwrap();
          let scoped = format!("--{}", self.maybe_escape(body));
          self.exports_by_source_index[source_index as usize].insert(
            name.into(),
            CssModuleExport {
              name: scoped,
              composes: vec![],
              is_referenced: true,
            },
          );
        }
        return None;
      }
    };

    // Reuse the default filename hash as a stable short id here, preserving
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
                None => {
                  let hash_input = self.hash_input_for(source_index, name.0.as_ref()).into_owned();
                  let body = self
                    .config
                    .pattern
                    .write_to_string(
                      String::new(),
                      &hash_input,
                      &self.sources[source_index as usize],
                      name.0.as_ref(),
                      if let Some(content_hashes) = &self.content_hashes {
                        &content_hashes[source_index as usize]
                      } else {
                        ""
                      },
                    )
                    .unwrap();
                  CssModuleReference::Local {
                    name: self.maybe_escape(body),
                  }
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
/// override the default Lightning CSS hash algorithm. When unspecified, the default algorithm
/// (an internal SipHash variant) is used, which preserves byte compatibility with previous
/// Lightning CSS output.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), derive(Serialize, serde::Deserialize))]
#[cfg_attr(any(feature = "serde", feature = "nodejs"), serde(rename_all = "lowercase"))]
pub enum HashAlgorithm {
  /// MD4. Matches webpack's `md4` hash function for css-loader/postcss-modules parity.
  Md4,
  /// xxHash64. Matches webpack's default `xxhash64` hash function.
  Xxhash64,
}

/// Post-process a rendered scoped name using css-loader/postcss-modules' `genericNames`
/// rules: replace any char outside `[a-zA-Z0-9\-_]` (plus the latin-1+ unicode range
/// `U+00A0..=U+FFFF`) with `-`, then prefix `_` when the result starts with `-?[0-9]`
/// or `--` so the output remains a valid CSS identifier.
///
/// This is intentionally separate from `cssparser::serialize_identifier` and
/// `serialize_name`: those functions make a string valid CSS syntax by backslash-escaping
/// it at print time. This rewrites the literal scoped name that appears in both CSS output
/// and the JS exports map so it matches legacy css-loader/postcss-modules output.
pub(crate) fn escape_scoped_name(s: &str) -> String {
  let mut out = String::with_capacity(s.len() + 1);
  for ch in s.chars() {
    let keep =
      ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' || (ch as u32 >= 0x00A0 && (ch as u32) <= 0xFFFF);
    out.push(if keep { ch } else { '-' });
  }
  // /^((-?[0-9])|--)/ -> "_$1"
  let bytes = out.as_bytes();
  let needs_prefix = matches!(
    (bytes.first(), bytes.get(1)),
    (Some(b'0'..=b'9'), _) | (Some(b'-'), Some(b'0'..=b'9' | b'-'))
  );
  if needs_prefix {
    let mut prefixed = String::with_capacity(out.len() + 1);
    prefixed.push('_');
    prefixed.push_str(&out);
    prefixed
  } else {
    out
  }
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
    // base64 alphabet (post-processing happens later in the legacy compatibility layer).
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
  fn parse_bare_hash_keeps_default_lightning_css_hash() {
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
  fn parse_full_legacy_webpack_pattern() {
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
  fn hash_prefix_is_prepended_to_hash_inputs() {
    let mut refs = HashMap::new();
    let sources = vec!["src/styles/Alpha.module.css".to_string()];
    let mut config = Config::default();
    config.hash_prefix = Some(std::borrow::Cow::Borrowed("\x00\x00\x00\x00"));
    let m = CssModule::new(&config, &sources, None, &mut refs, &None);
    assert_eq!(m.hash_inputs[0], "\x00\x00\x00\x00src/styles/Alpha.module.css");
  }

  #[test]
  fn hash_prefix_default_none_leaves_input_unchanged() {
    let mut refs = HashMap::new();
    let sources = vec!["src/styles/Alpha.module.css".to_string()];
    let config = Config::default();
    let m = CssModule::new(&config, &sources, None, &mut refs, &None);
    assert_eq!(m.hash_inputs[0], "src/styles/Alpha.module.css");
  }

  #[test]
  fn hash_local_name_appends_local_after_nul() {
    let mut refs = HashMap::new();
    let sources = vec!["src/styles/Alpha.module.css".to_string()];
    let mut config = Config::default();
    config.hash_prefix = Some(std::borrow::Cow::Borrowed("\x00\x00\x00\x00"));
    config.hash_local_name = true;
    let m = CssModule::new(&config, &sources, None, &mut refs, &None);
    let hi = m.hash_input_for(0, "foo");
    assert_eq!(&*hi, "\x00\x00\x00\x00src/styles/Alpha.module.css\x00foo");
  }

  #[test]
  fn hash_local_name_disabled_returns_path_only() {
    let mut refs = HashMap::new();
    let sources = vec!["src/styles/Alpha.module.css".to_string()];
    let config = Config::default();
    let m = CssModule::new(&config, &sources, None, &mut refs, &None);
    let hi = m.hash_input_for(0, "foo");
    assert_eq!(&*hi, "src/styles/Alpha.module.css");
  }

  #[test]
  fn parity_full_input_matches_webpack_digest() {
    // End-to-end: with hash_prefix + hash_local_name set, the bytes hashed for
    // (path="src/styles/Alpha.module.css", local="foo") match Vite's captured md4 base64
    // truncated digest "YTbdH".
    let mut refs = HashMap::new();
    let sources = vec!["src/styles/Alpha.module.css".to_string()];
    let mut config = Config::default();
    config.hash_prefix = Some(std::borrow::Cow::Borrowed("\x00\x00\x00\x00"));
    config.hash_local_name = true;
    let m = CssModule::new(&config, &sources, None, &mut refs, &None);
    let hi = m.hash_input_for(0, "foo");
    let digest = hash_with_options(hi.as_bytes(), HashAlgorithm::Md4, DigestType::Base64, Some(5));
    assert_eq!(digest, "YTbdH");
  }

  #[test]
  fn escape_replaces_invalid_chars_with_dash() {
    // Standard base64 alphabet contains `+/`; both should become `-`.
    assert_eq!(escape_scoped_name("LOY/5"), "LOY-5");
    assert_eq!(escape_scoped_name("/ta+0"), "-ta-0");
  }

  #[test]
  fn escape_prefixes_leading_digit() {
    assert_eq!(escape_scoped_name("2kP0r"), "_2kP0r");
    assert_eq!(escape_scoped_name("2lUK7"), "_2lUK7");
  }

  #[test]
  fn escape_prefixes_leading_dash_digit() {
    // `-0F5/` -> after invalid-char replace: `-0F5-` -> leading -? followed by digit -> `_-0F5-`.
    assert_eq!(escape_scoped_name("-0F5-"), "_-0F5-");
  }

  #[test]
  fn escape_prefixes_leading_double_dash() {
    // `//GdO` -> `--GdO` -> leading `--` -> `_--GdO`.
    assert_eq!(escape_scoped_name("--GdO"), "_--GdO");
  }

  #[test]
  fn escape_keeps_dash_letter_unprefixed() {
    // `/rXwJ` -> `-rXwJ` -> leading `-` followed by letter -> unchanged.
    assert_eq!(escape_scoped_name("-rXwJ"), "-rXwJ");
  }

  #[test]
  fn escape_keeps_clean_input_unchanged() {
    assert_eq!(escape_scoped_name("Alpha-module__foo__YTbdH"), "Alpha-module__foo__YTbdH");
  }

  #[test]
  fn escape_keeps_unicode_above_a0() {
    assert_eq!(escape_scoped_name("Café"), "Café");
  }

  #[test]
  fn extended_hash_patterns_escape_scoped_names_automatically() {
    let mut refs = HashMap::new();
    let sources = vec!["src/styles/Alpha.module.css".to_string()];
    {
      let mut config = Config::default();
      config.pattern = Pattern::parse("[md4:hash:base64:5]").unwrap();
      let m = CssModule::new(&config, &sources, None, &mut refs, &None);
      assert_eq!(m.maybe_escape("/ta+0".to_string()), "-ta-0");
    }

    let config = Config::default();
    let m = CssModule::new(&config, &sources, None, &mut refs, &None);
    assert_eq!(m.maybe_escape("/ta+0".to_string()), "/ta+0");
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
