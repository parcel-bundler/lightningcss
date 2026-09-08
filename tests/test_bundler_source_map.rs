#![cfg(feature = "bundler")]

use lightningcss::{
  bundler::{Bundler, ResolveResult, SourceProvider},
  printer::{OriginalLocation, SourceMap},
  rules::CssRule,
  stylesheet::{ParserOptions, PrinterOptions},
};
use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

struct TestProvider(HashMap<PathBuf, String>);

impl SourceProvider for TestProvider {
  type Error = std::io::Error;

  fn read<'a>(&'a self, file: &Path) -> Result<&'a str, Self::Error> {
    Ok(self.0.get(file).unwrap())
  }

  fn resolve(&self, specifier: &str, originating_file: &Path) -> Result<ResolveResult, Self::Error> {
    Ok(originating_file.with_file_name(specifier).into())
  }
}

// This test writer supports one inline map fixture.
const INPUT_MAP: &str = r#"data:application/json,{"version":3,"sources":["/original.scss"],"sourcesContent":[".original{color:red}"],"names":[],"mappings":"AAYG"}"#;

#[derive(Default)]
struct TestSourceMap {
  sources: Vec<(u32, String, Option<String>)>,
  mappings: Vec<OriginalLocation>,
  input_mapping: Option<OriginalLocation>,
}

impl SourceMap for TestSourceMap {
  fn add_source(&mut self, source: &str) -> u32 {
    // Simulate a writer that canonicalizes and deduplicates source paths.
    let source = if source == "/alias.css" {
      "/dependency.css"
    } else {
      source
    };
    if let Some((index, _, _)) = self.sources.iter().find(|(_, path, _)| path == source) {
      return *index;
    }

    // Source indices need not be contiguous or start at zero.
    let index = 100 + self.sources.len() as u32 * 10;
    self.sources.push((index, source.into(), None));
    index
  }

  fn add_name(&mut self, _: &str) -> u32 {
    unreachable!()
  }

  fn set_source_content(&mut self, source_index: u32, source_content: &str) {
    self.sources.iter_mut().find(|(index, _, _)| *index == source_index).unwrap().2 = Some(source_content.into());
  }

  fn add_mapping(&mut self, _: u32, _: u32, original: Option<OriginalLocation>) {
    self.mappings.push(original.unwrap());
  }

  fn from_data_url(_: &str, data_url: &str) -> Option<Self> {
    if data_url != INPUT_MAP {
      return None;
    }

    let mut map = Self::default();
    let source = map.add_source("/original.scss");
    map.set_source_content(source, ".original{color:red}");
    map.input_mapping = Some(OriginalLocation {
      original_line: 12,
      original_column: 3,
      source,
      name: None,
    });
    Some(map)
  }

  fn find_closest_mapping(&mut self, _: u32, _: u32) -> Option<OriginalLocation> {
    self.input_mapping
  }

  fn get_source(&self, source_index: u32) -> Option<&str> {
    self
      .sources
      .iter()
      .find(|(index, _, _)| *index == source_index)
      .map(|(_, source, _)| source.as_str())
  }

  fn get_name(&self, _: u32) -> Option<&str> {
    None
  }

  fn get_source_content(&self, source_index: u32) -> Option<&str> {
    self
      .sources
      .iter()
      .find(|(index, _, _)| *index == source_index)
      .and_then(|(_, _, content)| content.as_deref())
  }
}

#[test]
fn translates_bundler_source_indices() {
  let entry = "@import 'dependency.css';\n@import 'alias.css';\n.entry { color: red }";
  let dependency = ".dependency { color: blue }";
  let fs = TestProvider(HashMap::from([
    (PathBuf::from("/entry.css"), entry.into()),
    (PathBuf::from("/dependency.css"), dependency.into()),
    (PathBuf::from("/alias.css"), dependency.into()),
  ]));

  for prepopulated in [false, true] {
    let mut map = TestSourceMap::default();
    if prepopulated {
      let existing = map.add_source("/existing.css");
      map.set_source_content(existing, "existing content");
      map.add_source("/dependency.css");
    }
    let mut bundler = Bundler::new(&fs, Some(&mut map), ParserOptions::default());
    let stylesheet = bundler.bundle(Path::new("/entry.css")).unwrap();

    // Rule locations must still refer to the stylesheet's internal source list.
    let rules: Vec<_> = stylesheet
      .rules
      .0
      .iter()
      .filter_map(|rule| match rule {
        CssRule::Style(rule) => Some(rule),
        _ => None,
      })
      .collect();
    assert_eq!(rules.len(), 3);
    for (rule, source) in rules.iter().zip(["/dependency.css", "/alias.css", "/entry.css"]) {
      assert_eq!(stylesheet.sources[rule.loc.source_index as usize], source);
    }

    stylesheet.to_css(PrinterOptions::default(), Some(&mut map)).unwrap();
    assert_eq!(map.mappings.len(), 3);
    for (mapping, (source, content)) in map.mappings.iter().zip([
      ("/dependency.css", dependency),
      ("/dependency.css", dependency),
      ("/entry.css", entry),
    ]) {
      assert_eq!(map.get_source(mapping.source), Some(source));
      assert_eq!(map.get_source_content(mapping.source), Some(content));
    }
    assert_eq!(map.mappings[0].source, map.mappings[1].source);
    if prepopulated {
      assert_eq!(map.get_source_content(100), Some("existing content"));
    }
  }
}

#[test]
fn translates_bundler_indices_alongside_inline_remapping() {
  let dependency = ".dependency { color: blue }";
  let fs = TestProvider(HashMap::from([
    (
      PathBuf::from("/entry.css"),
      format!("@import 'dependency.css';\n.entry {{ color: red }}\n/*# sourceMappingURL={INPUT_MAP} */"),
    ),
    (PathBuf::from("/dependency.css"), dependency.into()),
  ]));
  let mut map = TestSourceMap::default();
  map.add_source("/existing.css");
  let mut bundler = Bundler::new(&fs, Some(&mut map), ParserOptions::default());
  let stylesheet = bundler.bundle(Path::new("/entry.css")).unwrap();
  stylesheet.to_css(PrinterOptions::default(), Some(&mut map)).unwrap();

  assert_eq!(map.mappings.len(), 2);
  let dependency_mapping = map.mappings[0];
  assert_eq!(map.get_source(dependency_mapping.source), Some("/dependency.css"));
  assert_eq!(map.get_source_content(dependency_mapping.source), Some(dependency));
  let original = map.mappings[1];
  assert_eq!(map.get_source(original.source), Some("/original.scss"));
  assert_eq!(map.get_source_content(original.source), Some(".original{color:red}"));
  assert_eq!((original.original_line, original.original_column), (12, 3));
  assert!(!map.sources.iter().any(|(_, source, _)| source == "/entry.css"));
}
