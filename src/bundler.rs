use cssparser::SourceLocation;
use std::{fs, path::{Path, PathBuf}};
use rayon::prelude::*;
use dashmap::DashMap;
use crate::{
  stylesheet::{StyleSheet, ParserOptions},
  rules::{CssRule, CssRuleList,
    media::MediaRule,
    supports::{SupportsRule, SupportsCondition},
    import::ImportRule
  },
  media_query::MediaList
};

pub struct Bundler<'a, P> {
  fs: &'a P,
  loaded: DashMap<PathBuf, ImportRule<'a>>,
  stylesheets: DashMap<PathBuf, BundleStyleSheet<'a>>,
  options: ParserOptions
}

pub trait SourceProvider: Send + Sync {
  fn read<'a>(&'a self, file: &Path) -> &'a str;
}

pub struct FileProvider {
  inputs: DashMap<PathBuf, String>
}

impl FileProvider {
  pub fn new() -> FileProvider {
    FileProvider {
      inputs: DashMap::new()
    }
  }
}

impl SourceProvider for FileProvider {
  fn read<'a>(&'a self, file: &Path) -> &'a str {
    let source = fs::read_to_string(file).unwrap();
    self.inputs.entry(file.to_owned())
      .or_insert(source)
      .downgrade()
      .value()
  }
}

#[derive(Debug)]
struct BundleStyleSheet<'a> {
  stylesheet: StyleSheet<'a>,
  dependencies: Vec<PathBuf>
}

impl<'a, P: SourceProvider> Bundler<'a, P> {
  pub fn new(fs: &'a P, options: ParserOptions) -> Self {
    Bundler {
      fs,
      loaded: DashMap::new(),
      stylesheets: DashMap::new(),
      options
    }
  }

  pub fn bundle(&mut self, entry: &'a Path) -> Result<StyleSheet<'a>, ()> {
    // Phase 1: load and parse all files.
    self.load_file(entry, ImportRule {
      url: entry.to_str().unwrap().into(),
      supports: None,
      media: MediaList::new(),
      loc: SourceLocation {
        line: 1,
        column: 0
      }
    });

    // Phase 2: concatenate rules in the right order.
    let mut rules: Vec<CssRule<'a>> = Vec::new();
    self.inline(entry, &mut rules);
    Ok(StyleSheet::new(
      "bundle.css".into(), 
      CssRuleList(rules), 
      self.options.clone()
    ))
  }

  fn load_file(&self, file: &Path, rule: ImportRule<'a>) {
    use dashmap::mapref::entry::Entry;

    // Check if we already loaded this file. This is stored in a separate
    // map from the stylesheet itself so we don't hold a lock while parsing.
    match self.loaded.entry(file.to_owned()) {
      Entry::Occupied(mut entry) => {
        // If we already loaded this file, combine the media queries and supports conditions
        // from this import rule with the existing ones using a logical or operator.
        let entry = entry.get_mut();

        // We cannot combine a media query and a supports query from different @import rules.
        // e.g. @import "a.css" print; @import "a.css" supports(color: red);
        // This would require duplicating the actual rules in the file.
        if (!rule.media.media_queries.is_empty() && !entry.supports.is_none()) || 
          (!entry.media.media_queries.is_empty() && !rule.supports.is_none()) {
          todo!()
        }

        if rule.media.media_queries.is_empty() {
          entry.media.media_queries.clear();
        } else if !entry.media.media_queries.is_empty() {
          entry.media.or(&rule.media);
        }

        if let Some(supports) = rule.supports {
          if let Some(existing_supports) = &mut entry.supports {
            existing_supports.or(&supports)
          }
        } else {
          entry.supports = None;
        }
        
        return;
      }
      Entry::Vacant(entry) => {
        entry.insert(rule.clone());
      }
    }

    let mut stylesheet = StyleSheet::parse(
      file.to_str().unwrap().into(),
      self.fs.read(file),
      self.options.clone(),
    ).unwrap();

    // Collect and load dependencies for this stylesheet in parallel.
    let dependencies = stylesheet.rules.0.par_iter_mut()
      .filter_map(|r| {
        if let CssRule::Import(import) = r {
          let path = file.with_file_name(&*import.url);

          // Combine media queries and supports conditions from parent 
          // stylesheet with @import rule using a logical and operator.
          let mut media = rule.media.clone();
          media.and(&import.media);
          
          self.load_file(&path, ImportRule {
            media,
            supports: combine_supports(rule.supports.clone(), &import.supports),
            // url: import.url.clone(),
            url: "".into(),
            loc: import.loc
          });

          *r = CssRule::Ignored;
          Some(path)
        } else {
          None
        }
      })
      .collect();

    self.stylesheets.insert(file.to_owned(), BundleStyleSheet {
      stylesheet,
      dependencies
    });
  }

  fn inline(&self, file: &Path, dest: &mut Vec<CssRule<'a>>) {
    // Retrieve the stylesheet for this file from the map and remove it.
    // If it doesn't exist, then we already inlined it (e.g. circular dep).
    let stylesheet = match self.stylesheets.remove(file) {
      Some((_, s)) => s,
      None => return
    };

    // Include all dependencies first.
    for path in &stylesheet.dependencies {
      self.inline(path, dest)
    }

    // Wrap rules in the appropriate @media and @supports rules.
    let mut rules: Vec<CssRule<'a>> = stylesheet.stylesheet.rules.0;
    let (_, loaded) = self.loaded.remove(file).unwrap();
    if !loaded.media.media_queries.is_empty() {
      rules = vec![
        CssRule::Media(MediaRule {
          query: loaded.media,
          rules: CssRuleList(rules),
          loc: loaded.loc
        })
      ]
    }

    if let Some(supports) = loaded.supports {
      rules = vec![
        CssRule::Supports(SupportsRule {
          condition: supports,
          rules: CssRuleList(rules),
          loc: loaded.loc
        })
      ]
    }

    dest.extend(rules);
  }
}

fn combine_supports<'a>(a: Option<SupportsCondition<'a>>, b: &Option<SupportsCondition<'a>>) -> Option<SupportsCondition<'a>> {
  if let Some(mut a) = a {
    if let Some(b) = b {
      a.and(b)
    }
    Some(a)
  } else {
    b.clone()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::stylesheet::PrinterOptions;
  use indoc::indoc;

  struct TestProvider {
    map: DashMap<PathBuf, String>
  }

  impl SourceProvider for TestProvider {
    fn read<'a>(&'a self, file: &Path) -> &'a str {
      self.map.get(file).unwrap().value()
    }
  }

  macro_rules! fs(
    { $($key:literal: $value:expr),* } => {
      {
        #[allow(unused_mut)]
        let mut m = DashMap::new();
        $(
          m.insert(PathBuf::from($key), $value.to_owned());
        )*
        TestProvider {
          map: m
        }
      }
    };
  );

  fn bundle(fs: TestProvider, entry: &str) -> String {
    let mut bundler = Bundler::new(&fs, ParserOptions::default());
    let stylesheet = bundler.bundle(Path::new(entry)).unwrap();
    stylesheet.to_css(PrinterOptions::default()).unwrap().code
  }

  #[test]
  fn test_bundle() {
    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css";
        .a { color: red }
      "#,
      "/b.css": r#"
        .b { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      .b {
        color: green;
      }
      
      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css" print;
        .a { color: red }
      "#,
      "/b.css": r#"
        .b { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      @media print {
        .b {
          color: green;
        }
      }
      
      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css" supports(color: green);
        .a { color: red }
      "#,
      "/b.css": r#"
        .b { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      @supports (color: green) {
        .b {
          color: green;
        }
      }
      
      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css" supports(color: green) print;
        .a { color: red }
      "#,
      "/b.css": r#"
        .b { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      @supports (color: green) {
        @media print {
          .b {
            color: green;
          }
        }
      }
      
      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css" print;
        @import "b.css" screen;
        .a { color: red }
      "#,
      "/b.css": r#"
        .b { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      @media print, screen {
        .b {
          color: green;
        }
      }
      
      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css" supports(color: red);
        @import "b.css" supports(foo: bar);
        .a { color: red }
      "#,
      "/b.css": r#"
        .b { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      @supports ((color: red) or (foo: bar)) {
        .b {
          color: green;
        }
      }
      
      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css" print;
        .a { color: red }
      "#,
      "/b.css": r#"
        @import "c.css" (color);
        .b { color: yellow }
      "#,
      "/c.css": r#"
        .c { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      @media print and (color) {
        .c {
          color: green;
        }
      }
      
      @media print {
        .b {
          color: #ff0;
        }
      }

      .a {
        color: red;
      }
    "#});

    let res = bundle(fs! {
      "/a.css": r#"
        @import "b.css";
        .a { color: red }
      "#,
      "/b.css": r#"
        @import "c.css";
      "#,
      "/c.css": r#"
        @import "a.css";
        .c { color: green }
      "#
    }, "/a.css");
    assert_eq!(res, indoc! { r#"
      .c {
        color: green;
      }

      .a {
        color: red;
      }
    "#});

    // let res = bundle(fs! {
    //   "/a.css": r#"
    //     @import "b.css" supports(color: red) (color);
    //     @import "b.css" supports(foo: bar) (orientation: horizontal);
    //     .a { color: red }
    //   "#,
    //   "/b.css": r#"
    //     .b { color: green }
    //   "#
    // }, "/a.css");

    // let res = bundle(fs! {
    //   "/a.css": r#"
    //     @import "b.css" not print;
    //     .a { color: red }
    //   "#,
    //   "/b.css": r#"
    //     @import "c.css" not screen;
    //     .b { color: green }
    //   "#,
    //   "/c.css": r#"
    //     .c { color: yellow }
    //   "#
    // }, "/a.css");
  }
}
