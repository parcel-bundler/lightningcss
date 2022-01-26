use cssparser::SourceLocation;
use std::{collections::{HashMap, VecDeque}, path::{Path, PathBuf}, sync::RwLock, ptr::read, borrow::BorrowMut};
use rayon::prelude::*;
use crate::{stylesheet::{StyleSheet, ParserOptions}, rules::{CssRule, CssRuleList, media::MediaRule, supports::{SupportsRule, SupportsCondition}, import::ImportRule}, error::ParserError, media_query::{MediaList, MediaType, MediaCondition, Operator, Qualifier}};
use std::fs;
use itertools::Itertools;
use dashmap::{DashMap, DashSet};

#[derive(Debug)]
pub struct Bundler {
  loaded: DashMap<PathBuf, ImportRule>,
  stylesheets: DashMap<PathBuf, BundleStyleSheet>
}

#[derive(Debug)]
struct BundleStyleSheet {
  stylesheet: StyleSheet,
  dependencies: Vec<PathBuf>
}

impl Bundler {
  pub fn bundle(entry: &str, options: ParserOptions) -> Result<StyleSheet, ()> {
    let bundler = Bundler {
      loaded: DashMap::new(),
      stylesheets: DashMap::new()
    };
    let entry_path = PathBuf::from(entry);
    bundler.load_file(entry_path.clone(), ImportRule {
      url: entry.into(),
      supports: None,
      media: MediaList::new(),
      loc: SourceLocation {
        line: 1,
        column: 0
      }
    }, options.clone());

    let mut rules = Vec::new();
    bundler.inline(entry_path, &mut rules);
    Ok(StyleSheet::new("bundle.css".into(), CssRuleList(rules), options))
  }

  fn load_file(&self, file: PathBuf, parent: ImportRule, options: ParserOptions) {
    use dashmap::mapref::entry::Entry;

    match self.loaded.entry(file.clone()) {
      Entry::Occupied(mut entry) => {
        if parent.media.media_queries.is_empty() {
          entry.get_mut().media.media_queries.clear();
        } else if !entry.get().media.media_queries.is_empty() {
          let entry = entry.get_mut();
          for mq in parent.media.media_queries {
            if !entry.media.media_queries.contains(&mq) {
              entry.media.media_queries.push(mq)
            }
          }
        }

        if let Some(supports) = parent.supports {
          if let Some(existing_supports) = &mut entry.get_mut().supports {
            *existing_supports = SupportsCondition::Or(vec![existing_supports.clone(), supports]);
          }
        } else {
          entry.get_mut().supports = None;
        }
        
        return;
      }
      Entry::Vacant(entry) => {
        entry.insert(parent.clone());
      }
    }

    let source = fs::read_to_string(&file).unwrap();
    let mut stylesheet = StyleSheet::parse(
      file.to_str().unwrap().into(),
      &source,
      options.clone(),
    ).unwrap();

    let dependencies = stylesheet.rules.0.par_iter_mut()
      .filter_map(|rule| {
        if let CssRule::Import(import) = rule {
          let path = file.with_file_name(import.url.clone());
          self.load_file(path.clone(), ImportRule {
            media: combine_media(parent.media.clone(), import.media.clone()),
            supports: combine_supports(parent.supports.clone(), import.supports.clone()),
            url: import.url.clone(),
            loc: import.loc
          }, options.clone());
          *rule = CssRule::Ignored;
          Some(path)
        } else {
          None
        }
      })
      .collect();

    self.stylesheets.insert(file, BundleStyleSheet {
      stylesheet,
      dependencies
    });
  }

  fn inline(&self, file: PathBuf, dest: &mut Vec<CssRule>) {
    let stylesheet = match self.stylesheets.remove(&file) {
      Some((_, s)) => s,
      None => return
    };

    for path in stylesheet.dependencies {
      self.inline(path, dest)
    }

    let mut rules = stylesheet.stylesheet.rules.0;
    let loaded = self.loaded.get(&file).unwrap();
    if !loaded.media.media_queries.is_empty() {
      rules = vec![
        CssRule::Media(MediaRule {
          query: loaded.media.clone(),
          rules: CssRuleList(rules),
          loc: loaded.loc
        })
      ]
    }

    if let Some(supports) = &loaded.supports {
      rules = vec![
        CssRule::Supports(SupportsRule {
          condition: supports.clone(),
          rules: CssRuleList(rules),
          loc: loaded.loc
        })
      ]
    }

    dest.extend(rules);
  }
}

fn combine_media(mut a: MediaList, b: MediaList) -> MediaList {
  if a.media_queries.is_empty() {
    return b;
  }

  for b in b.media_queries {
    if a.media_queries.contains(&b) {
      continue;
    }

    for a in &mut a.media_queries {
      if b.media_type != MediaType::All || b.qualifier != None {
        if a.media_type != MediaType::All {
          if a.qualifier != Some(Qualifier::Not) {
            a.media_type = MediaType::All;
            a.qualifier = Some(Qualifier::Not);
          } else {
            // TODO (e.g. not print and not screen)
          }
        } else {
          a.media_type = b.media_type.clone();
          a.qualifier = b.qualifier.clone();
        }
      }

      if let Some(cond) = &b.condition {
        a.condition = if let Some(condition) = &a.condition {
          Some(MediaCondition::Operation(vec![condition.clone(), cond.clone()], Operator::And))
        } else {
          Some(cond.clone())
        }
      }
    }
  }

  a
}

fn combine_supports(a: Option<SupportsCondition>, b: Option<SupportsCondition>) -> Option<SupportsCondition> {
  if let Some(a) = a {
    if let Some(b) = b {
      if let SupportsCondition::And(mut a) = a {
        if !a.contains(&b) {
          a.push(b);
        }
        Some(SupportsCondition::And(a))
      } else if a != b {
        Some(SupportsCondition::Parens(Box::new(SupportsCondition::And(vec![a, b]))))
      } else {
        Some(a)
      }
    } else {
      Some(a)
    }
  } else {
    b
  }
}
