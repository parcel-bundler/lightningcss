//! The `@layer` rule.

use super::{CssRuleList, Location, MinifyContext};
use crate::error::{MinifyError, ParserError, PrinterError};
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::traits::{Parse, ToCss};
use crate::values::string::CowArcStr;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;

/// A [`<layer-name>`](https://drafts.csswg.org/css-cascade-5/#typedef-layer-name) within
/// a `@layer` or `@import` rule.
///
/// Nested layers are represented using a list of identifiers. In CSS syntax, these are dot-separated.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct LayerName<'i>(#[cfg_attr(feature = "serde", serde(borrow))] pub SmallVec<[CowArcStr<'i>; 1]>);

macro_rules! expect_non_whitespace {
  ($parser: ident, $($branches: tt)+) => {{
    let start_location = $parser.current_source_location();
    match *$parser.next_including_whitespace()? {
      $($branches)+
      ref token => {
        return Err(start_location.new_basic_unexpected_token_error(token.clone()))
      }
    }
  }}
}

impl<'i> Parse<'i> for LayerName<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut parts = SmallVec::new();
    let ident = input.expect_ident()?;
    parts.push(ident.into());

    loop {
      let name = input.try_parse(|input| {
        expect_non_whitespace! {input,
          Token::Delim('.') => Ok(()),
        }?;

        expect_non_whitespace! {input,
          Token::Ident(ref id) => Ok(id.into()),
        }
      });

      match name {
        Ok(name) => parts.push(name),
        Err(_) => break,
      }
    }

    Ok(LayerName(parts))
  }
}

impl<'i> ToCss for LayerName<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let mut first = true;
    for name in &self.0 {
      if first {
        first = false;
      } else {
        dest.write_char('.')?;
      }

      serialize_identifier(name, dest)?;
    }

    Ok(())
  }
}

/// A [@layer statement](https://drafts.csswg.org/css-cascade-5/#layer-empty) rule.
///
/// See also [LayerBlockRule](LayerBlockRule).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct LayerStatementRule<'i> {
  /// The layer names to declare.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub names: Vec<LayerName<'i>>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for LayerStatementRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@layer ")?;
    self.names.to_css(dest)?;
    dest.write_char(';')
  }
}

/// A [@layer block](https://drafts.csswg.org/css-cascade-5/#layer-block) rule.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct LayerBlockRule<'i, R = DefaultAtRule> {
  /// The name of the layer to declare, or `None` to declare an anonymous layer.
  #[cfg_attr(feature = "serde", serde(borrow))]
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub name: Option<LayerName<'i>>,
  /// The rules within the `@layer` rule.
  pub rules: CssRuleList<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i, T: Clone> LayerBlockRule<'i, T> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.rules.minify(context, parent_is_unused)?;

    Ok(self.rules.0.is_empty())
  }
}

impl<'a, 'i, T: ToCss> ToCss for LayerBlockRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@layer")?;
    if let Some(name) = &self.name {
      dest.write_char(' ')?;
      name.to_css(dest)?;
    }

    dest.whitespace()?;
    dest.write_char('{')?;
    dest.indent();
    dest.newline()?;
    self.rules.to_css(dest)?;
    dest.dedent();
    dest.newline()?;
    dest.write_char('}')
  }
}

/// A full cascade layer path, relative to the rule list being minified.
/// Anonymous layers are represented by an empty segment, which can never be a
/// valid layer name, so they never match another path.
pub(crate) type LayerPath<'i> = SmallVec<[CowArcStr<'i>; 2]>;

type FastSet<T> = std::collections::HashSet<T, ahash::RandomState>;
type FastMap<K, V> = std::collections::HashMap<K, V, ahash::RandomState>;

fn is_anonymous(path: &[CowArcStr]) -> bool {
  path.iter().any(|s| s.is_empty())
}

/// How a rule affects cascade layers: which layer paths it declares (and
/// whether only conditionally, e.g. inside `@media`), and which paths it
/// contributes styles to. Used to decide when moving layer rules is safe.
#[derive(Default)]
pub(crate) struct LayerEffects<'i> {
  declared: SmallVec<[(LayerPath<'i>, bool); 2]>,
  contributed: SmallVec<[LayerPath<'i>; 1]>,
  /// The rule may declare layers we can't see (e.g. an `@import`).
  unknown: bool,
}

impl<'i> LayerEffects<'i> {
  /// Collects the layer effects of a rule, including nested rules.
  pub fn of<T>(rule: &super::CssRule<'i, T>) -> Self {
    let mut effects = LayerEffects::default();
    effects.collect(rule, &mut SmallVec::new(), false, None);
    effects
  }

  /// Like `of`, but skips layers whose top-level name isn't in `roots`.
  /// Only used for conditional rules, whose declarations never become known,
  /// so skipped layers can't matter to any merge target.
  fn of_rooted<T>(rule: &super::CssRule<'i, T>, roots: &FastMap<CowArcStr<'i>, usize>) -> Self {
    let mut effects = LayerEffects::default();
    effects.collect(rule, &mut SmallVec::new(), false, Some(roots));
    effects
  }

  /// Only the layer that a rule itself names (and its ancestors), without
  /// looking at nested rules. This is enough to track which layers are known
  /// when nothing depends on the rule's other effects. Treating nested layers
  /// as unknown is conservative.
  pub fn of_name<T>(rule: &super::CssRule<'i, T>) -> Self {
    use super::CssRule;
    let mut effects = LayerEffects::default();
    match rule {
      CssRule::LayerBlock(layer) => match &layer.name {
        Some(name) => effects.declare(&[], &name.0, false),
        None => effects.declare(&[], &[CowArcStr::from("")], false),
      },
      CssRule::LayerStatement(..) | CssRule::Import(..) => {
        effects.collect(rule, &mut SmallVec::new(), false, None)
      }
      // Layers nested in other rules are conditional and never become known.
      _ => {}
    }
    effects
  }

  fn declare(&mut self, prefix: &[CowArcStr<'i>], name: &[CowArcStr<'i>], conditional: bool) {
    // A dotted name also declares each of its ancestors.
    for i in 1..=name.len() {
      let path: LayerPath<'i> = prefix.iter().chain(&name[..i]).cloned().collect();
      self.declared.push((path, conditional));
    }
  }

  fn collect<T>(
    &mut self,
    rule: &super::CssRule<'i, T>,
    prefix: &mut LayerPath<'i>,
    conditional: bool,
    roots: Option<&FastMap<CowArcStr<'i>, usize>>,
  ) {
    use super::CssRule;
    let irrelevant =
      |name: &LayerName<'i>| prefix.is_empty() && roots.map_or(false, |r| !r.contains_key(&name.0[0]));
    match rule {
      CssRule::LayerBlock(layer) => match &layer.name {
        Some(name) if irrelevant(name) => {}
        Some(name) => {
          self.declare(prefix, &name.0, conditional);
          let len = prefix.len();
          prefix.extend(name.0.iter().cloned());
          if layer.rules.0.iter().any(|r| {
            !matches!(
              r,
              CssRule::LayerBlock(..) | CssRule::LayerStatement(..) | CssRule::Ignored
            )
          }) {
            self.contributed.push(prefix.clone());
          }
          self.collect_list(&layer.rules, prefix, conditional, None);
          prefix.truncate(len);
        }
        None => {
          // Anonymous layers are unique and can't be referenced, so their
          // contents don't matter, only that a new layer is declared.
          self.declare(prefix, &[CowArcStr::from("")], conditional);
        }
      },
      CssRule::LayerStatement(statement) => {
        for name in &statement.names {
          if !irrelevant(name) {
            self.declare(prefix, &name.0, conditional);
          }
        }
      }
      CssRule::Import(import) => {
        // The imported stylesheet may declare any layers.
        self.unknown = true;
        let conditional = conditional || import.supports.is_some() || !import.media.media_queries.is_empty();
        match &import.layer {
          Some(Some(name)) => self.declare(prefix, &name.0, conditional),
          Some(None) => self.declare(prefix, &[CowArcStr::from("")], conditional),
          None => {}
        }
      }
      // Layers inside conditional rules and style rules only apply when the
      // condition (or selector) does, so treat them as conditional.
      CssRule::Media(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::Supports(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::Container(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::Scope(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::StartingStyle(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::MozDocument(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::Style(r) => self.collect_list(&r.rules, prefix, true, roots),
      CssRule::Nesting(r) => self.collect_list(&r.style.rules, prefix, true, roots),
      _ => {}
    }
  }

  fn collect_list<T>(
    &mut self,
    rules: &CssRuleList<'i, T>,
    prefix: &mut LayerPath<'i>,
    conditional: bool,
    roots: Option<&FastMap<CowArcStr<'i>, usize>>,
  ) {
    for rule in &rules.0 {
      self.collect(rule, prefix, conditional, roots);
    }
  }

  /// Whether a rule can have layer effects at all. Cheap check used to skip
  /// collecting effects for most rules.
  pub fn may_affect_layers<T>(rule: &super::CssRule<'i, T>) -> bool {
    use super::CssRule;
    match rule {
      CssRule::LayerBlock(..) | CssRule::LayerStatement(..) | CssRule::Import(..) => true,
      CssRule::Media(..)
      | CssRule::Supports(..)
      | CssRule::Container(..)
      | CssRule::Scope(..)
      | CssRule::StartingStyle(..)
      | CssRule::MozDocument(..)
      | CssRule::Nesting(..) => true,
      CssRule::Style(r) => !r.rules.0.is_empty(),
      _ => false,
    }
  }

  /// Whether this rule declares any layer that isn't already declared.
  pub fn declares_new<S: std::hash::BuildHasher>(
    &self,
    known: &std::collections::HashSet<LayerPath<'i>, S>,
  ) -> bool {
    self.unknown || self.declared.iter().any(|(p, _)| is_anonymous(p) || !known.contains(p))
  }

  /// Records the unconditional declarations of this rule as known.
  pub fn declare_known<S: std::hash::BuildHasher>(&self, known: &mut std::collections::HashSet<LayerPath<'i>, S>) {
    for (path, conditional) in &self.declared {
      if !conditional && !is_anonymous(path) && !known.contains(path) {
        known.insert(path.clone());
      }
    }
  }
}

/// The earliest `@layer` block with a given name that later blocks with the
/// same name may be merged into, plus what has happened to that layer's
/// subtree since.
struct LayerMergeTarget<'i> {
  index: usize,
  /// Paths in this layer's subtree that received styles since the block.
  contributed_since: FastSet<LayerPath<'i>>,
  /// Paths in this layer's subtree that were declared for the first time
  /// (or conditionally) since the block.
  declared_since: FastSet<LayerPath<'i>>,
}

/// Decides when a later `@layer` block can be merged into an earlier one with
/// the same name.
///
/// Merging moves the later block's contents to the earlier block's position.
/// Layer order is determined by when each layer is first declared, and styles
/// within a layer by source order, so this is only safe when no rule in
/// between:
///  * contributed styles to a layer the later block also contributes to,
///  * first declared a layer the later block declares or contributes to
///    (the moved block would become its first declaration), or
///  * first declared a sublayer while the later block also declares a new
///    sublayer (their relative order would change).
/// Rules that only re-declare known layers, or have no layers at all, never
/// prevent merging.
#[derive(Default)]
pub(crate) struct LayerMergeTracker<'i> {
  known: FastSet<LayerPath<'i>>,
  targets: FastMap<LayerPath<'i>, LayerMergeTarget<'i>>,
  /// Number of targets per top-level layer name.
  roots: FastMap<CowArcStr<'i>, usize>,
}

impl<'i> LayerMergeTracker<'i> {
  pub fn has_targets(&self) -> bool {
    !self.targets.is_empty()
  }

  pub fn is_known(&self, path: &[CowArcStr<'i>]) -> bool {
    self.known.contains(path)
  }

  /// Effects of an intervening (non-layer) rule that are relevant to the
  /// current targets.
  pub fn effects_of<T>(&self, rule: &super::CssRule<'i, T>) -> LayerEffects<'i> {
    LayerEffects::of_rooted(rule, &self.roots)
  }

  /// Effects of a named `@layer` block, scanning nested rules only when the
  /// result can matter: when the block's merge target has seen intervening
  /// changes, or other targets share its top-level name. Otherwise the name
  /// alone is enough (treating nested layers as unknown is conservative), and
  /// skipping the scan keeps deeply nested layers linear.
  pub fn block_effects<T>(&self, name: &[CowArcStr<'i>], rule: &super::CssRule<'i, T>) -> LayerEffects<'i> {
    let related = self.roots.get(&name[0]).copied().unwrap_or(0);
    let needs_full = match self.targets.get(name) {
      Some(target) => related > 1 || !target.contributed_since.is_empty() || !target.declared_since.is_empty(),
      None => related > 0,
    };
    if needs_full {
      LayerEffects::of(rule)
    } else {
      LayerEffects::of_name(rule)
    }
  }

  /// Returns the index of the block that a later block with this name and
  /// these effects can be merged into, if any.
  pub fn merge_target(&self, name: &[CowArcStr<'i>], effects: &LayerEffects<'i>) -> Option<usize> {
    let target = self.targets.get(name)?;
    if effects.unknown {
      return None;
    }
    if effects.contributed.iter().any(|p| target.contributed_since.contains(p)) {
      return None;
    }
    let touched = effects.contributed.iter().chain(effects.declared.iter().map(|(p, _)| p));
    if touched.clone().any(|p| target.declared_since.contains(p)) {
      return None;
    }
    if !target.declared_since.is_empty() && effects.declares_new(&self.known) {
      return None;
    }
    Some(target.index)
  }

  /// Makes `index` the merge target for blocks named `name`.
  pub fn set_target(&mut self, name: LayerPath<'i>, index: usize) {
    if !self.targets.contains_key(&name) {
      *self.roots.entry(name[0].clone()).or_insert(0) += 1;
    }
    self.targets.insert(
      name,
      LayerMergeTarget {
        index,
        contributed_since: Default::default(),
        declared_since: Default::default(),
      },
    );
  }

  /// Records the effects of a rule that now follows every existing target,
  /// except `skip` (a block that it was merged into).
  pub fn apply(&mut self, effects: &LayerEffects<'i>, skip: Option<&[CowArcStr<'i>]>) {
    if effects.unknown {
      // e.g. @import: earlier blocks can't be merged across it.
      self.targets.clear();
      self.roots.clear();
    }
    if !self.targets.is_empty() {
      for (path, _) in &effects.declared {
        if is_anonymous(path) || !self.known.contains(path) {
          self.for_ancestors(path, skip, |t| {
            t.declared_since.insert(path.clone());
          });
        }
      }
      for path in &effects.contributed {
        self.for_ancestors(path, skip, |t| {
          t.contributed_since.insert(path.clone());
        });
      }
    }
    effects.declare_known(&mut self.known);
  }

  /// Calls `f` on the target for every prefix of `path` (including itself).
  fn for_ancestors(
    &mut self,
    path: &[CowArcStr<'i>],
    skip: Option<&[CowArcStr<'i>]>,
    mut f: impl FnMut(&mut LayerMergeTarget<'i>),
  ) {
    for len in 1..=path.len() {
      let prefix = &path[..len];
      if Some(prefix) == skip {
        continue;
      }
      if let Some(target) = self.targets.get_mut(prefix) {
        f(target);
      }
    }
  }
}
