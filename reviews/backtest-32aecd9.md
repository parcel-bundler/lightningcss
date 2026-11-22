# Commit 32aecd9 (reviewed as a PR against 93b9e66): Fix layer merging ordering (Fixes #974)
**Verdict:** request changes. The fix is correct, but it is far more conservative than it needs to be, and on common inputs the size cost is large. **Confidence:** high on correctness and size numbers, medium on the exact shape of the better fix (prototype below).

## Summary
`CssRuleList::minify` used to merge every later `@layer X {}` block into the first block named `X`, and to hoist empty layer declarations into an earlier `@layer` statement. Rules in between could reorder layers or the styles inside a layer, and the code ignored them. The PR makes three changes:
- it keys the merge map by top-level layer name and only merges an exact-name match into the latest block for that root;
- it clears the merge map before **any** rule that is not a layer rule, `Ignored`, or a style rule without nested rules;
- it stops hoisting across the same set of rules, including unlayered `@import`, and does not count conditional `@import … layer(x) media` as declaring `x`.

The bugs are real. On the base build, all 7 of the PR's test inputs, the #974 repro and the #974 follow-up repro (components.legacy) produce different layer order (browser: NOT EQUIVALENT where observable). The PR fixes all of them. The problem is the cost. Because of the blanket clear, `@property`, `@keyframes`, `@font-face`, unlayered `@media` and similar rules now block merging, even though none of them can declare a layer. These rules sit between repeated layer blocks in typical bundled framework output.

## Spec
The fetch of drafts.csswg.org was blocked by the session's network policy, so I could not quote the spec verbatim. From memory, to be verified (css-cascade-5, §"Layer Ordering" / §"Declaring a Cascade Layer", https://drafts.csswg.org/css-cascade-5/#layer-ordering):
- Layers are ordered by when they are **first declared**.
- Nested layers are grouped inside their parent, and the parent's own (unlayered-in-parent) styles come after its sublayers.
- `!important` reverses layer order.
- `@layer` inside conditional group rules (`@media`, `@supports`) declares the layer only when the condition applies.
- `@import … layer(x)` declares `x` at the position of the import, if its conditions match.

Chromium agrees with all of this in the cases below (the conditional-declaration cases are NOT EQUIVALENT on base under Chromium at a matching viewport).

## Findings

1. **Should fix (blocking for merge in my view): the blanket `layer_rules.clear()` / `layer_statement = None` on every non-layer rule throws away safe merges and causes large size regressions.** The guard is `!matches!(rule, LayerBlock|LayerStatement|Ignored) && !matches!(rule, Style(s) if s.rules.0.is_empty())`. It fires for `@property`, `@keyframes`, `@font-face`, `@counter-style`, `@page`, `@namespace`, `@view-transition`, `@position-try`, `@custom-media`, and also for unlayered `@media`/`@supports` and nested style rules that contain no `@layer` at all. None of these can declare or contribute to a layer, so merging layer blocks across them is order-preserving.
   - Minimal case: `@layer a{.x{color:red}} @font-face{font-family:f;src:local(Arial)} @layer b{.x{color:blue}} @layer a{.x{color:green}}`
     - Base: `@layer a{.x{color:green}}@font-face{…}@layer b{.x{color:#00f}}` (browser: equivalent)
     - PR: `@layer a{.x{color:red}}@font-face{…}@layer b{.x{color:#00f}}@layer a{.x{color:green}}`
   - Same with `@keyframes` between `@layer b;` and a later `@layer b{…}`, with `.x{& .y{…}}` between, and with `@media (…){.y{…}}` between (cases file, all equivalent on base).
   - Tailwind v4 shape (`@layer properties; … @property … @layer properties{…}`): the trailing `properties` block no longer folds into the leading declaration.
   - Measured on generated framework-style CSS (`gen/gen.py`), `--minify`, bytes:

     | input | base | PR | Δ PR | gzip base→PR | prototype (below) |
     |---|---|---|---|---|---|
     | tw4 (one Tailwind-v4-shaped build: statement, theme/base/components/utilities, nested `@media`, 20 `@property`, `@keyframes`, `@layer properties{@supports…}`) | 20009 | 20027 | +18 (+0.1%) | 3274→3282 | 20009 |
     | tw4x3 (app + 2 prebuilt Tailwind-v4 libs concatenated, as a bundler does) | 33431 | 38949 | **+5518 (+16.5%)** | 4763→4864 | 33431 |
     | components (80 per-component `@layer components{}` chunks with `@keyframes`/unlayered `@media` between) | 7848 | 8895 | **+1047 (+13%)**; 3→58 layer blocks | 953→1054 | 7848 |
     | atomic (one `@layer atoms{}` per class, `@property`/`@keyframes` interleaved) | 13200 | 14040 | +840 (+6%); 1→61 blocks | 1998→2116 | 13200 |
     | atomic-media (`@media{@layer atoms{…}}` interleaved with `@layer atoms{}`) | 11950 | 18288 | +6338 (+53%); 2→251 blocks | 2149→2315 | 18288 |
     | big (30× tw4x3+components+atomic, i.e. a library's CSS imported repeatedly), 2.1 MB | 460251 | **1679927 (3.65×)** | | | 460251 |

     Most of the tw4x3/big loss is not just the extra `@layer x{}` wrappers. Once the blocks are merged, identical `:root,:host{--…}` theme rules and duplicate utilities inside one layer get deduplicated. The PR loses that.
   - The atomic-media row is the **one cost that is required for correctness**. A conditional `@layer atoms{}` in between really does contribute to `atoms`, and base's merge there is the PR's own "middle conditional block" bug. Recovering that size would take per-property or per-selector conflict analysis. That is out of scope, so accept it.
   - **Suggested fix (prototyped, `proto.diff`):** clear and stop hoisting only when the intervening rule *may declare layers*. That means `@import`, `@layer`, unknown/custom at-rules, or a grouping or style rule whose nested rule list (recursively) contains one. Rules such as `@property`, `@keyframes` and `@font-face` never do.
     ```rust
     fn may_declare_layers<'i, T>(rule: &CssRule<'i, T>) -> bool {
       fn list<'i, T>(r: &CssRuleList<'i, T>) -> bool { r.0.iter().any(may_declare_layers) }
       match rule {
         CssRule::Ignored | CssRule::Keyframes(_) | CssRule::FontFace(_) | CssRule::FontPaletteValues(_)
         | CssRule::FontFeatureValues(_) | CssRule::Page(_) | CssRule::CounterStyle(_) | CssRule::Namespace(_)
         | CssRule::Viewport(_) | CssRule::CustomMedia(_) | CssRule::Property(_) | CssRule::ViewTransition(_)
         | CssRule::PositionTry(_) | CssRule::NestedDeclarations(_) => false,
         CssRule::Style(r) => list(&r.rules),
         CssRule::Nesting(r) => list(&r.style.rules),
         CssRule::Media(r) => list(&r.rules), CssRule::Supports(r) => list(&r.rules),
         CssRule::Container(r) => list(&r.rules), CssRule::Scope(r) => list(&r.rules),
         CssRule::StartingStyle(r) => list(&r.rules), CssRule::MozDocument(r) => list(&r.rules),
         _ => true, // Import, LayerBlock/Statement, Unknown, Custom
       }
     }
     ```
     It is used in place of the two `matches!` guards (first pass: `if !matches!(rule, LayerBlock|LayerStatement) && may_declare_layers(&rule) { layer_rules.clear() }`; hoist pass: `rule if !may_declare_layers(rule) => {}`).
     - All 122 lib tests pass, including the PR's new `test_layer_order_across_rules`.
     - It restores base size exactly on every input above except atomic-media.
     - All 40 edge cases are browser-equivalent.
     - It does a recursive scan per rule at each nesting level, O(depth × rules), with no allocation. The CLI on the 2.1 MB input takes about 0.26 s, against 0.20–0.29 s for base and PR (noisy, single machine).

2. **Should fix (further precision, optional): the top-level-name key gives up some safe merges and could be tighter in both directions.**
   - `@layer a{.x{…}} @layer a.b{…} @layer a{.x{…}}`: base merges and the result is equivalent, because a's own styles sit after its sublayers whatever the block order. The PR no longer merges.
   - `@layer a.b{…} @layer a.c{…} @layer a.b{…}`: the same.

   The truly unsafe case is a later block that contains nested `@layer`, or an intervening block that contributes to the exact same path. Both are already covered by the PR's `a.y`/`a.z` test. A more precise rule:
   - merge a later block named P into the earlier block for P, unless an intervening rule contributes to P itself;
   - if the later block contains nested layer rules, also require that no intervening rule contributes to any path under the same root.

   For intervening *conditional* rules, collect the set of top-level names they declare and remove only those keys, instead of clearing the whole map. Merging a later `a` block across `@media{@layer b{}}` is safe because `a` was already declared earlier. Hoisting must still stop at any rule that declares layers, including anonymous ones, as the PR does. I did not prototype this; Finding 1 recovers almost all of the measured size.

3. **Nit: the tests only cover the fixed bugs, not the optimizations that must survive.** The PR changed no existing expectations, and `test_merge_layers` still passes. That shows no existing test checks merging across neutral rules, which is why the size regression went unnoticed. Add `minify_test` assertions:
   - `@layer a{…} @property --p{…} @layer a{…}` still merges;
   - `@layer properties; … @property … @layer properties{…}` (Tailwind v4) still folds;
   - `@keyframes` between two empty `@layer` statements still hoists.

   Also add the cases I verified the PR fixes but it does not test: `@container`, `@scope` and nested style `.p{@layer a{}}` as the conditional declarer, and an `!important` variant. Only minified output is tested; there is no pretty-print test.

4. **Out of scope (pre-existing, same on base):** custom property values are re-serialized (`--c:#00ff00` → `#0f0`, `oklch(0.2 …)` → `oklch(20% …)`), which is observable through `getComputedStyle`. The browser oracle flagged this on the framework inputs for base, PR and prototype alike. It is not related to this change.

## Edge cases checked
Cases files: `cases.css`, `cases2.css`, `cases3.css`, `cases4.css` (40 cases total), run with `--minify` and `--minify --targets 'chrome 90'`.

| case | base | PR | prototype |
|---|---|---|---|
| #974 repro (`@layer one, one.a, one.b` + nested `b`) | NOT EQUIV | fixed, equiv | equiv |
| #974 follow-up (components.legacy vs nested legacy) | NOT EQUIV | fixed | equiv |
| conditional middle block `@layer a{} @media{@layer a{}} @layer a{}` | NOT EQUIV | fixed | equiv |
| dotted vs nested (`a{@layer x}` / `a.y` / `a{@layer z}`) | NOT EQUIV | fixed | equiv |
| sublayer in `@media` between two `a` blocks | NOT EQUIV | fixed | equiv |
| hoist across `@media`/`@container`/`@scope`/nested-style `@layer a`, then `@layer b, a` with b styled later | NOT EQUIV (×4) | fixed | equiv |
| same with `!important` | NOT EQUIV | fixed | equiv |
| PR tests: `@import` unlayered / conditional `@import layer(a) screen` | output wrong order / drops `a` | fixed | same as PR |
| `@font-face`/`@keyframes`/`@property`/nested unlayered style/unlayered `@media` between same-layer blocks | equiv, merged | equiv, **not merged** | equiv, merged |
| `a{} a.b{} a{}`; `a.b{} a.c{} a.b{}` | equiv, merged | equiv, not merged | not merged (Finding 2) |
| anonymous layer containing `a` between `a` blocks; nested `@media` inside layer | unchanged | unchanged | unchanged |

No PR or prototype output was NOT EQUIVALENT in any case.

## Tests & performance
- PR worktree: `cargo test --lib` gives 122 passed (`CARGO_TARGET_DIR=target/review/cargo-test`). The prototype also gives 122 passed.
- The PR's 7 new assertions fail on base. I checked by running each input through the base CLI: each produces the hoisted or merged (wrong) output, e.g. `@layer c,b,a;@media…`, `@import "a.css" layer(a) screen;@layer b;`.
- Browser oracle on whole framework files: components and atomic are equivalent for PR and prototype. tw4x3 is equivalent apart from the pre-existing custom-property color re-serialization (Finding 4).
- Performance: the PR adds an O(1) check per rule. CLI timing on the 2.1 MB input is within noise (about 0.2–0.29 s for every build). `cargo bench` was not run.

## Not verified
- Spec text was not quoted verbatim (drafts.csswg.org blocked). Statements come from memory plus Chromium behavior.
- Firefox/Safari were not tested. The oracle uses Chromium only.
- Bundler path (`@import … layer()` inlined by `bundler.rs`) was not tested. Only unbundled `@import` was checked through the PR's tests.
- `yarn test:wpt:baseline` was skipped: there are no parser or serializer changes.
- Finding 2's refined rule is not prototyped.

## Tooling feedback
- **Disclosure:** `issue_read get_comments` cannot filter by date. It returned a maintainer comment on #974 dated 2026-11-04, after the commit date, which says the fix was later judged too conservative. I could not avoid seeing it. My cost hypothesis (blanket clear on any non-layer rule) came from reading the diff before that, and every number above was measured independently. Still, treat this backtest as partially contaminated. Suggestion: give backtests a pre-filtered issue snapshot.
- `drafts.csswg.org` is not in the session's allowed hosts, and no web-fetch agent type was available here, so the skill's "quote the spec" step could not be done. Allowlist the host, or vendor the relevant spec sections under `.claude/knowledge/specs/`.
- `scripts/equivalence.mjs` crashes on large generated fixtures with forced states (`ProtocolError: DOM.getDocument … CBOR: stack limit exceeded` in `forceStates`). `--states none` works around it, but a 40 KB input with `--seeds 1` took about 10 minutes. Consider a depth/size cap for generated DOM and a shallow `DOM.getDocument` (`depth`).
- The oracle reports custom-property re-serialization as a difference on every framework-like input. That drowns the signal. An `--ignore-custom-props` or "same as base" diff mode would help when comparing PR against base.
- `cases.mjs` gives no hint when a case is non-discriminating, i.e. base is also equivalent because the fixture doesn't exercise the order. My first 10 "hoisting" cases were useless until I added a later conditional contribution to `b`. A `--expect-base-fail` per-case marker, or reporting "base also equivalent" for cases tagged as repros, would catch this.
- The pitfalls file has no `@layer` section. Worth adding: layer merging and hoisting across conditional rules, `@import`, nested-style `@layer`, dotted vs nested names, and "don't over-block: `@property`/`@keyframes`/`@font-face` can't declare layers".
- `build-pr.sh` reused existing builds via the `commit` stamp, which worked well. The proto build needed its own target dir (`proto-target/`) to avoid the shared-lock wait.

Artifacts (all under `reviews/backtest-32aecd9/`): `cases*.css`, `gen/gen.py` and generated inputs/outputs (`*.base|pr|proto.min.css`), `proto.diff`, `proto/` (patched source).
