# CSS pitfalls for Lightning CSS changes

A checklist of ways transforms have gone wrong, or easily could. Use it to generate edge cases when reviewing a PR, reproducing an issue or writing a fix. Each item has a "probe": the kind of input that exposes the problem. Commit hashes and issue numbers point to real past regressions.

Add an entry whenever a review misses something or a maintainer corrects a review.

## 1. Cascade order across rules

Lightning CSS merges, deduplicates and moves rules. That is only safe if no rule *between* the old and new position can match the same elements with the same properties at a competing cascade position.

- **Non-adjacent merging.** Something between two rules can change who wins: rules with equal specificity, `@media`/`@supports`/`@container` blocks, `@layer`, `@import`, `@scope`, nested rules. Probe: `.a{color:red} @media (min-width:1px){.b{color:blue}} .a{color:green}` with an element having both classes.
- **Duplicate-rule removal must compare importance**, not just property names (aa083a7 → 93b9e66, #1347). Probe: `.x{color:red!important} .y{color:green} .x{color:blue}`.
- **Stale hash keys / nondeterminism** after merging (open #1235, #1236; d4eec35). Output must be deterministic run to run and independent of hash iteration order.
- **`@layer` order is set by first declaration**, including declarations inside `@media`/`@supports` (conditional), nested `@layer` blocks (`a.b`) and `@import ... layer()`. Merging or hoisting layer blocks/statements across any of these can reorder layers (6419d54 → 33ea2c1 → 32aecd9, reverted in 4a1b083, #974). Unlayered styles beat layered ones; for `!important` the layer order is reversed. Probe: `@layer one, one.a, one.b; @layer one{@layer b{…}} @layer one.a{…}`.
- **Merging adjacent rules with different selectors into one list.** If any selector is invalid or unsupported in a target browser, that browser drops the *whole* rule (only `:is()`/`:where()` are forgiving). Vendor-prefixed pseudos (`::-webkit-…`, `:-moz-…`) are the classic case. `StyleRule::is_compatible` guards this; check new selector features are covered by compat data.
- **Merging adjacent `@media`/`@supports`/`@container` blocks** (620a36b) is only safe when nothing in between interleaves.
- **Layer merging must be precise, not just safe.** Layer order depends only on when each layer *path* is first declared, so an intervening rule matters only if:
  - it declares a layer path that hasn't been declared yet, including conditionally (`@media{@layer x{}}`) or through `@import … layer()`; or
  - it conditionally contributes styles to the layer being merged.

  Re-declaring layers that are already known changes nothing. Neither does `@property`, `@keyframes` or `@font-face`.
  - 32aecd9 blocked merging at every non-layer rule and was reverted: react-spectrum's atomic CSS exploded.
  - The backtest's prototype (`may_declare_layers`) blocked at every `@layer` rule instead. That still grew react-spectrum S2 by 288%, because each component file starts with `@layer _.prose;` and re-declares `_.a`, `_.b`, `_.b.l`, …
  - Track the set of declared paths; don't classify by rule type. #1418 (draft) proposes an implementation (`LayerMergeTracker`), with tests pinning down the merges that must keep happening.
- **Hash-seed nondeterminism.** `AHasher` keys are random per process, so a bug that depends on hash collisions or iteration order shows up in only a small fraction of runs. #1235/#1236: about 1% of runs silently *dropped a live rule*. Test with many processes and many distinct copies of the pattern.
- **Dedup across CSS-module files.** A candidate that can't be removed, because it came from another file, must not pile up in a bucket. That makes dedup quadratic in the number of files (found in the #1236 review: 12k files took 0.2 s on master and 7.7 s with the PR).

## 2. Order and buffering inside a declaration block

- Handlers buffer declarations and flush later. A buffered value must not move past an unparsed value, a `var()` value, a CSS-wide keyword (`inherit`, `initial`, `unset`, `revert`, `revert-layer`) or `all` (22a8b6f, d7aeff3 → 60622a1, open #1387, #1391). Probe: `width:var(--w);width:200px`, `all:unset;background:var(--x)`.
- **An unparsed value must not replace an earlier valid declaration** unless it contains `var()` or `env()`. Without those, it may be invalid at parse time, in which case the browser ignores it and keeps the earlier one (the fallback).
  - The fallback handler in `prefix_handler.rs` (color, fill, stroke, filter, text-shadow, caret-color, …) treats any unparsed value as valid.
  - `color:red;color:bananas` becomes `color:bananas`, and `color:#0003;color:color-mix(…currentColor…)` loses its fallback for old targets. Both have been wrong since 1.21.0 (#1392, #1404).
  - Probe: `color:red;color:bananas`.
- **Combining longhands into a shorthand resets other longhands.** For example, `font` resets `font-variant-*`, `font-kerning`, `line-height` and more; `border` resets `border-image`; `background` resets `background-size`, `-origin`, `-clip` and others. Combining is only safe when every longhand of the shorthand is known. Probe: `.a{font-kerning:none; font:12px serif}` compared with the reverse order.
- **`!important` and normal declarations** are separate buckets; never merge across them.
- **Fallback chains are intentional.** `color: red; color: oklch(…)` must keep the first declaration when targets lack support for the second. Without targets, removing the earlier declaration is fine only if the later one is known to be valid everywhere.
- **`var()` in a shorthand** makes every longhand pending-substitution. A shorthand with `var()` cannot be split into longhands. Longhands containing `var()` cannot be safely folded into a shorthand.
- **Fallback lists in `PropertyHandlerContext` must be emitted and cleared for every rule kind.** The handler context collects `@supports` overrides (`get_supports_rules`), logical-property rules and dark-mode `@media (prefers-color-scheme)` rules (`get_additional_rules`).
  - If a rule kind doesn't drain *all* of them, nested declarations (`CSSNestedDeclarations`) for example, they are dropped or attached to the next sibling rule's selector.
  - The #1386 review found that PR draining only `@supports`; the logical and dark-mode fallbacks still leaked.
- **Queued fallbacks can resurrect overridden values.** With `--x: lab(…); --x: blue` and targets, an `@supports (color: lab(…)) { --x: lab(…) }` override emitted *after* the rule makes the overridden `lab()` value win in modern browsers. Fallback rules must only carry values that are still the winning declaration.

## 3. Values and serialization

- **Zero units.** Unitless `0` is valid only where `<length>` is accepted *and* no `<number>` alternative changes the meaning.
  - **Affected properties** (every property in mdn-data whose grammar includes both `<number>` and `<length>`):
    - `line-height` and the `/lh` part of `font`;
    - `tab-size`;
    - `border-image-outset`/`-width`;
    - `mask-border-outset`/`-width` and their `-webkit-mask-box-image-*` aliases.
  - **`@property` `initial-value`** when the syntax has a `<number>`/`<integer>` component. Components match *in order*, so `<length> | <number>` with `0` is still a length, and only `<number>` before `<length>` is ambiguous.
  - **Other unsafe spots:** `flex` (`flex:1 1 0` vs a `0px` basis), `<time>` (`0s` must keep its unit), `0%` vs `0`, and values inside `calc()` (`calc(0)` is a number).
  - **Effect:** the type changes, so animations and transitions become discrete, and for `tab-size` and `border-image-*` the computed value changes too (#1416).
- **calc() simplification.** Simplifying is wrong when it:
  - mixes types or loses percentages whose basis depends on the property;
  - drops a range clamp that applies at computed-value time (`width: calc(-1px)` is valid and clamps to 0; `width: -1px` is invalid);
  - mishandles NaN, infinity or `-0` (open #1397, #1398, #1405).
  - `clamp()`/`min()`/`max()` reductions need comparisons that cannot be known at build time (`em` vs `vh`; ddc9ce8).
- **Comma-separated lists keep their pairing.** In `transition` and `animation` lists, entries pair by index with the timing lists, and for a repeated property the *last* entry applies. Deduplicating property names without the timing lists shifts timings onto the wrong properties: `transition: -webkit-transform .3s, transform .3s, opacity .5s` came out with `opacity .3s` (#1401, since 1.28.2).
- **Number precision.** CSSOM serializes numbers to 6 digits, but browsers compute at higher precision internally, so rounding a value at build time changes layout. Chromium lays out in 1/64px, so `33.3333%` of 300px is 99.984375px rather than 100px (#949). For repeating decimals, keep the `calc()` as written or reduce the fraction (e.g. `calc(100% / 3)`). That is shorter and more accurate than printing more digits.
- **Transforms.** Collapsing a transform list into `matrix()` changes how it interpolates in transitions and animations (e37b9ed, #288). Individual `translate`/`rotate`/`scale` properties are separate from `transform` (40fb4ea).
- **Colors:**
  - Conversions between color spaces lose precision.
  - `none` components, alpha (a862e39: relative colors lost the origin's alpha), `currentColor` (06ba62f, 7f59c00: panics in fallbacks), gamut mapping (ab14f90) and `light-dark()`.
  - Minified `color()` must keep all channels (445def9).
  - Relative colors containing `var()` cannot be resolved at build time.
- **Whitespace in token lists.** In custom properties and unknown values, whitespace can be significant. Some examples: `calc(1px - 2px)` needs the spaces around `-`, `a -b` is not `a-b`, and `.a .b` is not `.a.b`. 32f1f52 fixed over-eager stripping, and the size follow-ups #1151 and #1378 show the trade-off.
- **Custom property values** are observable through `var()`, `style()` container queries and CSSOM. Changing their tokens (even `#ff0000` → `red`) changes the computed value.
- **Strings, escapes and identifiers.** Re-escaping must round-trip. Custom identifiers (animation names, counters, grid areas, layer names) are case-sensitive and may collide with keywords (`6e47cdb`: string animation names were unsupported in Chrome). CSS-wide keywords cannot be custom identifiers.
- **Output must re-parse to the same thing.** Minified output that a browser reads differently is the worst kind of bug, because it looks fine in review.

## 4. Lowering for targets

- **Specificity.** Lowered nesting, `:is()` and `:where()` must keep specificity. For example, `.a, #b { .c & {} }` needs `:is()` semantics (max specificity); expanding it into a plain selector list changes the winner (open #1324).
- **Media query range syntax.** `(width < 600px)` → `(max-width: 599.98px)` style epsilons; `not` needs parentheses when combined with `or`/`and` (7f29035 → c4091ee).
- **Boolean context.** `(min-width)` is not a valid `<media-feature>`, while `(width)` is. Chromium treats `(min-width)` as `<general-enclosed>`, which evaluates to *unknown* like `(min-foo)`; it is not a whole-query syntax error (`not all`). So `(min-width) or (width)` still matches, and siblings in a comma list survive (#1366, #1380).
- **Unknown conditions keep their siblings.** Turning a condition into opaque tokens stops lowering its sibling features in the same query, e.g. `(min-width) or (width > 100px)` for Safari 12.
- **Logical properties and `:dir()`/`:lang()` fallbacks** generate extra rules whose position in the cascade matters.
- **Vendor prefixes:**
  - The prefixed form must come before the unprefixed form.
  - Some prefixed syntaxes differ, e.g. `-webkit-linear-gradient` angle direction and legacy `-webkit-box`.
  - Prefixes must not be removed while targets still need them.
  - Unknown prefix combinations must not panic (9d0da3f: `stretch` with all prefixes hit `unreachable!()`).
- **`@supports` fallbacks** must wrap exactly the declarations that need them, and nested declarations keep their order (#1386).
- **Compat data.** "Unknown support" must be treated as unsupported. Partial support in caniuse often means "do not rely on it".
- **Partial implementations in browser-compat-data must count as unsupported.** `scripts/build-prefixes.js` drops `partial_implementation` entries for some features but not all. For example, `animation-timeline` in the `animation` shorthand is "reset-only" in Chrome, yet the shorthand is emitted, and Chromium rejects `animation:… scroll(root)`, losing the whole animation (#572, #1283, PR #1306).
- **Generated compat data can silently change between releases.** A Safari version cap in the generated prefix data dropped `-webkit-user-select` in 1.28.2–1.29.1 (#925). Releases that lock two different caniuse-lite copies are a suspected trigger.

## 5. Strictness and error recovery

- **Strict mode** (the default) errors on definitely invalid use of *known* syntax: `(min-width)` in a boolean context, `(min-width: hi)`. Unknown-but-valid syntax passes through unchanged: `(min-foo)`, unknown properties with valid tokens.
- **`errorRecovery` mode** turns those errors into warnings and keeps the CSS *as written*, the way a browser would skip it. Never "repair" invalid input into something with a different meaning: `(min-width)` → `(width)` was #1366.
- **Recovery granularity.** Recovering a whole condition as opaque tokens also stops optimizing or lowering its valid siblings. Recover the smallest unit possible.
- **Non-standard syntax extensions** (as opposed to unknown properties or at-rules with valid CSS syntax) are out of scope. Don't add parser support for them.

## 6. Robustness

- Pathological input must not take quadratic or exponential time: deeply nested `calc()`/`var()`/`light-dark()` (#1390, #1383), long selector lists, many rules.
- No panics on any input. Look for `unwrap()`, `unreachable!()`, indexing and arithmetic overflow in new code, especially in fallback generation for unusual combinations.
- Error recovery: invalid declarations and rules are dropped exactly as a browser would, without taking neighbors with them.

## 7. Reviewing tests

- **Updated expectations.** Be suspicious when a PR changes existing expected outputs to match new behavior. Wrong outputs have been enshrined before (445def9, a862e39, 60622a1). Every changed expectation needs evidence.
- **Pretty and minified output.** Check that tests cover both, and with targets as well as without.
- **Over-conservative fixes cost bytes for everyone.** A fix that disables an optimization broadly can be correct and still unacceptable. 32aecd9 fixed #974 by stopping layer merging across any other rule. Output from atomic CSS libraries, which wrap each rule in its own `@layer` block, then exploded in size because those blocks no longer merged, so it was reverted (4a1b083; confirmed by the maintainer). Report the size impact on realistic CSS, not just the bug case.
- **Realistic size corpora to check:**
  - atomic CSS (thousands of tiny rules, each in its own `@layer` block, with interleaved `@media`);
  - utility frameworks (Tailwind-style: a layer statement, then `@property`, then layer blocks, then `@media`);
  - component libraries (Bootstrap-style).
  
  The fix must be precise: only stop optimizing when an intervening rule can actually affect the result.
