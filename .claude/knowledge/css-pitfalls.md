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

## 2. Order and buffering inside a declaration block

- Handlers buffer declarations and flush later. A buffered value must not move past an unparsed value, a `var()` value, a CSS-wide keyword (`inherit`, `initial`, `unset`, `revert`, `revert-layer`) or `all` (22a8b6f, d7aeff3 → 60622a1, open #1387, #1391). Probe: `width:var(--w);width:200px`, `all:unset;background:var(--x)`.
- **Combining longhands into a shorthand resets other longhands.** For example, `font` resets `font-variant-*`, `font-kerning`, `line-height` and more; `border` resets `border-image`; `background` resets `background-size`, `-origin`, `-clip` and others. Combining is only safe when every longhand of the shorthand is known. Probe: `.a{font-kerning:none; font:12px serif}` compared with the reverse order.
- **`!important` and normal declarations** are separate buckets; never merge across them.
- **Fallback chains are intentional.** `color: red; color: oklch(…)` must keep the first declaration when targets lack support for the second. Without targets, removing the earlier declaration is fine only if the later one is known to be valid everywhere.
- **`var()` in a shorthand** makes every longhand pending-substitution. A shorthand with `var()` cannot be split into longhands. Longhands containing `var()` cannot be safely folded into a shorthand.

## 3. Values and serialization

- **Zero units.** Unitless `0` is valid only where `<length>` is accepted *and* no `<number>` alternative changes the meaning. Unsafe examples: `line-height` (`0` is a number), `flex` (`flex:1 1 0` vs `0px` basis), `tab-size`, `<time>` (`0s` must keep its unit), `0%` vs `0`, and values inside `calc()` (`calc(0)` is a number). #1416 is about this. Probe each property whose grammar includes both `<number>` and `<length>`.
- **calc() simplification.** Simplifying is wrong when it:
  - mixes types or loses percentages whose basis depends on the property;
  - drops a range clamp that applies at computed-value time (`width: calc(-1px)` is valid and clamps to 0; `width: -1px` is invalid);
  - mishandles NaN, infinity or `-0` (open #1397, #1398, #1405).
  - `clamp()`/`min()`/`max()` reductions need comparisons that cannot be known at build time (`em` vs `vh`; ddc9ce8).
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
- **Media query range syntax.** `(width < 600px)` → `(max-width: 599.98px)` style epsilons; `not` needs parentheses when combined with `or`/`and` (7f29035 → c4091ee). Boolean context: `(min-width)` is invalid, while `(width)` is valid (#1366, #1380).
- **Logical properties and `:dir()`/`:lang()` fallbacks** generate extra rules whose position in the cascade matters.
- **Vendor prefixes:**
  - The prefixed form must come before the unprefixed form.
  - Some prefixed syntaxes differ, e.g. `-webkit-linear-gradient` angle direction and legacy `-webkit-box`.
  - Prefixes must not be removed while targets still need them.
  - Unknown prefix combinations must not panic (9d0da3f: `stretch` with all prefixes hit `unreachable!()`).
- **`@supports` fallbacks** must wrap exactly the declarations that need them, and nested declarations keep their order (#1386).
- **Compat data.** "Unknown support" must be treated as unsupported. Partial support in caniuse often means "do not rely on it".

## 5. Robustness

- Pathological input must not take quadratic or exponential time: deeply nested `calc()`/`var()`/`light-dark()` (#1390, #1383), long selector lists, many rules.
- No panics on any input. Look for `unwrap()`, `unreachable!()`, indexing and arithmetic overflow in new code, especially in fallback generation for unusual combinations.
- Error recovery: invalid declarations and rules are dropped exactly as a browser would, without taking neighbors with them.

## 6. Reviewing tests

- **Updated expectations.** Be suspicious when a PR changes existing expected outputs to match new behavior. Wrong outputs have been enshrined before (445def9, a862e39, 60622a1). Every changed expectation needs evidence.
- **Pretty and minified output.** Check that tests cover both, and with targets as well as without.
- **Over-conservative fixes cost bytes for everyone.** A fix that disables an optimization broadly can be correct and still unacceptable (32aecd9 was reverted). Report the size impact on realistic CSS, not just the bug case.
