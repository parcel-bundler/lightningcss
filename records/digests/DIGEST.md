# Backlog sweep: 60 issues

The sweep covered the 45 most-reacted open issues and the 15 newest. Nothing was posted, labeled or closed on GitHub.

Every bug was reproduced on master and on releases, and regressions were bisected to a release and commit. Behavior claims were checked in Chromium, and your calibration from the pilot was applied. Per-issue records with draft replies are `<N>.json` and `<N>.md` in `triage/sweep/`. Repros are in `.claude/triage/repros/` on the tooling branch, where the weekly re-check covers them.

**Totals:**
- **By severity:** 2 critical, 8 high, 17 medium, 33 low.
- **By outcome:** 25 confirmed bugs, 12 closable now, 4 duplicates, and 21 feature or design decisions for you.

## 1. The one PR that fixes the most

**#1386** is a contributor's PR that we already commented on. If it emits *all* of the handler context's fallbacks, as our comment asked, it fixes:
- **#1049 (critical regression since 1.30.0, commit 6c465c1):** logical-property fallbacks after a nested rule are lost or attached to the wrong rule. Next.js's default targets hit this, as do Tailwind and daisyUI setups. It has 8 reactions, plus duplicates #1023 and #1246.
- **#1381:** the `@supports` color fallbacks case.
- **The dark-mode `light-dark()` case** from my earlier issues file.

The other two open PRs on this are worse: #1123 only fixes the reported case and still leaks into the next rule, and #1061 contains only debug code.

## 2. Critical and high bugs

| Issue | What's wrong | Fix route |
|---|---|---|
| **#1235** (critical) | Dedup drops live rules nondeterministically; master lost a rule in 48 of 50 processes on an amplified input. Tailwind production builds are affected. | PR #1236 is waiting on the simpler design you asked for. The missed-dedup case from the issue body fails 100% of the time, which gives the author the reliable test we asked for. |
| **#1283** (high, plus duplicates #1342 and #1369) | `animation-timeline` is folded into the shorthand, which Chromium rejects, so the whole animation is lost. Cause: browser-compat-data marks it a partial implementation. | PR #1306, reworked as you asked. **Decision:** it also keeps the timeline separate when no targets are set, which contradicts your earlier "no targets means a perfect future browser" answer. No engine supports the merged form yet. |
| **#1221** (high) | `font-family: monospace, monospace` is reduced to `monospace`, so normalize.css's code text shrinks from 16px to 13px in Chromium. That contradicts the earlier comment that this only matters in old browsers. | **PR #1250** is a minimal root-cause fix with tests. normalize.css is now in the real-world corpus. |
| **#780** (high) | Source map columns are counted in UTF-8 bytes instead of UTF-16, so mappings drift after the first non-ASCII character. Wrong in every release. | **PR #1354** fixes it, but adds a scan of every byte on every write; ask for an ASCII fast path. |
| **#821** (high, plus duplicate #873) | `light-dark()` lowered inside a custom property is resolved where the token is declared, so descendant `color-scheme` changes don't apply. 18 reactions in total. | PR #1287 has the right idea (add a native `@supports` layer) but bundles unrelated changes. The docs caveat you agreed to add is still missing. |
| **#310** (high) | Escapes become raw UTF-8 and `@charset` is dropped, so text breaks on non-UTF-8 pages (checked in the browser). | **Decision:** emit `@charset "UTF-8"` when the output has non-ASCII (the correctness fix), and/or add an ASCII-only output option. |
| **#426** (high until released) | Individual `translate`/`rotate`/`scale` were dropped or merged wrongly. | Fixed on master by #1340, unreleased. The stored repro will report it fixed when the release ships, so close it then. |

## 3. Medium bugs to queue

- **Small fixes (good first issues):**
  - **#604 (docs):** three transforms examples use a `Property` visitor key that has never existed, so they silently do nothing. The real key is `Declaration`.
  - **#1396 (regression since 1.29.0):** `not (width = 10px)` silently loses its `not`. The fix is one line in `MediaFeatureComparison::negate`.
  - **#1406:** `calc()` in `resolution` media features; copy the approach from `time.rs`.
- **#1395 (regression since 1.30.0):** the relative color hue keyword is typed as an angle, so `hsl(from red calc(h + 180) s l)` is no longer resolved or lowered. Small fix, but two existing tests in `src/lib.rs` encode the wrong behavior.
- **#1402:** `calc()` doesn't type-check `+`/`-`. This matters for forgotten-unit typos: `width:100px;width:calc(100% - 20)` loses the valid fallback (wrong since 1.21.0).
- **#1392 (and #1404's side finding):** the fallback handler lets any unparsed value replace an earlier valid one. `color:red;color:bananas` becomes `color:bananas`, wrong since 1.21.0. The fix is small, and it's now in the pitfall catalog.
- **#1165:** with `errorRecovery`, declarations directly inside `@scope` are silently dropped, along with the rules after them. The feature is Baseline in all three engines, so implement it without a flag. Also found: lowering nesting for Chrome 118 drops the parent selector of a nested `@scope` (not yet filed).
- **#761:** `composes` is rejected inside `@layer`/`@media`/`@supports`, because `is_nested()` counts any block. postcss-modules allows it. Small fix.
- **#757:** the `lightningcss-cli` placeholder bin fails under pnpm 10 and Bun when install scripts don't run. Use a Node launcher, as esbuild does.
- **#1389 and #1382:** exponential parse time (4.5 s at depth 20), a denial-of-service concern for build services. Review PRs #1390 and #1383. #1383 also needs the `rgb()`/`hsl()` alpha path, and there is a depth-limit bypass that segfaults Node.
- **#660:** a CSS modules pattern without `[local]` gives every class in a file the same name, silently. Hash the class name in (css-loader's behavior) or return an error.
- **#1069:** range syntax in `@container style()`. It's shipped in Chrome and Firefox, so it should be a normal feature rather than going behind a flag.

## 4. Close now (draft replies ready)

- **Fixed in a release:**
  - #701, the most-reacted issue (23 reactions): fixed in 1.32.0.
  - #1300: fixed in 1.33.0.
  - #109: fixed in 1.21.0. Its leftover example is #1381.
  - #440 and #556: answered by the `StyleSheet` visitor since 1.23.0. **The real gap is that this visitor isn't documented** in `website/pages/transforms.md`.
- **Works as intended:**
  - #826: no targets, so `stretch` wins.
  - #828: use `exclude: Features.LightDark` or targets with native support.
  - #6: the remaining items are tracked in #404 and #659.
- **Fringe** (per your calibration): #1398 (spec and browsers disagree on `-0`), #1399.
- **Duplicates:** #873 → #821, #1342 → #1283, #1391 → #1225 (PR #1387 covers it), #1394 → #949.
- **Likely close as fringe**, like #1397: #1405 (NaN), #1393 (quadratic only with hundreds of terms), #1400 (`none` in a transition list).

## 5. Decisions for you (features and design)

- **CSS modules:**
  - #404 `:export`: the most-requested gap. It fails silently today with a misleading `::export` hint. At minimum, replace the hint with a real warning.
  - #659 `@value`: you declined it before. The new angle is importing hashed class names.
  - #102: `bundle()` exports only include the entry file's classes, and the bundler's own test asserts this. Pick a model.
  - #633 and #491: naming options. #1262 likely deadlocks the synchronous `transform()`; #1237 is large.
- **Comments, #43 (12 reactions):** license comments after `@charset` or a BOM are dropped; fixable, and PR #1301 covers the BOM case. Whether to keep `/*!` comments *between* rules, as esbuild does, is your call.
- **Platform and API:**
  - #1163 ppc64le/s390x: PR #1217 likely names the binary wrong.
  - #747: sync `bundle()` silently ignores `resolver`. A cheap first step is a clear error.
  - #5: CSSOM tracking issue.
  - #28: splitting files is the bundler's job.
- **Optimizations:**
  - #271 (12 reactions), #240 and #225: about 1% raw savings and less after compression. The unsafe cases are pinned down in the browser.
  - #225 asks for an opt-in "unsafe" mode; the agent recommends declining it.
  - #231: declarations are regrouped even with `minify: false`. A cheap option is to skip `minify()` when `minify` is false and no targets are set.
- **Drafts and specs:**
  - #1184: keep it behind the existing flag. PR #1321 has one gap: `::scroll-button(*):disabled`.
  - #99 Color 5: `contrast-color()` is the main gap now that all three engines ship it.
  - #202: nesting under a list uses `:is()`; options are a warning or exact expansion.
- **#313:** single quotes when shorter. Review #1338; #1025 has an escaping bug.

## 6. New problems found during the sweep (no issues filed yet)

- **Android WebView compat data:** about 21 features treat WebView below 150 as unsupported, because caniuse only lists the current version. The threshold resets with every data update (correct in 1.25.0, wrong from 1.30.2). browserslist `defaults` with an outdated database then gets the system font stack, nesting lowering and more. The fix belongs in `scripts/build-prefixes.js`; #1198 only patched one feature.
- **Stale caniuse-lite:** the repo's copy is 23 months old. Together with the WebView bug, #925 and #572's partial-implementation bug, this makes the compat data pipeline the most impactful area for automation. Reviewing kdy1's #1408 is the next step there.
- **Smaller ones:**
  - Two different prefixed values collapse to the last one (`-webkit-max-content;-moz-max-content`).
  - A unitless length (`width:10`) silently becomes `10px`.
  - Nested `@scope` lowering drops the parent selector.
  - The CLI panics on parse errors; one more repro for #578.

## 7. Requests

- **Read access to `w3c/csswg-drafts`**, for CSSWG resolutions on spec questions (needed for #1398). I can attach it to this session read-only if you OK it.
- **A browser-backed `check()` mode for stored repros**, so issues like #821 can check "renders the same as native" rather than one particular fix's output. I'll build it next unless you'd rather prioritize something else.
