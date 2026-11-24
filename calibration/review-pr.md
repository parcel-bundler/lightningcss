# Review calibration log

Maintainer verdicts on agent reviews. This is an eval set for the review skill: after changing the skill, re-run reviews of these PRs (or blind backtests) and check whether they reach the maintainer's conclusions. Don't over-fit to individual PRs; general lessons go into `SKILL.md` and `../../knowledge/css-pitfalls.md`.

| Target | Agent verdict | Maintainer verdict | What the agent missed or got right |
|---|---|---|---|
| 32aecd9 (blind backtest, layer merge fix for #974) | request changes: correct, but large size regression; prototyped `may_declare_layers` | agrees; reverted for the size regression | **Missed:** perf testing of the recursive check. The prototype still grew react-spectrum S2's real atomic CSS by 288% because it blocks at every `@layer` rule, and only a real-world corpus showed it. |
| #1416 zero units with `<number> \| <length>` | approve with nits | right idea, but the fix is in the wrong place | **Missed:** the fix is copy-pasted into several properties. It belongs in `LengthOrNumber` (or a `ToCss` derive) so future properties get it automatically. **Right:** completeness check across all affected properties. |
| #1236 nondeterministic dedup | approve, one should-fix (quadratic) | would request a different solution | **Missed:** quadratic behavior should block on its own, and the design has heavy bookkeeping. Benchmark whether the pre-computed hash helps at all vs rehashing (default/fx hasher) per lookup; if not, a much simpler design is possible. |
| #1386 `@supports` in nested declarations | request changes (incomplete fix) | agrees | none |
| #1380 `(min-width)` in a boolean context | needs maintainer decision | strict: error only for known features; error recovery: warning, keep the CSS as written | **Right:** identified the policy question and the Chromium behavior. The policy is now recorded in `CLAUDE.md`. |
| #1418 (agent-written fix for #974) | correct; no size regression on S2 or the rest of the corpus; perf checked | open; to be revisited | **Noted:** "quite a bit more complicated than the initial patch". When writing fixes, weigh precision against complexity: show the simplest correct version first, and justify each extra piece of machinery with a measured need. |

## Decision queue PR batch 2026-11-22

Comments approved and posted for #1250, #1259, #1306, #1321, #1354, #1383, #1387, #1390; #1287 (needs-maintainer-decision) still pending. Maintainer edited three drafts before posting, all small trims:
- #1250: dropped the "thanks for pushing back on the old-browsers-only point" opener; softened "It's still true" to "Looks like it's still true".
- #1354: removed "CI hasn't run on this PR yet, so that still needs to happen."
- #1387: removed "There's no size change on real-world CSS."
Pattern: leave out remarks about process (CI status, size results with no change) and anything that re-litigates the thread; keep the browser behavior and the requests.
Maintainer notes:
- For selector changes, check whether upstream servo/stylo selectors (the fork source) has related changes. Added to the skill.
- Follow-up bugs already mentioned in a PR comment count as handled there; only file separate issues for ones that weren't.
- size-check without targets missed #1287's growth; size-check now also runs with older targets.

## Decision queue PR batch 2026-11-24

Approved as drafted: #1301, #1302, #1352, #1357, #1364 (approving reviews); #1339 closed in favor of #1295.
Approved with edits:
- #1374: removed the optional nit about the Node test pinning `loc` column. Keep optional test-assertion nits out unless they matter.
- #1376: removed the request to add a default body to the new required `Interpolate` trait method. "Not worried about breaking the Rust API for now while in alpha." Don't flag Rust API breaks as blockers during the 1.0 alpha; JS API/AST changes still matter. Maintainer asked when the spec changed: answer spec-history questions in the summary up front.
- #1304 (still pending): removed "so quickly" from the thank-you.
Changed verdicts:
- #1275 merge -> request changes: "Would be nice to avoid the clones if we can." Extra clones/copies in a fix are a design problem, not a nit; and always measure perf when a fix adds cloning (maintainer asked "did you measure perf?").
- #1365 rejected: "The real fix is to actually parse the property so it doesn't parse as a custom property." A workaround for untyped properties (Property::Custom) is a hack even when it works; recommend adding the typed property instead (fully-parse philosophy). Applies equally to #1209 and #1363.

## Decision queue PR batch 2026-11-24 (2)

Approved as drafted (request changes): #1025, #1123 (minor edit), #1227, #1245, #1275 (redraft), #1311, #1314, #1316, #1334, #1363/#1365 (typed-property redrafts). Approved (approve): #1307, #1309, #909.
- #1218: draft flag wanted: "a flag would make sense since WebKit and Mozilla have not given positions yet." Rule: new syntax implemented only in Chrome, with no position from WebKit/Mozilla, goes behind a `drafts` option. Asked whether errorRecovery passes unknown container queries through: it does (verified on master and 1.33.0 via the #1176 repro); new syntax should keep passing through unmodified when opted in.
- All 8 follow-up cards approved; the original PRs were already merged by then.
