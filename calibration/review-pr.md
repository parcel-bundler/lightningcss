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
