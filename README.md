# Lightning CSS triage data

This branch holds data that the maintenance tooling on `master` reads and writes: issue repros, their recorded status, triage records and calibration logs. It has no shared history with `master` and no CI. It is updated directly (no PRs) by the triage automation; every change is a commit, so `git log -- repros/<issue>` shows how a triage decision evolved.

The scripts on master find it in a worktree at `target/triage` (created on first use from `origin/triage`), or at `$LCSS_TRIAGE_DIR`.

## Layout

- `repros/<issue>/*.mjs`: repro modules for triaged issues, in the format of `scripts/repro.mjs`.
- `repro-status.json`: the last recorded result of each repro case on master and on the latest release. `scripts/recheck-issues.mjs` compares against it; `--update` rewrites it.
- `records/<issue>.json|md`: triage records written by the `triage-issue` skill (status, evidence, draft reply). `records/digests/` holds batch summaries.
- `calibration/`: maintainer verdicts on agent reviews (`review-pr.md`) and triage calls (`triage-issue.md`). Eval sets for the skills: re-run these cases after changing a skill.
- `backtest/`: inputs for blind review backtests.
- `reviews/`: review write-ups for PRs.

## Conventions for stored repros

- **`check()` returns true for the behavior we want, according to the triage decision.** That isn't necessarily what the reporter asked for. For an issue closed as works-as-intended, don't store a repro of the reporter's expectation; store controls that pin down the correct current behavior instead.
- **Controls are welcome.** Variants that are correct today act as regression guards: if one starts failing, it shows up as "regressed".
- **Keep repros self-contained.** No absolute paths. Use the `require` passed to `run(lightningcss, {require})` for packages such as browserslist, and keep fixtures in the issue's directory.
- **Repros for nondeterministic bugs must be deterministic in aggregate.** Loop enough iterations inside `run()` that a single run always observes the bug while it exists.
- **When a fix lands,** its PR adds a real test in `src/lib.rs`; the repro here flips to correct at the next recheck. Remove an issue's directory when it's closed, or keep a control from it as a regression guard.
