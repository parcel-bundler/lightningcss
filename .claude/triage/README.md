# Triage data

- `repros/<issue>/*.mjs`: repro modules for triaged issues, in the format of `scripts/repro.mjs`.
- `repro-status.json`: the last recorded result of each repro case on master and on the latest release. `scripts/recheck-issues.mjs` compares against it.

## Conventions for stored repros

- **`check()` returns true for the behavior we want, according to the triage decision.** That isn't necessarily what the reporter asked for. For an issue closed as works-as-intended, don't store a repro of the reporter's expectation; store controls that pin down the correct current behavior instead.
- **Controls are welcome.** These are variants that are correct today, such as "the prefixed-first order already works". They act as regression guards: if one starts failing, it shows up as "regressed".
- **Keep repros self-contained.** No absolute paths. Use the `require` passed to `run(lightningcss, {require})` for packages such as browserslist, and keep fixtures in the issue's directory.
- **Repros for nondeterministic bugs must be deterministic in aggregate.** Loop enough iterations inside `run()` that a single run always observes the bug while it exists; otherwise the weekly re-check reports false fixes or regressions.
- **Remove an issue's directory when it's closed,** or keep a control from it as a regression test.

Run `node scripts/recheck-issues.mjs` after compat data updates, releases, or on a schedule. Pass `--update` once the reported changes have been handled.
