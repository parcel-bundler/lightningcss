# Follow-ups found during PR reviews (not filed)

- From #1390: nested `atan2()` is still exponential (parse_atan2 tries 5 arg types, re-parsing nested args). Valid input 1.8 s at 14 levels; with var() 10 levels 8.1 s, 12 levels >20 s. Details: reviews/pr-1390.md, pr1390/timing.mjs.
- From #1390: relative colors with nested math ignore channel keywords: `rgb(from red calc(calc(r) - 10) g b)` fails to parse (kept as written, not lowered). Chromium accepts it. Likely fix: pass parse_ident into nested parse.
- From #1383: `rgb()`/`hsl()` `/ alpha` in UnresolvedColor::parse is also parse-then-reparse (exponential: 22 levels 7 s), and those branches reset depth to 0 (20000 levels overflow the stack). Fix belongs in UnresolvedColor::parse.
- From #1383: CssColor::parse has no nesting limit: `color: light-dark(red,` x20000 overflows the stack (crash).
- Tooling: size-check.sh runs without targets, so it misses lowering-driven growth (#1287: S2 +7.7% with chrome 100/safari 15 targets, size-check showed 0). Add a targets config to size-check.
- From #1259 (pre-existing): the second, differently-prefixed fallback declaration gets no color fallbacks (Safari 15 + oklch() drop-shadow in backdrop-filter loses the effect).
- From #1357 (pre-existing, unverified by me): a stylesheet returned unchanged from a JS visitor prints `100%` as `100.0%` (also 1.30.1).
- From #1357 (pre-existing): Rule visitor cost grows superlinearly with nesting depth: 200 trees nested 30 deep took 66 s on master. Script: pr1357/bench.cjs.
- From #1374 (pre-existing): declarations directly in @scope are dropped; already tracked in #1165.
