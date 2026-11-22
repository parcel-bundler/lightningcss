// #202 (also #944, #1159): lowering nesting for targets without :is() (Chrome < 88, Safari < 14, Firefox < 78).
// Triage decision: `&` has :is() semantics (spec), so :is() output is correct in general and must not be
// replaced by a non-equivalent expansion. What we want for old targets: either an exactly equivalent
// plain selector list (parents of equal specificity where expansion cannot change matching), or at least
// a warning that the output needs :is(). Controls pin the spec-correct behavior that must not regress.
const old = {chrome: 55 << 16};
const modern = {chrome: 100 << 16};
const runWithWarnings = (css, targets) => ({transform}) => {
  const r = transform({filename: 'a.css', code: Buffer.from(css), minify: true, targets});
  return JSON.stringify({code: r.code.toString(), warnings: r.warnings.map((w) => w.message).sort()});
};
export const cases = [
  {name: 'want: .foo, .bar { & .baz } for chrome 55 -> no :is(), or a warning',
   run: runWithWarnings(`.foo, .bar { & .baz { padding: 0 } }`, old),
   check: (o) => { const {code, warnings} = JSON.parse(o); return !code.includes(':is(') || warnings.length > 0; }},
  {name: 'want: complex parent `.foo .bar { .baz & }` for chrome 55 (#1159) -> warning (cannot be lowered exactly)',
   run: runWithWarnings(`.foo .bar { .baz & { margin-top: 0 } }`, old),
   check: (o) => { const {code, warnings} = JSON.parse(o); return !code.includes(':is(') || warnings.length > 0; }},
  {name: 'control: single compound parent lowers to a plain selector (chrome 55)',
   css: `.bar { .foo & { color: red } }`, options: {targets: old, minify: true},
   check: (o) => o === '.foo .bar{color:red}'},
  {name: 'control: complex parent keeps :is() semantics, chrome 100',
   css: `.bar .baz { .foo & { color: red } }`, options: {targets: modern, minify: true},
   check: (o) => o === '.foo :is(.bar .baz){color:red}'},
  {name: 'control: mixed-specificity parents keep :is() (expansion changes specificity), chrome 100',
   css: `.foo, #bar { & .baz { color: red } }`, options: {targets: modern, minify: true},
   check: (o) => o.includes(':is(.foo,#bar) .baz')},
  {name: 'control: mixed-specificity parents are never expanded, even for chrome 55',
   css: `.foo, #bar { & .baz { color: red } }`, options: {targets: old, minify: true},
   check: (o) => !o.includes('.foo .baz')},
  {name: 'control: hand-written :is() gets a :-webkit-any() fallback for chrome 55',
   css: `:is(.foo, .bar) .baz { padding: 0 }`, options: {targets: old, minify: true},
   check: (o) => o.includes(':-webkit-any(.foo,.bar) .baz')},
];
