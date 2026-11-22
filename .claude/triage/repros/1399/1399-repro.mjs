// #1399: division by a literal zero in calc(). Spec (css-values-4 #calc-ieee): x / 0 is ±infinity,
// clamped at the top level; Chromium 153 serializes `calc(1px / 0)` as `calc(infinity * 1px)`.
// On master `@media (min-width: calc(1px / 0))` is a strict-mode error ("Invalid media query").
// Triage decision: fringe input, close. So only controls are stored here: a division by zero in a
// declaration must never be folded to a finite/small value, and the `infinity` idiom keeps working.
export const cases = [
  {
    name: 'control-decl-div-zero-not-mangled',
    css: `.a{width:calc(100px / 0)}.b{width:calc(100px / (1 - 1))}`,
    options: {minify: true},
    check: (out) => /\.a\{width:(calc\(100px ?\/ ?0\)|calc\(infinity ?\* ?1px\)|3\.40282e38px)\}/.test(out),
  },
  {
    name: 'control-media-div-zero-error-recovery-keeps-query',
    css: `@media (min-width: calc(1px / 0)) { .a { color: red } }`,
    options: {minify: true, errorRecovery: true},
    check: (out) => out.includes('@media') && out.includes('.a{color:red}'),
  },
  {
    name: 'control-infinity-idiom',
    css: `@media (min-width: calc(infinity * 1px)) { .a { color: red } }`,
    options: {minify: true},
    check: (out) => out.includes('@media (width>=') && out.includes('.a{color:red}'),
  },
];
