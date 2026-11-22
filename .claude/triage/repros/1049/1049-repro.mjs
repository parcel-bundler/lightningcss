// #1049: logical border-radius with var() dropped when the rule also contains a nested rule
// check(): the logical radius must survive (lowered to physical + :lang() rules, or kept as-is).
const T80 = {chrome: 80 << 16};
const T120 = {chrome: 120 << 16};
const keep = (out) => /border-(top-left|start-start)-radius/.test(out);
export const cases = [
  {name: 'reported: nested :where(&) + var() radius, chrome 80',
   css: `.btn1 { :where(&) { width: unset; } border-start-start-radius: var(--join-ss, var(--radius-field)); }`,
   options: {targets: T80}, check: keep},
  {name: 'nested rule + literal radius, chrome 80',
   css: `.btn1 { :where(&) { width: unset; } border-start-start-radius: 4px; }`,
   options: {targets: T80}, check: keep},
  {name: 'nested rule + margin-inline-start (other logical), chrome 60',
   css: `.btn1 { .x & { width: unset; } margin-inline-start: var(--m); }`,
   options: {targets: {chrome: 60 << 16}}, check: (o) => /margin-(left|inline-start)/.test(o)},
  {name: 'declaration before nested rule, chrome 80',
   css: `.btn1 { border-start-start-radius: var(--r); &:hover { width: unset; } }`,
   options: {targets: T80}, check: keep},
  {name: 'minified, chrome 80',
   css: `.btn1 { :where(&) { width: unset; } border-start-start-radius: var(--r); }`,
   options: {targets: T80, minify: true}, check: keep},
  {name: '#1246 variant: fallback must not leak into the next nested rule (safari 14)',
   css: `.btn { &:focus { color: blue } border-start-start-radius: 8px; &:hover { color: red } }`,
   options: {targets: {safari: 14 << 16}},
   // base .btn must get the physical radius, and no :hover rule may carry it
   check: (o) => /\.btn:not\(:lang[^{]*\{\s*border-top-left-radius/.test(o) && !/:hover[^{]*\{\s*border-top/.test(o)},
  {name: '#1023 variant: logical property inside nested @media (chrome 68)',
   css: `h1 { margin-inline-start: 1px; @media (width < 10px) { color: red; margin-inline-start: 2px; } }`,
   options: {targets: {chrome: 68 << 16}},
   check: (o) => /@media[^{]*\{[^@]*margin-(left|right): 2px/.test(o)},
  {name: 'control: no nested rule, chrome 80',
   css: `.btn2 { border-start-start-radius: var(--r); }`,
   options: {targets: T80}, check: keep},
  {name: 'control: nested rule, chrome 120 (supports logical radius + nesting lowered)',
   css: `.btn1 { :where(&) { width: unset; } border-start-start-radius: var(--r); }`,
   options: {targets: T120}, check: (o) => o.includes('border-start-start-radius')},
];
