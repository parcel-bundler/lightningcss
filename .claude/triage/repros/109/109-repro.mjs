// #109: hand-picked fallbacks must not be overwritten by generated ones.
// Triage decision: the core request (keep a manual fallback declared before a modern value in the
// same block, and don't generate fallbacks inside a matching @supports) is implemented; these are
// controls pinning it. Plus one open sub-case: the reporter's original pattern (custom property
// overridden inside a nested @media) loses the display-p3 value entirely since 1.30.0 (see #1381/#1386).
const t95 = {chrome: 95 << 16};      // no display-p3 (Chrome 111+)
const t120 = {chrome: 120 << 16};    // has display-p3
const norm = (s) => s.replace(/\s+/g, ' ');
export const cases = [
  {name: 'control: manual color fallback kept (chrome 95)',
   css: `.a { color: #4263eb; color: color(display-p3 0 .5 1); }`, options: {targets: t95},
   check: (o) => norm(o).includes('color: #4263eb; color: color(display-p3 0 .5 1);') && !o.includes('@supports')},
  {name: 'control: modern value kept, no generated fallback, when all targets support p3 (chrome 120)',
   // (the now-unneeded manual #4263eb is currently kept too; harmless, only bytes)
   css: `.a { color: #4263eb; color: color(display-p3 0 .5 1); }`, options: {targets: t120, minify: true},
   check: (o) => o.endsWith('color:color(display-p3 0 .5 1)}') && !o.includes('@supports')},
  {name: 'control: nested @supports with p3 kept as-is, manual fallback kept (chrome 95)',
   css: `:where(html) { --link: #4263eb; @supports (color: color(display-p3 0 0 0)) { --link: color(display-p3 0 .5 1); } }`,
   options: {targets: t95},
   check: (o) => o.includes('--link: #4263eb') && o.includes('--link: color(display-p3 0 .5 1)') && !o.includes('#0083ff')},
  {name: 'control: top-level @supports block not given an extra fallback (chrome 95)',
   css: `@supports (color: color(display-p3 0 0 0)) { .d { color: color(display-p3 0 .5 1); } }`,
   options: {targets: t95, minify: true},
   check: (o) => o === '@supports (color:color(display-p3 0 0 0)){.d{color:color(display-p3 0 .5 1)}}'},
  {name: 'control: manual gradient fallback kept (chrome 95)',
   css: `.e { background: red; background: linear-gradient(lch(50% 132 50), lch(50% 130 150)); }`,
   options: {targets: t95},
   check: (o) => o.includes('background: red') && o.includes('lch(50% 132 50)')},
  {name: 'control: manual url() fallback before image-set kept (chrome 95)',
   css: `.f { background-image: url(a.png); background-image: image-set(url(a.png) 1x, url(b.png) 2x); }`,
   options: {targets: {chrome: 20 << 16}},
   check: (o) => /background-image: url\("?a\.png"?\)/.test(o)},
  {name: 'reported pattern: p3 value inside nested @media must still reach modern browsers (chrome 95)',
   css: `:where(html) { --link: #4263eb; @media (dynamic-range: high) { --link: color(display-p3 0 .5 1); } }`,
   options: {targets: t95},
   check: (o) => o.includes('--link: #4263eb') && o.includes('color(display-p3 0 .5 1)')},
];
