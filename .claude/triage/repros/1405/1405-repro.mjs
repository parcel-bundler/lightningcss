// #1405: a top-level NaN calculation must not serialize as a huge finite length.
// Desired: censored to 0 (or kept as calc(NaN * 1px)), never ~5.1e38.
export const cases = [
  {name: 'margin-left', css: '.a { margin-left: calc(NaN * 1px) }', options: {minify: true},
    check: (out) => /margin-left:(0|calc\(NaN \* 1px\))(\}|;)/.test(out)},
  {name: 'media-min-width', css: '@media (min-width: calc(NaN * 1px)) { .a { color: red } }', options: {minify: true},
    check: (out) => !out.includes('e38') && !out.startsWith('ERROR')},
  {name: 'container-min-width', css: '@container (min-width: calc(NaN * 1px)) { .a { color: red } }', options: {minify: true},
    check: (out) => !out.includes('e38') && !out.startsWith('ERROR')},
  {name: 'nan-number-opacity', css: '.a { opacity: calc(NaN) }', options: {minify: true},
    check: (out) => !out.includes('e38') && !out.startsWith('ERROR')},
  {name: 'nan-rotate', css: '.a { rotate: calc(NaN * 1deg) }', options: {minify: true},
    check: (out) => !out.includes('e38') && !out.startsWith('ERROR')},
  {name: 'nan-inside-kept-calc', css: '.a { margin-left: calc(NaN * 1px + 1em) }', options: {minify: true},
    // Browser computes 0px (NaN propagates, then censored). Must not print a finite huge px term.
    check: (out) => !out.includes('e38') && !out.startsWith('ERROR')},
  // Controls: infinity clamps to the largest finite value, which is equivalent in browsers.
  {name: 'control-infinity', css: '.a { margin-left: calc(infinity * 1px) }', options: {minify: true},
    check: (out) => !out.startsWith('ERROR') && !/NaN/i.test(out)},
  {name: 'control-width-nan', css: '.a { width: calc(NaN * 1px) }', options: {minify: true},
    check: (out) => /width:(0|calc\(NaN \* 1px\))(\}|;)/.test(out)},
];
