// #1221: `font-family: monospace, monospace` must not be reduced to a lone `monospace`.
// A lone generic `monospace` triggers the browser's smaller default monospace size
// (Chromium 153: 13px instead of 16px, and 26px instead of 32px for 2em); a list of two
// does not. normalize.css 8.0.1 uses exactly `font-family: monospace, monospace; font-size: 1em`.
// check(): true when the family list still has more than one entry / unrelated dedupe still works.
const fam = (out) => {
  let m = out.match(/font(?:-family)?:([^;}]*)/);
  return m ? m[1].replace(/^[^a-z"]*?(?:\d[\w.%]*\s+)/i, '').split(',').map(s => s.trim()) : [];
};
const twoPlus = (out) => !out.startsWith('ERROR') && fam(out).length >= 2;
export const cases = [
  {name: 'normalize-pre-minify', css: `pre{font-family:monospace,monospace;font-size:1em}`, options: {minify: true}, check: twoPlus},
  {name: 'normalize-pre-no-minify', css: `pre{font-family:monospace,monospace;font-size:1em}`, options: {minify: false}, check: twoPlus},
  {name: 'triple-monospace', css: `code{font-family:monospace,monospace,monospace}`, options: {minify: true}, check: twoPlus},
  {name: 'font-shorthand-em', css: `code{font:1em monospace,monospace}`, options: {minify: true}, check: twoPlus},
  // Controls: these are correct today and should stay that way.
  {name: 'control-named-dedupe', css: `code{font-family:Menlo,Menlo,monospace}`, options: {minify: true}, check: (o) => o.includes('font-family:Menlo,monospace}')},
  {name: 'control-with-fallback', css: `code{font-family:monospace,monospace,serif}`, options: {minify: true}, check: twoPlus},
  {name: 'control-single', css: `code{font-family:monospace}`, options: {minify: true}, check: (o) => o.includes('font-family:monospace}')},
];
