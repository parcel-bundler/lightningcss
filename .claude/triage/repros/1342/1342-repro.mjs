// #1342 (duplicate of #1283): duration-less shorthand + scroll(root y) folded into `animation`.
// Fix PR #1306. check() = timeline kept as its own longhand after the shorthand.
const sep = (o) => !/animation:[^;}]*scroll\(/.test(o) && o.indexOf('animation-timeline') > o.indexOf('animation:');
const css = `.cloud{animation:cloud-pan ease-in-out both;animation-timeline:scroll(root y)}`;
export const cases = [
  {name: 'no targets (Astro/Vite default path)', css, options: {minify: true}, check: sep},
  {name: 'chrome 120 target', css, options: {minify: true, targets: {chrome: 120 << 16}}, check: sep},
  // control: `y` axis is preserved (not normalized to an invalid value)
  {name: 'control: axis y preserved', css, options: {minify: true, targets: {safari: 18 << 16}}, check: (o) => /scroll\(root y\)/.test(o)},
];
