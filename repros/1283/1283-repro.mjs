// #1283 (dups #1342, #1369; sub-case of #572): animation-timeline folded into the `animation` shorthand.
// Chrome 115+ only treats animation-timeline as a reset-only sub-property (BCD partial_implementation),
// so `animation: ... view()` is an invalid declaration in every engine and the animation is dropped.
// Fix PR #1306. check() = timeline stays a separate longhand placed after the shorthand.
const C = (v) => v << 16;
const sep = (o) => !/animation:[^;}]*(scroll|view)\(/.test(o) && /animation-timeline:/.test(o)
  && o.indexOf('animation-timeline') > o.indexOf('animation:');
const src = `img{animation:parallax 1ms linear both;animation-timeline:view();animation-range:exit 0% exit 100%}`;
export const cases = [
  {name: 'chrome115 target (reporter)', css: src, options: {minify: true, targets: {chrome: C(115)}}, check: sep},
  {name: 'chrome130 not minified', css: src, options: {minify: false, targets: {chrome: C(130)}}, check: sep},
  {name: 'scroll(root) chrome120', css: `.a{animation:x linear both;animation-timeline:scroll(root);animation-range:0 100px}`,
    options: {minify: true, targets: {chrome: C(120)}}, check: sep},
  // Decision point: with no targets, LCSS assumes a "future perfect" browser; the spec grammar does allow a
  // timeline in the shorthand, but no engine implements it. PR #1306 keeps the longhand here too.
  {name: 'no targets', css: src, options: {minify: true}, check: sep},
  // Controls (correct today): targets that include a non-Chromium engine or Chrome < 115.
  {name: 'control: chrome114', css: src, options: {minify: true, targets: {chrome: C(114)}}, check: sep},
  {name: 'control: chrome130+safari18', css: src, options: {minify: true, targets: {chrome: C(130), safari: C(18)}}, check: sep},
  // Control: timeline declared BEFORE the shorthand is reset by it; must not be resurrected or reordered after it.
  {name: 'control: timeline before shorthand', css: `.a{animation-timeline:view();animation:x 1s}`,
    options: {minify: true, targets: {chrome: C(130)}}, check: (o) => !/view\(/.test(o) || o.indexOf('view(') < o.indexOf('animation:')},
];
