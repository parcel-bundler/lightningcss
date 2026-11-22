// #1393: adding a length to a long non-folding calc() sum is quadratic
// (DimensionPercentage::add_recursive deep-clones the left spine at each level).
// check() = the transform finishes within a generous time budget. On master,
// N=2000 takes ~1.7 s; a linear implementation takes ~20 ms (see the controls).
const N = 2000;
const BUDGET_MS = 500;
function timed(lightningcss, css) {
  let start = performance.now();
  lightningcss.transform({filename: 'a.css', minify: true, code: Buffer.from(css)});
  let ms = performance.now() - start;
  return ms < BUDGET_MS ? `within ${BUDGET_MS}ms budget` : `over ${BUDGET_MS}ms budget`;
}
const terms = (t) => Array(N).fill(t).join(' + ');
const within = (out) => out.startsWith('within');
export const cases = [
  {
    name: 'min-terms-plus-length',
    run: (l) => timed(l, `.a{width:calc(${terms('min(1px, 1em)')} + 1px)}`),
    check: within,
  },
  {
    // Control: length first is found at the leftmost leaf, so it is fast today.
    name: 'control-length-first',
    run: (l) => timed(l, `.a{width:calc(1px + ${terms('min(1px, 1em)')})}`),
    check: within,
  },
  {
    // Control: same sum without the trailing length is linear today.
    name: 'control-no-trailing-length',
    run: (l) => timed(l, `.a{width:calc(${terms('min(1px, 1em)')})}`),
    check: within,
  },
  {
    // Control: var() terms with a trailing length are linear today.
    name: 'control-var-terms',
    run: (l) => timed(l, `.a{width:calc(${terms('var(--x)')} + 1px)}`),
    check: within,
  },
];
