// #1389: nested math functions containing var() parse in time doubling per level
// (Calc::parse_value tries Calc::parse, then V::parse re-parses the same function).
// Depths are modest so a broken build takes ~0.5s (release) / ~8s (debug) per case,
// while a fixed build takes a few ms. The budget is generous for a fixed build and
// several times below a broken release build. Output records only fast/slow plus
// whether the value round-trips, so identical outputs group across versions.
const BUDGET_MS = 100;
const nest = (inner, d, wrap) => { let v = inner; for (let i = 0; i < d; i++) v = wrap(v); return v; };
const timed = (lightningcss, code, expectIncludes) => {
  const t = performance.now();
  let out;
  try {
    out = lightningcss.transform({filename: 'x.css', code: Buffer.from(code), minify: true}).code.toString();
  } catch (e) {
    out = 'ERROR: ' + e.message;
  }
  const ms = performance.now() - t;
  return `${ms < BUDGET_MS ? 'fast' : 'slow'}; ${out.includes(expectIncludes) ? 'value preserved' : 'value changed: ' + out}`;
};
const ok = (out) => out === 'fast; value preserved';

export const cases = [
  {
    name: 'calc() x17 around var() in width',
    run: (l) => {
      const v = nest('var(--x)', 17, (v) => `calc(${v} - 1px)`);
      return timed(l, `.x{width:${v}}`, v);
    },
    check: ok,
  },
  {
    name: 'min() x17 around var() in width',
    run: (l) => {
      const v = nest('var(--x)', 17, (v) => `min(${v}, 1px)`);
      return timed(l, `.x{width:${v}}`, 'var(--x)');
    },
    check: ok,
  },
  {
    name: 'control: calc() x17 without var()',
    run: (l) => timed(l, `.x{width:${nest('10px', 17, (v) => `calc(${v} + 1px)`)}}`, 'width:27px'),
    check: ok,
  },
  {
    name: 'control: same value in a custom property',
    run: (l) => {
      const v = nest('var(--x)', 17, (v) => `calc(${v} - 1px)`);
      return timed(l, `.x{--w:${v}}`, 'var(--x)');
    },
    check: ok,
  },
];
