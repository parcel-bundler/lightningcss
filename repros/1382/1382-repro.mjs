// #1382: token-list parsing (custom properties, and any property whose value falls back to
// unparsed tokens because of var()) takes time doubling per nesting level when
// UnresolvedColor::parse fails after fully parsing its arguments and the generic function
// path parses them again. Depths are modest: a broken build takes ~0.5s (release) / ~5-9s
// (debug) per case, a fixed build a few ms. Output records only fast/slow plus the result kind.
const BUDGET_MS = 100;
const timed = (lightningcss, code, expect) => {
  const t = performance.now();
  let out;
  try {
    out = lightningcss.transform({filename: 'x.css', code: Buffer.from(code), minify: true, errorRecovery: true}).code.toString();
  } catch (e) {
    out = 'ERROR';
  }
  const ms = performance.now() - t;
  return `${ms < BUDGET_MS ? 'fast' : 'slow'}; ${expect(out) ? 'expected output' : 'unexpected output: ' + out.slice(0, 200)}`;
};
const ok = (out) => out === 'fast; expected output';
const rep = (s, n) => s.repeat(n);

export const cases = [
  {
    name: 'reporter: --x: light-dark( x20 var(--a) )',
    run: (l) => {
      const v = `${rep('light-dark(', 20)}var(--a)${rep(')', 20)}`;
      return timed(l, `.a{--x:${v}}`, (out) => out === `.a{--x:${v}}`);
    },
    check: ok,
  },
  {
    name: 'typed property: color: light-dark( x20 var(--a) )',
    run: (l) => {
      const v = `${rep('light-dark(', 20)}var(--a)${rep(')', 20)}`;
      return timed(l, `.a{color:${v}}`, (out) => out === `.a{color:${v}}`);
    },
    check: ok,
  },
  {
    // Invalid (stray `]`): dropped or errors either way; only the time matters.
    name: 'with comma and an error token: light-dark(red, x19 ... ])',
    run: (l) => timed(l, `.a{--x:${rep('light-dark(red, ', 19)}var(--a) ]${rep(')', 19)}}`, () => true),
    check: ok,
  },
  {
    // Not touched by PR #1383: same pattern through the rgb()/hsl() relative-alpha path.
    name: 'rgb(0 0 0 / x18 ... ]) alpha path',
    run: (l) => timed(l, `.a{--x:${rep('rgb(0 0 0 / ', 18)}var(--a) ]${rep(')', 18)}}`, () => true),
    check: ok,
  },
  {
    name: 'control: valid nested light-dark(..., blue) x20',
    run: (l) => {
      let v = 'red';
      for (let i = 0; i < 20; i++) v = `light-dark(${v}, blue)`;
      return timed(l, `.a{--x:${v}}`, (out) => out.startsWith('.a{--x:light-dark('));
    },
    check: ok,
  },
];
