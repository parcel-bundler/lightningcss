// #1398: `-0` number tokens in unparsed values print as `0`. Per css-values-4 #calc-ieee,
// "''0'', ''+0'' and ''-0'' all produce the standard unsigned zero", so this is equivalent per spec
// (Chromium 153 deviates: it treats a literal -0 as 0⁻). Triage: works as intended.
// Controls: the sign of non-zero dimensions/numbers in token lists must be preserved,
// and a literal -0 must print as some zero.
export const cases = [
  {
    name: 'control-negative-dimension-kept',
    css: `.a{margin-left:var(--y, calc(-1px / 2))}.b{--x:calc(1px / -2)}`,
    options: {minify: true},
    check: (out) => out.includes('-1px') && out.includes('-2'),
  },
  {
    name: 'control-minus-zero-is-zero',
    css: `.a{margin-left:calc(1px / (-0 * var(--z)))}`,
    options: {minify: true},
    check: (out) => /calc\(1px ?\/ ?\(-?0 ?\* ?var\(--z\)\)\)/.test(out),
  },
];
