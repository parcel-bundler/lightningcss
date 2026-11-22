// #592: lower oklch()/oklab()/lab()/lch() whose alpha is var(), like rgb()/hsl() already are.
// Wanted: an sRGB fallback that keeps the var() alpha, plus @supports-guarded p3/lab versions.
const targets = {chrome: 90 << 16, safari: 14 << 16}; // no oklch/lab support
const hasSrgbFallback = out => /color:rgba?\(255[ ,]+219[ ,]+218[ ,/]+var\(--o\)\)/.test(out) && out.includes('@supports');

export const cases = [
  {
    name: 'oklch-var-alpha-lowered',
    css: `.a{color:oklch(92.19% 0.04 20 / var(--o))}`,
    options: {minify: true, targets},
    check: hasSrgbFallback,
  },
  {
    name: 'oklab-var-alpha-lowered',
    css: `.a{color:oklab(0.9219 0.0376 0.0137 / var(--o))}`,
    options: {minify: true, targets},
    check: out => /color:rgba?\(/.test(out) && out.includes('var(--o)') && out.includes('@supports'),
  },
  {
    name: 'lab-var-alpha-lowered',
    css: `.a{color:lab(90.5374% 12.6997 5.39228 / var(--o))}`,
    options: {minify: true, targets},
    check: out => /color:rgba?\(/.test(out) && out.includes('var(--o)'),
  },
  {
    // Control: modern targets need no lowering; output keeps oklch with the var alpha.
    name: 'control-modern-targets-unchanged',
    css: `.a{color:oklch(92.19% 0.04 20 / var(--o))}`,
    options: {minify: true, targets: {chrome: 120 << 16, safari: 17 << 16}},
    check: out => out.trim() === '.a{color:oklch(92.19% .04 20/var(--o))}' || out.trim() === '.a{color:oklch(92.19% .04 20 / var(--o))}',
  },
  {
    // Control: var() in the color components can't be lowered statically; must pass through.
    name: 'control-var-components-passthrough',
    css: `.a{color:oklch(var(--c) / .5)}`,
    options: {minify: true, targets},
    check: out => out.includes('oklch(var(--c)'),
  },
  {
    // Control: the existing rgb() var-alpha lowering.
    name: 'control-rgb-var-alpha-legacy',
    css: `.a{color:rgb(255 0 0 / var(--o))}`,
    options: {minify: true, targets: {safari: 11 << 16}},
    check: out => out.includes('rgba(255,0,0,var(--o))'),
  },
];
