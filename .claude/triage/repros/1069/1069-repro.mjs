// #1069: range syntax in container style queries (css-conditional-5 <style-range>).
// Shipped in Chrome 142 and Firefox 151 (MDN BCD 8.0.7), so it should parse in strict mode.

export const cases = [
  {
    name: 'reporter-dimension-range',
    css: `@container style(1em < 20px){.a{color:red}}`,
    options: {minify: true},
    check: out => !out.startsWith('ERROR') && !out.includes('warnings:') && /style\(1em ?< ?20px\)/.test(out),
  },
  {
    name: 'custom-property-range',
    css: `@container style(--x > 10){.a{color:red}}`,
    options: {minify: true},
    check: out => !out.startsWith('ERROR') && !out.includes('warnings:') && /style\(--x ?> ?10\)/.test(out),
  },
  {
    name: 'interval-range-named-container',
    css: `@container card style(10 < --x <= 20){.a{color:red}}`,
    options: {minify: true},
    check: out => !out.startsWith('ERROR') && !out.includes('warnings:') && /style\(10 ?< ?--x ?<= ?20\)/.test(out),
  },
  {
    name: 'range-inside-boolean-combination',
    css: `@container style((--x > 1) or (--y: 2)){.a{color:red}}`,
    options: {minify: true},
    check: out => !out.startsWith('ERROR') && !out.includes('warnings:') && /--x ?> ?1/.test(out) && out.includes('--y:2'),
  },
  {
    // Control: errorRecovery keeps the rule as written, with a warning.
    name: 'control-error-recovery-passthrough',
    css: `@container style(--x > 10){.a{color:red}}`,
    options: {minify: true, errorRecovery: true},
    check: out => /@container style\(--x ?> ?10\)\{\.a\{color:red\}\}/.test(out),
  },
  {
    // Control: plain and boolean style features already work.
    name: 'control-plain-style-feature',
    css: `@container style((--x: 1) or (--y)){.a{color:red}}`,
    options: {minify: true},
    check: out => out.trim() === '@container style((--x:1) or (--y)){.a{color:red}}',
  },
];
