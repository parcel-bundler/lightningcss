// #1392: a fallback declaration is dropped before a relative color that
// lightningcss keeps as tokens (origin currentColor), even for targets without
// relative color syntax (Chrome < 119). check() = behavior we want.
const old = {minify: true, targets: {chrome: 100 << 16}};
const modern = {minify: true, targets: {chrome: 130 << 16}};
const keepsBoth = (prop, fb) => (out) => out.includes(`${prop}:${fb};${prop}:`);
export const cases = [
  {
    name: 'color-currentColor-old-target',
    css: `.a{color:blue;color:lch(from currentColor l c h)}`,
    options: old,
    check: keepsBoth('color', '#00f'),
  },
  {
    // The plausible pattern: translucent currentColor.
    name: 'color-currentColor-alpha-old-target',
    css: `.a{color:#0003;color:rgb(from currentColor r g b / 20%)}`,
    options: old,
    check: keepsBoth('color', '#0003'),
  },
  {
    name: 'fill-currentColor-alpha-old-target',
    css: `.a{fill:#0003;fill:rgb(from currentColor r g b / 20%)}`,
    options: old,
    check: keepsBoth('fill', '#0003'),
  },
  {
    // color-mix() is also kept as tokens here (Chrome 111).
    name: 'color-mix-currentColor-old-target',
    css: `.a{color:#0003;color:color-mix(in srgb, currentColor 20%, transparent)}`,
    options: old,
    check: keepsBoth('color', '#0003'),
  },
  {
    // Other FallbackHandler properties (text-shadow, stroke, caret-color, filter...) share the bug.
    name: 'text-shadow-color-mix-old-target',
    css: `.a{text-shadow:0 0 1px #0003;text-shadow:0 0 1px color-mix(in srgb, currentColor 20%, transparent)}`,
    options: old,
    check: keepsBoth('text-shadow', '0 0 1px #0003'),
  },
  {
    // Control: border-color already keeps the fallback.
    name: 'control-border-color-old-target',
    css: `.a{border-color:#0003;border-color:rgb(from currentColor r g b / 20%)}`,
    options: old,
    check: keepsBoth('border-color', '#0003'),
  },
  {
    // Control: with var() the browser accepts the value at parse time
    // (invalid at computed-value time), so the fallback never applies; dropping it is fine.
    name: 'control-var-origin-drops-fallback',
    css: `.a{color:blue;color:lch(from var(--c) l c h)}`,
    options: old,
    check: (out) => out === '.a{color:lch(from var(--c) l c h)}',
  },
  {
    // Control: modern targets support relative colors, so dropping the fallback is fine.
    name: 'control-modern-target-drops-fallback',
    css: `.a{color:blue;color:lch(from currentColor l c h)}`,
    options: modern,
    check: (out) => out === '.a{color:lch(from currentColor l c h)}',
  },
  {
    // Control: a resolvable relative color is lowered for old targets.
    name: 'control-resolvable-relative-color',
    css: `.a{color:blue;color:lch(from red l c h)}`,
    options: old,
    check: (out) => !out.includes('from'),
  },
];
