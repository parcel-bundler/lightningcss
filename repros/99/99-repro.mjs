// #99 CSS Color Level 5 umbrella. One case per feature.
// check() = the behavior we want: supported features are parsed and resolved
// at build time; at-risk features no browser ships (device-cmyk(),
// @color-profile, custom color spaces) must pass through unchanged.
const min = {minify: true};
const old = {minify: true, targets: {safari: 15 << 16}}; // no color-mix/RCS/light-dark/contrast-color
const noFn = (name) => (o) => !o.startsWith('ERROR') && !o.includes(name + '(');

export const cases = [
  // --- Supported today (controls) ---
  {name: 'color-mix-two-colors', css: `.a{color:color-mix(in srgb, red 40%, blue)}`, options: min, check: (o) => o === '.a{color:#609}'},
  {name: 'relative-color-rgb', css: `.a{color:rgb(from red r g b / 50%)}`, options: min, check: (o) => o === '.a{color:#ff000080}'},
  {name: 'relative-color-color-fn', css: `.a{color:color(from red srgb r g b)}`, options: min, check: (o) => o === '.a{color:color(srgb 1 0 0)}'},
  {name: 'alpha-function', css: `.a{color:alpha(from red / 50%)}`, options: min, check: (o) => o === '.a{color:#ff000080}'},
  {name: 'light-dark-lowered', css: `.a{color:light-dark(red, blue)}`, options: old, check: (o) => o.includes('var(--lightningcss-light,red)') && o.includes('var(--lightningcss-dark,#00f)')},
  {name: 'light-dark-image-lowered', css: `.a{background-image:light-dark(url(a.png), url(b.png))}`, options: old, check: (o) => o.includes('var(--lightningcss-light,url(a.png))')},

  // --- Missing ---
  // contrast-color(): Chrome 147, Firefox 146, Safari 26. Open PRs #808, #1160.
  // Chrome 153 resolves contrast-color(red) and contrast-color(#777) to black.
  {name: 'contrast-color-resolved', css: `.a{color:contrast-color(red)}`, options: min, check: (o) => o === '.a{color:#000}'},
  {name: 'contrast-color-lowered-old-target', css: `.a{color:contrast-color(#777)}`, options: old, check: (o) => o === '.a{color:#000}'},
  {name: 'contrast-color-var-preserved', css: `.a{color:contrast-color(var(--bg))}`, options: min, check: (o) => o === '.a{color:contrast-color(var(--bg))}'},
  // Optional interpolation method (defaults to oklab), new in the 2026 draft.
  {name: 'color-mix-default-oklab', css: `.a{color:color-mix(red, blue)}`, options: old, check: noFn('color-mix')},
  // More than two colors (Firefox 150; Chrome 153 rejects it).
  {name: 'color-mix-variadic', css: `.a{color:color-mix(in srgb, red, blue, lime)}`, options: old, check: noFn('color-mix')},
  // display-p3-linear color space (Chrome 144, Firefox 146, Safari 26.2).
  {name: 'display-p3-linear-color', css: `.a{color:color(display-p3-linear 1 0 0)}`, options: old, check: noFn('color')},
  {name: 'display-p3-linear-mix', css: `.a{color:color-mix(in display-p3-linear, red, blue)}`, options: old, check: noFn('color-mix')},

  // --- At-risk, no browser support: must pass through unchanged ---
  {name: 'device-cmyk-passthrough', css: `.a{color:device-cmyk(0 81% 81% 30%)}`, options: min, check: (o) => o.startsWith('.a{color:device-cmyk(0 81% 81% 30%)}')},
  {name: 'color-profile-passthrough', css: `@color-profile --swop { src: url(swop.icc) }\n.a{color:color(--swop 0 .5 1 .2)}`, options: {...min, errorRecovery: true}, check: (o) => o.includes('@color-profile --swop') && o.includes('color(--swop 0 .5 1 .2)')},
];
