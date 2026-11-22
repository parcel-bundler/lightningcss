// #821: light-dark() polyfill in custom properties ignores color-scheme set on a descendant.
// Desired (per triage): in browsers that support light-dark() natively, the custom property
// keeps native light-dark() (e.g. via an @supports override, as in open PR #1287), so it
// resolves against the color-scheme of the element that uses it. Old browsers keep the polyfill.
const chrome90 = {chrome: 90 << 16};
export const cases = [
  {
    name: 'reporter: tokens on :root, color-scheme on child',
    css: `:root{--background:light-dark(white,black);--text:light-dark(black,white)}
p{color:var(--text);background:var(--background);color-scheme:dark}`,
    options: {minify: true, targets: chrome90},
    // Wanted: a native light-dark() value reachable by modern browsers.
    check: (out) => /--background:light-dark\(/.test(out) && /--text:light-dark\(/.test(out),
  },
  {
    name: 'control: tokens on same element as color-scheme get toggles + dark media rule',
    css: `:root{color-scheme:light dark;--bg:light-dark(#fff,#000)}`,
    options: {minify: true, targets: chrome90},
    check: (out) => out.includes('--lightningcss-light:initial') && /@media \(prefers-color-scheme:dark\)\{:root\{--lightningcss-light: ;--lightningcss-dark:initial\}\}/.test(out),
  },
  {
    name: 'control (comment): color-scheme:inherit preserved (#1152)',
    css: `:host{color-scheme:inherit}`,
    options: {minify: true, targets: chrome90},
    check: (out) => out.includes('color-scheme:inherit') && !out.includes('normal'),
  },
  {
    name: 'control (comment): html/[data-theme] example gets toggles when targets are passed to transform',
    css: `html{color-scheme:light dark}html[data-theme=light]{color-scheme:light}html[data-theme=dark]{color-scheme:dark}button{background:light-dark(#aaa,#444)}`,
    options: {minify: true, targets: {chrome: 96 << 16}},
    check: (out) => out.includes('html{--lightningcss-light:initial;--lightningcss-dark: ;color-scheme:light dark}') && out.includes('html[data-theme=dark]{--lightningcss-light: ;--lightningcss-dark:initial'),
  },
  {
    name: 'control: native targets (chrome 123) leave light-dark() untouched',
    css: `:root{--bg:light-dark(white,black)}p{color-scheme:dark}`,
    options: {minify: true, targets: {chrome: 123 << 16}},
    check: (out) => out.includes('--bg:light-dark(') && !out.includes('lightningcss-'),
  },
];
