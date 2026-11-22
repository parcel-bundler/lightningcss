// #873: reporter's claims + follow-ups. Triage: the `dark only` order is valid (works as intended);
// the theme-class case is the same root cause as #821 (tokens computed on :root, color-scheme
// changed on a descendant). Desired for that case: native light-dark() reachable in modern browsers.
const chrome90 = {chrome: 90 << 16};
const themeCss = `:root{color-scheme:light dark;--white:#fff;--black:#000;--bg:light-dark(var(--white),var(--black));--fg:light-dark(var(--black),var(--white))}
body{background-color:var(--bg);color:var(--fg)}
.dark{color-scheme:only dark;background-color:var(--bg);color:var(--fg)}
.light{color-scheme:only light;background-color:var(--bg);color:var(--fg)}`;
export const cases = [
  {
    name: 'theme classes on descendants (same root cause as #821)',
    css: themeCss,
    options: {minify: true, targets: chrome90},
    check: (out) => /--bg:light-dark\(var\(--white\),var\(--black\)\)/.test(out),
  },
  {
    name: 'control: `only dark` serializes as `dark only` (valid: && allows any order)',
    css: `.dark{color-scheme:only dark}.light{color-scheme:light only}`,
    options: {minify: true},
    check: (out) => out === '.dark{color-scheme:dark only}.light{color-scheme:light only}',
  },
  {
    name: 'control (comment): toggles emitted for color-scheme even without light-dark() (needed across separately compiled files, see #708)',
    css: `html{color-scheme:light}html.dark{color-scheme:dark}`,
    options: {minify: true, targets: chrome90},
    check: (out) => out.includes('html{--lightningcss-light:initial;--lightningcss-dark: ;color-scheme:light}') && out.includes('html.dark{--lightningcss-light: ;--lightningcss-dark:initial;color-scheme:dark}'),
  },
];
