// #828: "disable the --lightningcss-light/dark variables". Triage: already possible via
// `exclude: Features.LightDark` or targets that support light-dark() natively. Controls only.
const LightDark = (l) => l.Features.LightDark;
const css = `:root{color-scheme:light dark}.a{color:light-dark(#fff,#000)}`;
export const cases = [
  {
    name: 'control: exclude Features.LightDark removes toggles and keeps light-dark()',
    run: (l) => l.transform({filename: 'a.css', code: Buffer.from(css), minify: true, targets: {chrome: 90 << 16}, exclude: LightDark(l)}).code.toString(),
    check: (out) => !out.includes('lightningcss-') && out.includes('light-dark(#fff,#000)'),
  },
  {
    name: 'control: native targets (safari 17.5) emit no toggles',
    css,
    options: {minify: true, targets: {safari: (17 << 16) | (5 << 8)}},
    check: (out) => !out.includes('lightningcss-') && out.includes('light-dark('),
  },
  {
    name: 'control: just below support (safari 17.4) is polyfilled',
    css,
    options: {minify: true, targets: {safari: (17 << 16) | (4 << 8)}},
    check: (out) => out.includes('--lightningcss-light:initial'),
  },
];
