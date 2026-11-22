// #568: system-ui fallback font stack (src/properties/font.rs: compatible_font_family).
// Triage: the stack is only meant for targets without system-ui support. Controls
// pin that. The android case tracks a compat-data artifact found during triage:
// caniuse lists only the current Android WebView version (150 at generation time)
// as supported, so `android` targets below 150 (e.g. browserslist `defaults` with an
// older caniuse-lite db -> android 131) get the stack although MDN says WebView 56+.
const css = `.a { font-family: ui-sans-serif, system-ui, sans-serif }`;
const STACK = '-apple-system';
const t = (o) => ({minify: true, targets: o});

export const cases = [
  {name: 'no-targets-unchanged', css, options: {minify: true}, check: (out) => !out.includes(STACK)},
  {name: 'modern-chrome-unchanged', css, options: t({chrome: 120 << 16}), check: (out) => !out.includes(STACK)},
  {name: 'old-chrome-gets-fallback-stack', css, options: t({chrome: 50 << 16}),
    check: (out) => out.includes(STACK) && out.includes('system-ui')},
  // Wanted: no stack (MDN webview_android system-ui: 56). Fails on 1.33.0/master.
  {name: 'android-131-unchanged', css, options: t({android: 131 << 16}), check: (out) => !out.includes(STACK)},
];
