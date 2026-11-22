// #826: -webkit-fill-available / -moz-available / stretch handling in the size handler.
// Triage decision: dropping prefixed fallbacks when no targets are given is documented
// behavior ("no targets = a browser with full standards support"), so that case is a control.
// The bugs pinned here: collapsing two *different* prefixed values into the last one.
const all3 = `.a{width:-webkit-fill-available;width:-moz-available;width:stretch}`;
const defaults = (l, require) => l.browserslistToTargets(require('browserslist')('defaults'));

export const cases = [
  {
    // Bug: two prefixed values, no standard one. Keeping only the last one breaks
    // Chromium/WebKit (-moz-available is invalid there; verified in Chrome 153).
    name: 'no-targets-webkit-then-moz-keeps-webkit',
    css: `.a{width:-webkit-fill-available;width:-moz-available}`,
    options: {minify: true, targets: {}},
    check: out => out.includes('-webkit-fill-available') && out.includes('-moz-available'),
  },
  {
    // Bug (even with targets): Size::is_compatible ignores the prefix of max-content,
    // so -webkit-max-content is dropped in favor of -moz-max-content.
    name: 'defaults-webkit-moz-max-content-keeps-both',
    run: (l, {require}) => l.transform({filename: 'a.css', code: Buffer.from(`.a{width:-webkit-max-content;width:-moz-max-content}`), minify: true, targets: defaults(l, require)}).code.toString(),
    check: out => out.includes('-webkit-max-content') && out.includes('-moz-max-content'),
  },
  {
    // Control: with real targets all three values are kept.
    name: 'control-defaults-targets-keep-all3',
    run: (l, {require}) => l.transform({filename: 'a.css', code: Buffer.from(all3), minify: true, targets: defaults(l, require)}).code.toString(),
    check: out => out.includes('-webkit-fill-available') && out.includes('-moz-available') && out.includes('stretch'),
  },
  {
    // Control: the reporter's case (Vite build.target esnext passes targets: {}). Documented
    // behavior: no targets means no fallbacks; output is `stretch` only.
    name: 'control-no-targets-all3-collapses-to-stretch',
    css: all3,
    options: {minify: true, targets: {}},
    check: out => out.trim() === '.a{width:stretch}',
  },
  {
    // Control: a lone prefixed value is kept.
    name: 'control-lone-prefixed-kept',
    css: `.a{width:-webkit-fill-available}`,
    options: {minify: true},
    check: out => out.includes('-webkit-fill-available'),
  },
  {
    // Control: the common iOS viewport hack is preserved with real targets.
    name: 'control-defaults-100vh-fallback-kept',
    run: (l, {require}) => l.transform({filename: 'a.css', code: Buffer.from(`.a{height:100vh;height:-webkit-fill-available}`), minify: true, targets: defaults(l, require)}).code.toString(),
    check: out => out.includes('100vh') && out.includes('-webkit-fill-available'),
  },
];
