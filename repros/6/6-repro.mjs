// #6: CSS modules feature checklist. Controls for the implemented items, plus the
// current (documented) handling of the non-function :local/:global form, which the
// maintainer declined in 2021: strict mode errors rather than silently mis-scoping.
const tr = (css, opts = {}) => (l) => {
  try {
    let r = l.transform({filename: 'a.module.css', code: Buffer.from(css), cssModules: true, ...opts});
    return r.code.toString() + '\nEXPORTS ' + JSON.stringify(r.exports) +
      (r.warnings.length ? '\nWARN ' + r.warnings.map(w => w.message).join('; ') : '');
  } catch (e) { return 'ERROR: ' + e.message; }
};
export const cases = [
  {name: 'control: classes local by default', run: tr('.foo { color: red }'),
   check: (o) => /\.[\w-]+_foo \{/.test(o)},
  {name: 'control: :global() / :local() function syntax', run: tr(':global(.g) :local(.l) { color: red }'),
   check: (o) => /^\.g \.[\w-]+_l \{/.test(o)},
  {name: 'control: local animation name', run: tr('@keyframes spin { to { opacity: 0 } } .a { animation: spin 1s }'),
   check: (o) => /@keyframes [\w-]+_spin/.test(o) && /animation: 1s [\w-]+_spin/.test(o)},
  {name: 'control: composes', run: tr('.a { color: red } .b { composes: a; color: blue }'),
   check: (o) => /"b":\{[^}]*"composes":\[\{"type":"local"/.test(o)},
  {name: 'control: non-function :global errors in strict mode', run: tr('.foo :global .bar { color: red }'),
   check: (o) => o.startsWith('ERROR') && /Ambiguous CSS module class/.test(o)},
  {name: 'control: non-function :global is a warning with errorRecovery', run: tr(':global { .bar { color: red } }', {errorRecovery: true}),
   check: (o) => /WARN .*Ambiguous CSS module class/.test(o)},
];
