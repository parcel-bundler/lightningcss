// #659: CSS modules @value. Unsupported by decision so far (#865, deprecation error added in #842, v1.28.0).
// Controls only: strict mode errors clearly, errorRecovery warns, and the supported
// alternative for sharing values (dashedIdents + `var(--x from "./file.css")`) works.
const tr = (css, opts) => (l) => {
  try {
    let r = l.transform({filename: 'spinner.module.css', code: Buffer.from(css), ...opts});
    return r.code.toString() + (r.warnings.length ? '\nWARN ' + r.warnings.map(w => w.message).join('; ') : '');
  } catch (e) { return 'ERROR: ' + e.message; }
};
const v = `@value button from "./button.module.css";\n.button .spinner { color: red }`;
export const cases = [
  {name: 'control: strict mode reports @value as unsupported', run: tr(v, {cssModules: true}),
   check: (o) => o.startsWith('ERROR') && /@value/.test(o)},
  {name: 'control: errorRecovery warns about @value', run: tr(v, {cssModules: true, errorRecovery: true}),
   check: (o) => /WARN .*@value/.test(o)},
  {name: 'control: @value without cssModules is an unknown at-rule passed through',
   run: tr(v, {errorRecovery: true}), check: (o) => o.includes('@value button from')},
  {name: 'control: dashed-ident import from another file is supported',
   run: (l) => {
     let r = l.transform({filename: 'a.module.css', code: Buffer.from('.a { color: var(--c from "./colors.css") }'),
       cssModules: {dashedIdents: true}});
     return r.code.toString() + '\nREFS ' + JSON.stringify(r.references);
   },
   check: (o) => !o.includes(" from ") && /REFS .*colors\.css/.test(o)},
];
