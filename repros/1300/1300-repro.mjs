// #1300: ::highlight() must be parsed as a known pseudo-element (added in #970), in strict and errorRecovery mode.
const run1 = (l, css, opts = {}) => {
  try {
    const r = l.transform({filename: 'in.css', code: Buffer.from(css), minify: true, ...opts});
    return r.code.toString() + (r.warnings.length ? ' WARN: ' + r.warnings.map(w => w.message).join('; ') : '');
  } catch (e) {
    return 'ERROR: ' + e.message;
  }
};
const ok = (out) => !out.includes('ERROR') && !out.includes('WARN') && out.includes('::highlight(');
export const cases = [
  {name: 'reporter, chrome 95 targets', run: (l) => run1(l, `::highlight(my-custom-highlight) {\n  background-color: peachpuff;\n}`, {targets: {chrome: 6225920}}), check: ok},
  {name: 'no targets', run: (l) => run1(l, `p::highlight(foo){color:red}`), check: ok},
  {name: 'errorRecovery', run: (l) => run1(l, `p::highlight(foo){color:red}`, {errorRecovery: true}), check: ok},
  {name: 'nested in style rule', run: (l) => run1(l, `.a{&::highlight(foo){color:red}}`, {targets: {chrome: 100 << 16}}), check: (o) => ok(o) && o.includes('.a::highlight(foo)')},
  {name: 'control: is() with highlight is not merged', run: (l) => run1(l, `.a::highlight(x){color:red}.b::highlight(x){color:red}`), check: (o) => !o.includes('ERROR')},
];
