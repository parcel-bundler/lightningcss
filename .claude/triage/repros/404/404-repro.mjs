// #404: ICSS `:export` in CSS modules.
// Feature case is false until `:export` is implemented (triage recommends accepting it):
// the block should be removed from the CSS and its declarations appear in `exports`.
// Controls pin current behavior that is correct regardless of the decision.
const tr = (css, opts) => (l) => {
  try {
    let r = l.transform({filename: 'a.module.css', code: Buffer.from(css), ...opts});
    return r.code.toString() + '\nEXPORTS ' + JSON.stringify(r.exports) +
      (r.warnings.length ? '\nWARN ' + r.warnings.map(w => w.message).join('; ') : '');
  } catch (e) { return 'ERROR: ' + e.message; }
};
const css = `:export { rowCount: 4; primaryColor: #ff0000; }\n.list { color: red }`;
export const cases = [
  {
    name: 'feature: :export values go to exports, not to CSS',
    run: tr(css, {cssModules: true}),
    check: (out) => {
      let [code, rest] = out.split('\nEXPORTS ');
      if (code.startsWith('ERROR')) return false;
      let exp = JSON.parse(rest.split('\nWARN')[0]);
      return !code.includes(':export') && 'rowCount' in exp && 'list' in exp;
    },
  },
  {
    name: 'control: without cssModules, :export passes through minification unchanged',
    run: tr(css, {minify: true}),
    check: (out) => out.startsWith(':export{rowCount:4;primaryColor:') && out.includes('.list{color:red}'),
  },
  {
    name: 'control: class next to :export is still hashed and exported',
    run: tr(css, {cssModules: true}),
    check: (out) => /EXPORTS .*"list":\{"name":"[^"]+_list"/.test(out),
  },
];
