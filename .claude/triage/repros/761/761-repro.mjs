// #761: `composes` inside @layer (and other conditional group rules) errors with
// "cannot be used within nested rules". The check is Printer::is_nested() (indent > 2), which
// counts any enclosing block, in both pretty and minify output. Wanted: composes in a plain
// class rule inside @layer/@media/@supports is accepted (same exports in both modes);
// composes inside a nested *style* rule still errors (control).
function t(l, css, minify) {
  try {
    const r = l.transform({filename: 'a.module.css', code: Buffer.from(css), cssModules: true, minify});
    const ex = Object.fromEntries(Object.entries(r.exports).sort(([a], [b]) => a.localeCompare(b)));
    return r.code.toString() + '\n' + JSON.stringify(ex);
  } catch (e) {
    return 'ERROR: ' + e.message;
  }
}
const ok = (out) => !out.startsWith("ERROR") && /"foo":\{"name":"[^"]*","composes":\[\{"type":"local","name":"[^"]*bar"/.test(out);
const mk = (name, css, minify, check = ok) => ({name, run: (l) => t(l, css, minify), check});
const layer = `@layer components { .foo { composes: bar; color: red } } .bar { color: blue }`;
const media = `@media screen { .foo { composes: bar; color: red } } .bar { color: blue }`;
const deep = `@layer a { @media screen { .foo { composes: bar; color: red } } } .bar { color: blue }`;
const from = `@layer components { .foo { composes: bar from './other.module.css'; color: red } }`;
const nested = `.x { .foo { composes: bar } } .bar { color: blue }`;
export const cases = [
  mk('layer-pretty', layer, false),
  mk('layer-minify', layer, true),
  mk('media-pretty', media, false),
  mk('media-minify', media, true),
  mk('layer-media-minify', deep, true),
  mk('from-file-layer-pretty', from, false, (o) => !o.startsWith('ERROR') && o.includes('other.module.css')),
  // controls
  mk('control-top-level', `.foo { composes: bar; color: red } .bar { color: blue }`, false),
  mk('control-nested-style-rule-errors-pretty', nested, false, (o) => o.includes('nested')),
  mk('control-nested-style-rule-errors-minify', nested, true, (o) => o.includes('nested')),
];
