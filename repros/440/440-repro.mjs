// #440: using lightningcss as a parser from Node. There is no `parse()` export, but the
// StyleSheet visitor exposes the whole typed AST, which works as a parse() today.
// check() = the workaround works (control: guards the visitor API used in the reply).
const sortKeys = (v) => Array.isArray(v) ? v.map(sortKeys)
  : v && typeof v === 'object' ? Object.fromEntries(Object.keys(v).sort().map(k => [k, sortKeys(v[k])])) : v;

function parse(lightningcss, code) {
  let ast;
  lightningcss.transform({
    filename: 'in.css', code: Buffer.from(code),
    visitor: {StyleSheet(s) { ast = s; }},
  });
  return ast;
}

export const cases = [
  {
    name: 'parse-via-StyleSheet-visitor',
    run(lightningcss) {
      let ast = parse(lightningcss, '.a { color: red; margin: 1px 2px }\n@media (min-width: 10px) { .b { width: 1px } }');
      let rule = ast.rules[0];
      return JSON.stringify(sortKeys({
        n: ast.rules.length,
        types: ast.rules.map(r => r.type),
        props: rule.value.declarations.declarations.map(d => d.property),
        color: rule.value.declarations.declarations[0].value,
        loc: rule.value.loc,
      }));
    },
    check: (out) => {
      let o = JSON.parse(out);
      return o.n === 2 && o.types.join() === 'style,media' && o.props.join() === 'color,margin'
        && o.color.type === 'rgb' && o.color.r === 255 && o.loc.line === 0;
    },
  },
  {
    name: 'parse-reports-errors',
    run(lightningcss) {
      try { parse(lightningcss, '.a { color: red'); parse(lightningcss, '@media (min-width: ) {}'); return 'no error'; }
      catch (e) { return 'error: ' + e.message; }
    },
    // known syntax that is invalid errors in strict mode
    check: (out) => out.startsWith('error:'),
  },
];
