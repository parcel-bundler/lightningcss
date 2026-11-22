// #556: AST -> CSS and CSS -> AST from JS. No dedicated toCss()/parse() exports exist, but:
//  - CSS -> AST: StyleSheet visitor captures the parsed AST;
//  - AST -> CSS: returning an AST (whole sheet, or a node wrapped in a rule) from a
//    StyleSheet visitor makes transform() print it;
//  - CSS string -> node inside a visitor: return {property, raw} / {raw}.
// check() = these round trips work (controls guarding the documented workarounds).
function parse(lightningcss, code) {
  let ast;
  lightningcss.transform({filename: 'in.css', code: Buffer.from(code), visitor: {StyleSheet(s) { ast = s; }}});
  return ast;
}
function print(lightningcss, ast, minify = false) {
  return lightningcss.transform({
    filename: 'out.css', code: Buffer.from(''), minify,
    visitor: {StyleSheet() { return ast; }},
  }).code.toString();
}

export const cases = [
  {
    name: 'roundtrip-stylesheet',
    run(lightningcss) {
      let src = '.a{color:red;margin:1px 2px}@media (min-width:10px){.b{width:1px}}';
      let ast = parse(lightningcss, src);
      return print(lightningcss, ast, true);
    },
    check: (out) => out === '.a{color:red;margin:1px 2px}@media (width>=10px){.b{width:1px}}',
  },
  {
    name: 'serialize-single-declaration',
    run(lightningcss) {
      // capture a Declaration object in a visitor, then print it by wrapping it in a rule
      let decl;
      lightningcss.transform({filename: 'in.css', code: Buffer.from('.a { border: 1px solid red }'),
        visitor: {Declaration(d) { decl = d; }}});
      let out = lightningcss.transform({
        filename: 'x.css', code: Buffer.from('.x{}'),
        visitor: {Rule: {style(r) { r.value.declarations = {declarations: [decl], importantDeclarations: []}; return r; }}},
      }).code.toString();
      return out;
    },
    check: (out) => /border: 1px solid red/.test(out),
  },
  {
    name: 'raw-declaration-return',
    css: '.a { color: green }',
    options: {visitor: {Declaration: {color() { return {property: 'color', raw: 'rgb(255 0 0)'}; }}}},
    check: (out) => /color: red/.test(out),
  },
];
