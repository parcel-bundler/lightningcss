// #1165: scoped declarations (css-cascade-6 #scoped-declarations): declarations directly inside @scope
// are wrapped in nested declarations rules that match the scoping root with zero specificity.
// Decision: implement (standard; @scope and CSSNestedDeclarations ship in all three engines).
const t = (l, css, opts = {}) => {
  try {
    const r = l.transform({filename: 'in.css', code: Buffer.from(css), minify: true, ...opts});
    return r.code.toString() + (r.warnings.length ? ' WARN: ' + r.warnings.map(w => w.message).join('; ') : '');
  } catch (e) {
    return 'ERROR: ' + e.message;
  }
};
export const cases = [
  {name: 'reporter: strict', run: (l) => t(l, `@scope (.foo) {\n  border: 1px solid black;\n}`), check: (o) => !o.includes('ERROR') && !o.includes('WARN') && /^@scope\(\.foo\)\{border:1px solid(?: #000)?\}$/.test(o.replace(' black', ' #000'))},
  // Today errorRecovery silently drops the declarations (and later sibling rules can be lost too).
  {name: 'errorRecovery keeps declarations', run: (l) => t(l, `@scope (.foo) { border: 1px solid black; .a{color:red} }`, {errorRecovery: true}), check: (o) => o.includes('border:') && o.includes('.a{color:red}')},
  {name: 'declarations after a rule', run: (l) => t(l, `@scope (.foo) { .a{color:red} color: blue }`), check: (o) => !o.includes('ERROR') && o.includes('.a{color:red}') && o.includes('color:#00f')},
  {name: 'implicit scope root', run: (l) => t(l, `@scope { color: red }`), check: (o) => !o.includes('ERROR') && o.includes('color:red')},
  // Controls that already work.
  {name: 'control: rules only', run: (l) => t(l, `@scope (.foo) { :scope{color:red} .a{color:blue} }`), check: (o) => o === '@scope(.foo){:scope{color:red}.a{color:#00f}}'},
  {name: 'control: @scope nested in style rule', run: (l) => t(l, `.x{@scope (.foo){color:red}}`), check: (o) => o === '.x{@scope(.foo){color:red}}'},
  // Related separate bug: when nesting is lowered (chrome 118 has @scope but not nesting), the parent `.x`
  // context is dropped, so the rule applies to every .foo. scope-start is relative to & per css-cascade-6.
  {name: 'related: nested @scope lowering keeps parent', run: (l) => t(l, `.x{@scope (.foo){.a{color:red}}}`, {targets: {chrome: 118 << 16}}), check: (o) => o.includes('.x')},
];
