// #604: run every example from website/pages/transforms.md and compare with
// the output the docs assert. check() = the documented output is produced.
// The `Property` visitor key used by three examples does not exist (the API
// key is `Declaration`), so those fail until the docs are fixed; controls
// at the end use `Declaration` and pass today.
const t = (l, opts) => l.transform({filename: 'test.css', ...opts});
const out = (res) => res.code.toString() + (res.warnings?.length ? `\n/* warnings: ${res.warnings.map(w => w.message).join('; ')} */` : '');

const overflowVisitor = (property) => [{
  property: 'custom',
  value: {name: '-webkit-overflow-scrolling', value: [{type: 'token', value: {type: 'ident', value: 'touch'}}]},
}, property];

const sizeVisitor = (property) => {
  if (property.value[0].type === 'length') {
    let value = {type: 'length-percentage', value: {type: 'dimension', value: property.value[0].value}};
    return [{property: 'width', value}, {property: 'height', value}];
  }
};

const doubleVisitor = {FunctionExit: {double(f) {
  if (f.arguments[0].type === 'length') {
    return {type: 'length', value: {unit: f.arguments[0].value.unit, value: f.arguments[0].value.value * 2}};
  }
}}};
const envVisitor = {EnvironmentVariable: {'--branding-padding': () => ({type: 'length', value: {unit: 'px', value: 20}})}};

export const cases = [
  {
    name: 'length-double',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { width: 12px; }`),
      visitor: {Length(length) { return {unit: length.unit, value: length.value * 2}; }}})),
    check: (o) => o === '.foo{width:24px}',
  },
  {
    name: 'docs-Property-overflow',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { overflow: auto; }`),
      visitor: {Property: {overflow: overflowVisitor}}})),
    check: (o) => o === '.foo{-webkit-overflow-scrolling:touch;overflow:auto}',
  },
  {
    name: 'docs-Property-custom-size',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { size: 12px; }`),
      visitor: {Property: {custom: {size: sizeVisitor}}}})),
    check: (o) => o === '.foo{width:12px;height:12px}',
  },
  {
    // Third `Property` example (parsed vs unparsed width): returns nothing, so
    // count calls instead.
    name: 'docs-Property-width-called',
    run: (l) => {
      let calls = 0;
      t(l, {code: Buffer.from(`.foo { width: 12px } .bar { width: var(--w) }`),
        visitor: {Property: {width() { calls++; }}}});
      return `calls=${calls}`;
    },
    check: (o) => o === 'calls=2',
  },
  {
    name: 'raw-color-function',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { color: color('red'); }`),
      visitor: {Function: {color() { return {raw: 'rgb(255, 0, 0)'}; }}}})),
    check: (o) => o === '.foo{color:red}',
  },
  {
    name: 'function-exit-env',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { padding: double(env(--branding-padding)); }`),
      visitor: {...doubleVisitor, ...envVisitor}})),
    check: (o) => o === '.foo{padding:40px}',
  },
  {
    name: 'compose-visitors',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { padding: double(env(--branding-padding)); }`),
      visitor: l.composeVisitors([envVisitor, doubleVisitor])})),
    check: (o) => o === '.foo{padding:40px}',
  },
  {
    name: 'unknown-at-rule-static-vars',
    run: (l) => {
      let declared = new Map();
      return out(t(l, {minify: true, code: Buffer.from(`@blue #056ef0;\n.menu_link { background: @blue; }`),
        visitor: {
          Rule: {unknown(rule) { declared.set(rule.name, rule.prelude); return []; }},
          Token: {'at-keyword'(token) { return declared.get(token.value); }},
        }}));
    },
    // An "Unknown at rule" warning is expected; compare the code only.
    check: (o) => o.split('\n')[0] === '.menu_link{background:#056ef0}',
  },
  {
    name: 'custom-at-rules-mixin',
    run: (l) => {
      let mixins = new Map();
      return out(t(l, {minify: true, targets: {chrome: 100 << 16},
        code: Buffer.from(`@mixin color { color: red; &.bar { color: yellow; } }\n.foo { @apply color; }`),
        customAtRules: {mixin: {prelude: '<custom-ident>', body: 'style-block'}, apply: {prelude: '<custom-ident>'}},
        visitor: {Rule: {custom: {
          mixin(rule) { mixins.set(rule.prelude.value, rule.body.value); return []; },
          apply(rule) { return mixins.get(rule.prelude.value); },
        }}}}));
    },
    check: (o) => o === '.foo{color:red}.foo.bar{color:#ff0}',
  },
  {
    // addDependency exists since 1.32.0. The docs' assertion shows the glob
    // dependency as {type:'glob', filePath}, but the API returns {type, glob}.
    name: 'dependencies',
    run: (l) => {
      let res = t(l, {code: Buffer.from(`@dep "foo.js";\n@glob "**/*.json";\n.foo { width: 32px; }`),
        visitor: ({addDependency}) => ({Rule: {unknown: {
          dep(rule) { addDependency({type: 'file', filePath: rule.prelude[0].value.value}); return []; },
          glob(rule) { addDependency({type: 'glob', glob: rule.prelude[0].value.value}); return []; },
        }}})});
      return JSON.stringify((res.dependencies || []).map(d => Object.fromEntries(Object.entries(d).sort())));
    },
    check: (o) => o === JSON.stringify([{filePath: 'foo.js', type: 'file'}, {glob: '**/*.json', type: 'glob'}]),
  },
  // Controls: same examples with the real key, `Declaration`.
  {
    name: 'control-Declaration-overflow',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { overflow: auto; }`),
      visitor: {Declaration: {overflow: overflowVisitor}}})),
    check: (o) => o === '.foo{-webkit-overflow-scrolling:touch;overflow:auto}',
  },
  {
    name: 'control-Declaration-custom-size',
    run: (l) => out(t(l, {minify: true, code: Buffer.from(`.foo { size: 12px; }`),
      visitor: {Declaration: {custom: {size: sizeVisitor}}}})),
    check: (o) => o === '.foo{width:12px;height:12px}',
  },
];
