// @ts-check

import { test } from 'uvu';
import * as assert from 'uvu/assert';
import {webcrypto as crypto} from 'node:crypto';

let transform, composeVisitors;
if (process.env.TEST_WASM === 'node') {
  ({transform, composeVisitors} = await import('../../wasm/wasm-node.mjs'));
} else if (process.env.TEST_WASM === 'browser') {
  // Define crypto globally for old node.
  // @ts-ignore
  globalThis.crypto ??= crypto;
  let wasm = await import('../../wasm/index.mjs');
  await wasm.default();
  ({transform, composeVisitors} = wasm);
} else {
  ({transform, composeVisitors} = await import('../index.mjs'));
}

test('different types', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: 16px;
        color: red;
      }
    `),
    visitor: composeVisitors([
      {
        Length(l) {
          if (l.unit === 'px') {
            return {
              unit: 'rem',
              value: l.value / 16
            }
          }
        }
      },
      {
        Color(c) {
          if (c.type === 'rgb') {
            return {
              type: 'rgb',
              r: c.g,
              g: c.r,
              b: c.b,
              alpha: c.alpha
            };
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{color:#0f0;width:1rem}');
});

test('simple matching types', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: 16px;
      }
    `),
    visitor: composeVisitors([
      {
        Length(l) {
          return {
            unit: l.unit,
            value: l.value * 2
          };
        }
      },
      {
        Length(l) {
          if (l.unit === 'px') {
            return {
              unit: 'rem',
              value: l.value / 16
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:2rem}');
});

test('different properties', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        size: 16px;
        bg: #ff0;
      }
    `),
    visitor: composeVisitors([
      {
        Declaration: {
          custom: {
            size(v) {
              return [
                { property: 'unparsed', value: { propertyId: { property: 'width' }, value: v.value } },
                { property: 'unparsed', value: { propertyId: { property: 'height' }, value: v.value } }
              ];
            }
          }
        }
      },
      {
        Declaration: {
          custom: {
            bg(v) {
              if (v.value[0].type === 'color') {
                return { property: 'background-color', value: v.value[0].value };
              }
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:16px;height:16px;background-color:#ff0}');
});

test('composed properties', () => {
  /** @type {import('../index').Visitor[]} */
  let visitors = [
    {
      Declaration: {
        custom: {
          size(v) {
            if (v.value[0].type === 'length') {
              return [
                { property: 'width', value: { type: 'length-percentage', value: { type: 'dimension', value: v.value[0].value } } },
                { property: 'height', value: { type: 'length-percentage', value: { type: 'dimension', value: v.value[0].value } } },
              ];
            }
          }
        }
      }
    },
    {
      Declaration: {
        width() {
          return [];
        }
      }
    }
  ];

  // Check that it works in any order.
  for (let i = 0; i < 2; i++) {
    let res = transform({
      filename: 'test.css',
      minify: true,
      code: Buffer.from(`
        .foo {
          size: 16px;
        }
      `),
      visitor: composeVisitors(visitors)
    });

    assert.equal(res.code.toString(), '.foo{height:16px}');
    visitors.reverse();
  }
});

test('same properties', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        color: red;
      }
    `),
    visitor: composeVisitors([
      {
        Declaration: {
          color(v) {
            if (v.property === 'color' && v.value.type === 'rgb') {
              return {
                property: 'color',
                value: {
                  type: 'rgb',
                  r: v.value.g,
                  g: v.value.r,
                  b: v.value.b,
                  alpha: v.value.alpha
                }
              };
            }
          }
        }
      },
      {
        Declaration: {
          color(v) {
            if (v.property === 'color' && v.value.type === 'rgb' && v.value.g > 0) {
              v.value.alpha /= 2;
            }
            return v;
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{color:#00ff0080}');
});

test('properties plus values', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        size: test;
      }
    `),
    visitor: composeVisitors([
      {
        Declaration: {
          custom: {
            size() {
              return [
                { property: 'width', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } },
                { property: 'height', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } },
              ];
            }
          }
        }
      },
      {
        Length(l) {
          if (l.unit === 'px') {
            return {
              unit: 'rem',
              value: l.value / 16
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:2rem;height:2rem}');
});

test('unparsed properties', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: test;
      }
      .bar {
        width: 16px;
      }
    `),
    visitor: composeVisitors([
      {
        Declaration: {
          width(v) {
            if (v.property === 'unparsed') {
              return [
                { property: 'width', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } },
                { property: 'height', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } },
              ];
            }
          }
        }
      },
      {
        Length(l) {
          if (l.unit === 'px') {
            return {
              unit: 'rem',
              value: l.value / 16
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:2rem;height:2rem}.bar{width:1rem}');
});

test('returning unparsed properties', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: test;
      }
    `),
    visitor: composeVisitors([
      {
        Declaration: {
          width(v) {
            if (v.property === 'unparsed' && v.value.value[0].type === 'token' && v.value.value[0].value.type === 'ident') {
              return {
                property: 'unparsed',
                value: {
                  propertyId: { property: 'width' },
                  value: [{
                    type: 'var',
                    value: {
                      name: {
                        ident: '--' + v.value.value[0].value.value
                      }
                    }
                  }]
                }
              }
            }
          }
        }
      },
      {
        Declaration: {
          width(v) {
            if (v.property === 'unparsed') {
              return {
                property: 'unparsed',
                value: {
                  propertyId: { property: 'width' },
                  value: [{
                    type: 'function',
                    value: {
                      name: 'calc',
                      arguments: v.value.value
                    }
                  }]
                }
              }
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:calc(var(--test))}');
});

test('all property handlers', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: test;
        height: test;
      }
    `),
    visitor: composeVisitors([
      {
        Declaration(decl) {
          if (decl.property === 'unparsed' && decl.value.propertyId.property === 'width') {
            return { property: 'width', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } };
          }
        }
      },
      {
        Declaration(decl) {
          if (decl.property === 'unparsed' && decl.value.propertyId.property === 'height') {
            return { property: 'height', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } };
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:32px;height:32px}');
});

test('all property handlers (exit)', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: test;
        height: test;
      }
    `),
    visitor: composeVisitors([
      {
        DeclarationExit(decl) {
          if (decl.property === 'unparsed' && decl.value.propertyId.property === 'width') {
            return { property: 'width', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } };
          }
        }
      },
      {
        DeclarationExit(decl) {
          if (decl.property === 'unparsed' && decl.value.propertyId.property === 'height') {
            return { property: 'height', value: { type: 'length-percentage', value: { type: 'dimension', value: { unit: 'px', value: 32 } } } };
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:32px;height:32px}');
});

test('tokens and functions', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: f3(f2(f1(test)));
      }
    `),
    visitor: composeVisitors([
      {
        FunctionExit: {
          f1(f) {
            if (f.arguments.length === 1 && f.arguments[0].type === 'token' && f.arguments[0].value.type === 'ident') {
              return {
                type: 'length',
                value: {
                  unit: 'px',
                  value: 32
                }
              }
            }
          }
        }
      },
      {
        FunctionExit(f) {
          return f.arguments[0];
        }
      },
      {
        Length(l) {
          if (l.unit === 'px') {
            return {
              unit: 'rem',
              value: l.value / 16
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:2rem}');
});

test('unknown rules', () => {
  let declared = new Map();
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @test #056ef0;

      .menu_link {
        background: @blue;
      }
    `),
    visitor: composeVisitors([
      {
        Rule: {
          unknown: {
            test(rule) {
              rule.name = 'blue';
              return {
                type: 'unknown',
                value: rule
              };
            }
          }
        }
      },
      {
        Rule: {
          unknown(rule) {
            declared.set(rule.name, rule.prelude);
            return [];
          }
        },
        Token: {
          'at-keyword'(token) {
            if (declared.has(token.value)) {
              return declared.get(token.value);
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.menu_link{background:#056ef0}');
});

test('custom at rules', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @testA;
      @testB;
    `),
    customAtRules: {
      testA: {},
      testB: {}
    },
    visitor: composeVisitors([
      {
        Rule: {
          custom: {
            testA(rule) {
              return {
                type: 'style',
                value: {
                  loc: rule.loc,
                  selectors: [
                    [{ type: 'class', name: 'testA' }]
                  ],
                  declarations: {
                    declarations: [
                      {
                        property: 'color',
                        value: {
                          type: 'rgb',
                          r: 0xff,
                          g: 0x00,
                          b: 0x00,
                          alpha: 1,
                        }
                      }
                    ]
                  }
                }
              };
            }
          }
        }
      },
      {
        Rule: {
          custom: {
            testB(rule) {
              return {
                type: 'style',
                value: {
                  loc: rule.loc,
                  selectors: [
                    [{ type: 'class', name: 'testB' }]
                  ],
                  declarations: {
                    declarations: [
                      {
                        property: 'color',
                        value: {
                          type: 'rgb',
                          r: 0x00,
                          g: 0xff,
                          b: 0x00,
                          alpha: 1,
                        }
                      }
                    ]
                  }
                }
              };
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.testA{color:red}.testB{color:#0f0}');
});

test('known rules', () => {
  let declared = new Map();
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .test:focus-visible {
        margin-left: 20px;
        margin-right: @margin-left;
      }
    `),
    targets: {
      safari: 14 << 16
    },
    visitor: composeVisitors([
      {
        Rule: {
          style(rule) {
            let valuesByProperty = new Map();
            for (let decl of rule.value.declarations.declarations) {
              /** @type string */
              let name = decl.property;
              if (decl.property === 'unparsed') {
                name = decl.value.propertyId.property;
              }
              valuesByProperty.set(name, decl);
            }

            rule.value.declarations.declarations = rule.value.declarations.declarations.map(decl => {
              // Only single value supported. Would need a way to convert parsed values to unparsed tokens otherwise.
              if (decl.property === 'unparsed' && decl.value.value.length === 1) {
                let token = decl.value.value[0];
                if (token.type === 'token' && token.value.type === 'at-keyword' && valuesByProperty.has(token.value.value)) {
                  let v = valuesByProperty.get(token.value.value);
                  return {
                    /** @type any */
                    property: decl.value.propertyId.property,
                    value: v.value
                  };
                }
              }
              return decl;
            });

            return rule;
          }
        }
      },
      {
        Rule: {
          style(rule) {
            let clone = null;
            for (let selector of rule.value.selectors) {
              for (let [i, component] of selector.entries()) {
                if (component.type === 'pseudo-class' && component.kind === 'focus-visible') {
                  if (clone == null) {
                    clone = [...rule.value.selectors.map(s => [...s])];
                  }

                  selector[i] = { type: 'class', name: 'focus-visible' };
                }
              }
            }

            if (clone) {
              return [rule, { type: 'style', value: { ...rule.value, selectors: clone } }];
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.test.focus-visible{margin-left:20px;margin-right:20px}.test:focus-visible{margin-left:20px;margin-right:20px}');
});

test('environment variables', () => {
  /** @type {Record<string, import('../ast').TokenOrValue>} */
  let tokens = {
    '--branding-small': {
      type: 'length',
      value: {
        unit: 'px',
        value: 600
      }
    },
    '--branding-padding': {
      type: 'length',
      value: {
        unit: 'px',
        value: 20
      }
    }
  };

  let res = transform({
    filename: 'test.css',
    minify: true,
    errorRecovery: true,
    code: Buffer.from(`
      @media (max-width: env(--branding-small)) {
        body {
          padding: env(--branding-padding);
        }
      }
    `),
    visitor: composeVisitors([
      {
        EnvironmentVariable: {
          '--branding-small': () => tokens['--branding-small']
        }
      },
      {
        EnvironmentVariable: {
          '--branding-padding': () => tokens['--branding-padding']
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '@media (width<=600px){body{padding:20px}}');
});

test('variables', () => {
  /** @type {Record<string, import('../ast').TokenOrValue>} */
  let tokens = {
    '--branding-small': {
      type: 'length',
      value: {
        unit: 'px',
        value: 600
      }
    },
    '--branding-padding': {
      type: 'length',
      value: {
        unit: 'px',
        value: 20
      }
    }
  };

  let res = transform({
    filename: 'test.css',
    minify: true,
    errorRecovery: true,
    code: Buffer.from(`
      body {
        padding: var(--branding-padding);
        width: var(--branding-small);
      }
    `),
    visitor: composeVisitors([
      {
        Variable(v) {
          if (v.name.ident === '--branding-small') {
            return tokens['--branding-small'];
          }
        }
      },
      {
        Variable(v) {
          if (v.name.ident === '--branding-padding') {
            return tokens['--branding-padding'];
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), 'body{padding:20px;width:600px}');
});

test('StyleSheet', () => {
  let styleSheetCalledCount = 0;
  let styleSheetExitCalledCount = 0;
  transform({
    filename: 'test.css',
    code: Buffer.from(`
      body {
        color: blue;
      }
    `),
    visitor: composeVisitors([
      {
        StyleSheet() {
          styleSheetCalledCount++
        },
        StyleSheetExit() {
          styleSheetExitCalledCount++
        }
      },
      {
        StyleSheet() {
          styleSheetCalledCount++
        },
        StyleSheetExit() {
          styleSheetExitCalledCount++
        }
      }
    ])
  });
  assert.equal(styleSheetCalledCount, 2);
  assert.equal(styleSheetExitCalledCount, 2);
});

test.run();
