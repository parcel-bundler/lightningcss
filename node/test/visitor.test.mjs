// @ts-check

import { test } from 'uvu';
import * as assert from 'uvu/assert';
import fs from 'fs';
import {webcrypto as crypto} from 'node:crypto';

let bundle, bundleAsync, transform, transformStyleAttribute;
if (process.env.TEST_WASM === 'node') {
  ({ bundle, bundleAsync, transform, transformStyleAttribute } = await import('../../wasm/wasm-node.mjs'));
} else if (process.env.TEST_WASM === 'browser') {
  // Define crypto globally for old node.
  // @ts-ignore
  globalThis.crypto ??= crypto;
  let wasm = await import('../../wasm/index.mjs');
  await wasm.default();
  ({ transform, transformStyleAttribute } = wasm);
  bundle = function(options) {
    return wasm.bundle({
      ...options,
      resolver: {
        read: (filePath) => fs.readFileSync(filePath, 'utf8')
      }
    });
  }

  bundleAsync = function (options) {
    if (!options.resolver?.read) {
      options.resolver = {
        ...options.resolver,
        read: (filePath) => fs.readFileSync(filePath, 'utf8')
      };
    }

    return wasm.bundleAsync(options);
  }
} else {
  ({ bundle, bundleAsync, transform, transformStyleAttribute } = await import('../index.mjs'));
}

test('px to rem', () => {
  // Similar to https://github.com/cuth/postcss-pxtorem
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: 32px;
        height: calc(100vh - 64px);
        --custom: calc(var(--foo) + 32px);
      }
    `),
    visitor: {
      Length(length) {
        if (length.unit === 'px') {
          return {
            unit: 'rem',
            value: length.value / 16
          };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{--custom:calc(var(--foo) + 2rem);width:2rem;height:calc(100vh - 4rem)}');
});

test('custom units', () => {
  // https://github.com/csstools/custom-units
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        --step: .25rem;
        font-size: 3--step;
      }
    `),
    visitor: {
      Token: {
        dimension(token) {
          if (token.unit.startsWith('--')) {
            return {
              type: 'function',
              value: {
                name: 'calc',
                arguments: [
                  {
                    type: 'token',
                    value: {
                      type: 'number',
                      value: token.value
                    }
                  },
                  {
                    type: 'token',
                    value: {
                      type: 'delim',
                      value: '*'
                    }
                  },
                  {
                    type: 'var',
                    value: {
                      name: {
                        ident: token.unit
                      }
                    }
                  }
                ]
              }
            }
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{--step:.25rem;font-size:calc(3*var(--step))}');
});

test('design tokens', () => {
  // Similar to https://www.npmjs.com/package/@csstools/postcss-design-tokens
  let tokens = {
    'color.background.primary': {
      type: 'color',
      value: {
        type: 'rgb',
        r: 255,
        g: 0,
        b: 0,
        alpha: 1
      }
    },
    'size.spacing.small': {
      type: 'length',
      value: {
        unit: 'px',
        value: 16
      }
    }
  };

  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        color: design-token('color.background.primary');
        padding: design-token('size.spacing.small');
      }
    `),
    visitor: {
      Function: {
        'design-token'(fn) {
          if (fn.arguments.length === 1 && fn.arguments[0].type === 'token' && fn.arguments[0].value.type === 'string') {
            return tokens[fn.arguments[0].value.value];
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:red;padding:16px}');
});

test('env function', () => {
  // https://www.npmjs.com/package/postcss-env-function
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
    visitor: {
      EnvironmentVariable(env) {
        if (env.name.type === 'custom') {
          return tokens[env.name.ident];
        }
      }
    }
  });

  assert.equal(res.code.toString(), '@media (width<=600px){body{padding:20px}}');
});

test('specific environment variables', () => {
  // https://www.npmjs.com/package/postcss-env-function
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
    visitor: {
      EnvironmentVariable: {
        '--branding-small': () => tokens['--branding-small'],
        '--branding-padding': () => tokens['--branding-padding']
      }
    }
  });

  assert.equal(res.code.toString(), '@media (width<=600px){body{padding:20px}}');
});

test('spacing with env substitution', () => {
  // Test spacing for different cases when `env()` functions are replaced with actual values.
  /** @type {Record<string, string>} */
  let tokens = {
    '--var1': 'var(--foo)',
    '--var2': 'var(--bar)',
    '--function': 'scale(1.5)',
    '--length1': '10px',
    '--length2': '20px',
    '--x': '4',
    '--y': '12',
    '--num1': '5',
    '--num2': '10',
    '--num3': '15',
    '--counter': '2',
    '--ident1': 'solid',
    '--ident2': 'auto',
    '--rotate': '45deg',
    '--percentage1': '25%',
    '--percentage2': '75%',
    '--color': 'red',
    '--color1': '#ff1234',
    '--string1': '"hello"',
    '--string2': '" world"'
  };

  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .test {
        /* Asymmetric spacing - no space after var(). */
        background: env(--var1) env(--var2);
        border: env(--var1)env(--ident1);
        transform: env(--function) env(--function);
        /* Normal spacing between values. */
        padding: env(--length1) env(--length2);
        margin: env(--length1) env(--ident2);
        outline: env(--color) env(--ident1);
        /* Raw numbers that need spacing. */
        cursor: url(cursor.png) env(--x) env(--y), auto;
        stroke-dasharray: env(--num1) env(--num2) env(--num3);
        counter-increment: myCounter env(--counter);
        /* Mixed token types. */
        background: linear-gradient(red env(--percentage1), blue env(--percentage2));
        content: env(--string1) env(--string2);
        /* Inside calc expressions. */
        width: calc(env(--length1) - env(--length2));
      }
    `),
    visitor: {
      EnvironmentVariable(env) {
        if (env.name.type === 'custom' && tokens[env.name.ident]) {
          return { raw: tokens[env.name.ident] };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.test{background:var(--foo) var(--bar);border:var(--foo)solid;transform:scale(1.5) scale(1.5);padding:10px 20px;margin:10px auto;outline:red solid;cursor:url(cursor.png) 4 12, auto;stroke-dasharray:5 10 15;counter-increment:myCounter 2;background:linear-gradient(red 25%, blue 75%);content:"hello" " world";width:calc(10px - 20px)}');
});

test('url', () => {
  // https://www.npmjs.com/package/postcss-url
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        background: url(foo.png);
      }
    `),
    visitor: {
      Url(url) {
        url.url = 'https://mywebsite.com/' + url.url;
        return url;
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{background:url(https://mywebsite.com/foo.png)}');
});

test('static vars', () => {
  // Similar to https://www.npmjs.com/package/postcss-simple-vars
  let declared = new Map();
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @blue #056ef0;

      .menu_link {
        background: @blue;
      }
    `),
    visitor: {
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
  });

  assert.equal(res.code.toString(), '.menu_link{background:#056ef0}');
});

test('selector prefix', () => {
  // Similar to https://www.npmjs.com/package/postcss-prefix-selector
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .a, .b {
        color: red;
      }
    `),
    visitor: {
      Selector(selector) {
        return [{ type: 'class', name: 'prefix' }, { type: 'combinator', value: 'descendant' }, ...selector];
      }
    }
  });

  assert.equal(res.code.toString(), '.prefix .a,.prefix .b{color:red}');
});

test('apply', () => {
  // Similar to https://www.npmjs.com/package/postcss-apply
  let defined = new Map();
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      --toolbar-theme {
        color: white;
        border: 1px solid green;
      }

      .toolbar {
        @apply --toolbar-theme;
      }
    `),
    visitor: {
      Rule: {
        style(rule) {
          for (let selector of rule.value.selectors) {
            if (selector.length === 1 && selector[0].type === 'type' && selector[0].name.startsWith('--')) {
              defined.set(selector[0].name, rule.value.declarations);
              return { type: 'ignored', value: null };
            }
          }

          rule.value.rules = rule.value.rules.filter(child => {
            if (child.type === 'unknown' && child.value.name === 'apply') {
              for (let token of child.value.prelude) {
                if (token.type === 'dashed-ident' && defined.has(token.value)) {
                  let r = defined.get(token.value);
                  let decls = rule.value.declarations;
                  decls.declarations.push(...r.declarations);
                  decls.importantDeclarations.push(...r.importantDeclarations);
                }
              }
              return false;
            }
            return true;
          });

          return rule;
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.toolbar{color:#fff;border:1px solid green}');
});

test('property lookup', () => {
  // Similar to https://www.npmjs.com/package/postcss-property-lookup
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
     .test {
        margin-left: 20px;
        margin-right: @margin-left;
     }
    `),
    visitor: {
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
    }
  });

  assert.equal(res.code.toString(), '.test{margin-left:20px;margin-right:20px}');
});

test('focus visible', () => {
  // Similar to https://www.npmjs.com/package/postcss-focus-visible
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .test:focus-visible {
        color: red;
      }
    `),
    targets: {
      safari: 14 << 16
    },
    visitor: {
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
  });

  assert.equal(res.code.toString(), '.test.focus-visible{color:red}.test:focus-visible{color:red}');
});

test('dark theme class', () => {
  // Similar to https://github.com/postcss/postcss-dark-theme-class
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @media (prefers-color-scheme: dark) {
        body {
          background: black
        }
      }
    `),
    visitor: {
      Rule: {
        media(rule) {
          let q = rule.value.query.mediaQueries[0];
          if (q.condition?.type === 'feature' && q.condition.value.type === 'plain' && q.condition.value.name === 'prefers-color-scheme' && q.condition.value.value.value === 'dark') {
            /** @type {import('../ast').Rule[]} */
            let clonedRules = [rule];
            for (let r of rule.value.rules) {
              if (r.type === 'style') {
                /** @type {import('../ast').Selector[]} */
                let clonedSelectors = [];
                for (let selector of r.value.selectors) {
                  clonedSelectors.push([
                    { type: 'type', name: 'html' },
                    { type: 'attribute', name: 'theme', operation: { operator: 'equal', value: 'dark' } },
                    { type: 'combinator', value: 'descendant' },
                    ...selector
                  ]);
                  selector.unshift(
                    { type: 'type', name: 'html' },
                    {
                      type: 'pseudo-class',
                      kind: 'not',
                      selectors: [
                        [{ type: 'attribute', name: 'theme', operation: { operator: 'equal', value: 'light' } }]
                      ]
                    },
                    { type: 'combinator', value: 'descendant' }
                  );
                }

                clonedRules.push({ type: 'style', value: { ...r.value, selectors: clonedSelectors } });
              }
            }

            return clonedRules;
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '@media (prefers-color-scheme:dark){html:not([theme=light]) body{background:#000}}html[theme=dark] body{background:#000}');
});

test('100vh fix', () => {
  // Similar to https://github.com/postcss/postcss-100vh-fix
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        color: red;
        height: 100vh;
      }
    `),
    visitor: {
      Rule: {
        style(style) {
          let cloned;
          for (let property of style.value.declarations.declarations) {
            if (property.property === 'height' && property.value.type === 'length-percentage' && property.value.value.type === 'dimension' && property.value.value.value.unit === 'vh' && property.value.value.value.value === 100) {
              if (!cloned) {
                cloned = structuredClone(style);
                cloned.value.declarations.declarations = [];
              }
              cloned.value.declarations.declarations.push({
                ...property,
                value: {
                  type: 'stretch',
                  vendorPrefix: ['webkit']
                }
              });
            }
          }

          if (cloned) {
            return [style, {
              type: 'supports',
              value: {
                condition: {
                  type: 'declaration',
                  propertyId: {
                    property: '-webkit-touch-callout'
                  },
                  value: 'none'
                },
                loc: style.value.loc,
                rules: [cloned]
              }
            }];
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:red;height:100vh}@supports (-webkit-touch-callout:none){.foo{height:-webkit-fill-available}}')
});

test('logical transforms', () => {
  // Similar to https://github.com/MohammadYounes/rtlcss
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        transform: translateX(50px);
      }

      .bar {
        transform: translateX(20%);
      }

      .baz {
        transform: translateX(calc(100vw - 20px));
      }
    `),
    visitor: {
      Rule: {
        style(style) {
          /** @type any */
          let cloned;
          for (let property of style.value.declarations.declarations) {
            if (property.property === 'transform') {
              let clonedTransforms = property.value.map(transform => {
                if (transform.type !== 'translateX') {
                  return transform;
                }

                if (!cloned) {
                  cloned = structuredClone(style);
                  cloned.value.declarations.declarations = [];
                }

                let value;
                switch (transform.value.type) {
                  case 'dimension':
                    value = { type: 'dimension', value: { unit: transform.value.value.unit, value: -transform.value.value.value } };
                    break;
                  case 'percentage':
                    value = { type: 'percentage', value: -transform.value.value };
                    break;
                  case 'calc':
                    value = { type: 'calc', value: { type: 'product', value: [-1, transform.value.value] } };
                    break;
                }

                return {
                  type: 'translateX',
                  value
                }
              });

              if (cloned) {
                cloned.value.selectors.at(-1).push({ type: 'pseudo-class', kind: 'dir', direction: 'rtl' });
                cloned.value.declarations.declarations.push({
                  ...property,
                  value: clonedTransforms
                });
              }
            }
          }

          if (cloned) {
            return [style, cloned];
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{transform:translate(50px)}.foo:dir(rtl){transform:translate(-50px)}.bar{transform:translate(20%)}.bar:dir(rtl){transform:translate(-20%)}.baz{transform:translate(calc(100vw - 20px))}.baz:dir(rtl){transform:translate(-1*calc(100vw - 20px))}');
});

test('hover media query', () => {
  // Similar to https://github.com/twbs/mq4-hover-shim
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @media (hover) {
        .foo {
          color: red;
        }
      }
    `),
    visitor: {
      Rule: {
        media(media) {
          let mediaQueries = media.value.query.mediaQueries;
          if (
            mediaQueries.length === 1 &&
            mediaQueries[0].condition &&
            mediaQueries[0].condition.type === 'feature' &&
            mediaQueries[0].condition.value.type === 'boolean' &&
            mediaQueries[0].condition.value.name === 'hover'
          ) {
            for (let rule of media.value.rules) {
              if (rule.type === 'style') {
                for (let selector of rule.value.selectors) {
                  selector.unshift({ type: 'class', name: 'hoverable' }, { type: 'combinator', value: 'descendant' });
                }
              }
            }
            return media.value.rules
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.hoverable .foo{color:red}');
});

test('momentum scrolling', () => {
  // Similar to https://github.com/yunusga/postcss-momentum-scrolling
  let visitOverflow = decl => [decl, {
    property: '-webkit-overflow-scrolling',
    raw: 'touch'
  }];

  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        overflow: auto;
      }
    `),
    visitor: {
      Declaration: {
        overflow: visitOverflow,
        'overflow-x': visitOverflow,
        'overflow-y': visitOverflow
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{-webkit-overflow-scrolling:touch;overflow:auto}');
});

test('size', () => {
  // Similar to https://github.com/postcss/postcss-size
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        size: 12px;
      }
    `),
    visitor: {
      Declaration: {
        custom: {
          size(property) {
            if (property.value[0].type === 'length') {
              /** @type {import('../ast').Size} */
              let value = { type: 'length-percentage', value: { type: 'dimension', value: property.value[0].value } };
              return [
                { property: 'width', value },
                { property: 'height', value }
              ];
            }
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{width:12px;height:12px}');
});

test('works with style attributes', () => {
  let res = transformStyleAttribute({
    filename: 'test.css',
    minify: true,
    code: Buffer.from('height: calc(100vh - 64px)'),
    visitor: {
      Length(length) {
        if (length.unit === 'px') {
          return {
            unit: 'rem',
            value: length.value / 16
          };
        }
      }
    }
  });

  assert.equal(res.code.toString(), 'height:calc(100vh - 4rem)');
});

test('works with bundler', () => {
  let res = bundle({
    filename: 'tests/testdata/a.css',
    minify: true,
    visitor: {
      Length(length) {
        if (length.unit === 'px') {
          return {
            unit: 'rem',
            value: length.value / 16
          };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.b{height:calc(100vh - 4rem)}.a{width:2rem}');
});

test('works with async bundler', async () => {
  let res = await bundleAsync({
    filename: 'tests/testdata/a.css',
    minify: true,
    visitor: {
      Length(length) {
        if (length.unit === 'px') {
          return {
            unit: 'rem',
            value: length.value / 16
          };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.b{height:calc(100vh - 4rem)}.a{width:2rem}');
});

test('dashed idents', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        --foo: #ff0;
        color: var(--foo);
      }
    `),
    visitor: {
      DashedIdent(ident) {
        return `--prefix-${ident.slice(2)}`;
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{--prefix-foo:#ff0;color:var(--prefix-foo)}');
});

test('custom idents', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @keyframes test {
        from { color: red }
        to { color: green }
      }
      .foo {
        animation: test;
      }
    `),
    visitor: {
      CustomIdent(ident) {
        return `prefix-${ident}`;
      }
    }
  });

  assert.equal(res.code.toString(), '@keyframes prefix-test{0%{color:red}to{color:green}}.foo{animation:prefix-test}');
});

test('returning string values', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @tailwind base;
    `),
    visitor: {
      Rule: {
        unknown(rule) {
          return {
            type: 'style',
            value: {
              loc: rule.loc,
              selectors: [
                [{ type: 'universal' }]
              ],
              declarations: {
                declarations: [
                  {
                    property: 'visibility',
                    raw: 'hi\\64 den' // escapes work for raw but not value
                  },
                  {
                    property: 'background',
                    raw: 'yellow'
                  },
                  {
                    property: '--custom',
                    raw: 'hi'
                  },
                  {
                    property: 'transition',
                    vendorPrefix: ['moz'],
                    raw: '200ms test'
                  },
                  {
                    property: '-webkit-animation',
                    raw: '3s cubic-bezier(0.25, 0.1, 0.25, 1) foo'
                  }
                ]
              }
            }
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '*{visibility:hidden;--custom:hi;background:#ff0;-moz-transition:test .2s;-webkit-animation:3s foo}');
});

test('errors on invalid dashed idents', () => {
  assert.throws(() => {
    transform({
      filename: 'test.css',
      minify: true,
      code: Buffer.from(`
        .foo {
          background: opacity(abcdef);
        }
      `),
      visitor: {
        Function(fn) {
          if (fn.arguments[0].type === 'token' && fn.arguments[0].value.type === 'ident') {
            fn.arguments = [
              {
                type: 'var',
                value: {
                  name: { ident: fn.arguments[0].value.value }
                }
              }
            ];
          }

          return {
            type: 'function',
            value: fn
          }
        }
      }
    })
  }, 'Dashed idents must start with --');
});

test('supports returning raw values for tokens', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        color: theme('red');
      }
    `),
    visitor: {
      Function: {
        theme() {
          return { raw: 'rgba(255, 0, 0)' };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:red}');
});

test('supports returning raw values as variables', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    cssModules: {
      dashedIdents: true
    },
    code: Buffer.from(`
      .foo {
        color: theme('foo');
      }
    `),
    visitor: {
      Function: {
        theme() {
          return { raw: 'var(--foo)' };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.EgL3uq_foo{color:var(--EgL3uq_foo)}');
});

test('works with currentColor', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        color: currentColor;
      }
    `),
    visitor: {
      Rule(rule) {
        return rule;
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:currentColor}');
});

test('nth of S to nth-of-type', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      a:nth-child(even of a) {
        color: red;
      }
    `),
    visitor: {
      Selector(selector) {
        for (let component of selector) {
          if (component.type === 'pseudo-class' && component.kind === 'nth-child' && component.of) {
            delete component.of;
            component.kind = 'nth-of-type';
          }
        }
        return selector;
      }
    }
  });

  assert.equal(res.code.toString(), 'a:nth-of-type(2n){color:red}');
});

test('media query raw', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @breakpoints {
        .m-1 {
          margin: 10px;
        }
      }
    `),
    customAtRules: {
      breakpoints: {
        prelude: null,
        body: "rule-list",
      },
    },
    visitor: {
      Rule: {
        custom: {
          breakpoints({ body, loc }) {
            /** @type {import('lightningcss').ReturnedRule[]} */
            const value = [];

            for (let rule of body.value) {
              if (rule.type !== 'style') {
                continue;
              }
              const clone = structuredClone(rule);
              for (let selector of clone.value.selectors) {
                for (let component of selector) {
                  if (component.type === 'class') {
                    component.name = `sm:${component.name}`;
                  }
                }
              }

              value.push(rule);
              value.push({
                type: "media",
                value: {
                  rules: [clone],
                  loc,
                  query: {
                    mediaQueries: [
                      { raw: '(min-width: 500px)' }
                    ]
                  }
                }
              });
            }

            return value;
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.m-1{margin:10px}@media (width>=500px){.sm\\:m-1{margin:10px}}');
});

test('visit stylesheet', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: 32px;
      }

      .bar {
        width: 80px;
      }
    `),
    visitor: {
      StyleSheetExit(stylesheet) {
        stylesheet.rules.sort((a, b) => a.value.selectors[0][0].name.localeCompare(b.value.selectors[0][0].name));
        return stylesheet;
      }
    }
  });

  assert.equal(res.code.toString(), '.bar{width:80px}.foo{width:32px}');
});

test.run();
