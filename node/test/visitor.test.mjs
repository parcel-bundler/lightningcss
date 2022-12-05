import { test } from 'uvu';
import * as assert from 'uvu/assert';
import { bundle, bundleAsync, transform, transformStyleAttribute } from '../index.mjs';
import path from 'path';

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
      visitLength(length) {
        if (length.unit === 'px') {
          return {
            unit: 'rem',
            value: length.value / 16
          };
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{width:2rem;height:calc(100vh - 4rem);--custom:calc(var(--foo) + 2rem)}');
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
      visitToken(token) {
        if (token.type === 'token' && token.value.type === 'dimension' && token.value.unit.startsWith('--')) {
          return {
            type: 'function',
            value: {
              name: 'calc',
              arguments: [
                {
                  type: 'token',
                  value: {
                    type: 'number',
                    value: token.value.value
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
                      ident: token.value.unit
                    }
                  }
                }
              ]
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
        type: 'rgba',
        value: [255, 0, 0, 255]
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
      visitToken(token) {
        if (token.type === 'function' && token.value.name === 'design-token' && token.value.arguments.length === 1 && token.value.arguments[0].value.type === 'string') {
          return tokens[token.value.arguments[0].value.value];
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:red;padding:16px}');
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
      visitUrl(url) {
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
      visitRule(rule) {
        if (rule.type === 'unknown') {
          declared.set(rule.value.name, rule.value.prelude);
          return [];
        }
      },
      visitToken(token) {
        if (token.type === 'token' && token.value.type === 'at-keyword' && declared.has(token.value.value)) {
          // TODO: a way to return multiple tokens?
          return declared.get(token.value.value)[0];
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
      visitSelector(selector) {
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
    drafts: {
      nesting: true
    },
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
      visitRule(rule) {
        if (rule.type !== 'style') {
          return;
        }

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
                decls.important_declarations.push(...r.important_declarations);
              }
            }
            return false;
          }
          return true;
        });

        return rule;
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
      visitRule(rule) {
        if (rule.type !== 'style') {
          return;
        }

        let valuesByProperty = new Map();
        for (let decl of rule.value.declarations.declarations) {
          let name = decl.property;
          if (decl.property === 'unparsed') {
            name = decl.value.property_id.property;
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
                property: decl.value.property_id.property,
                value: v.value
              };
            }
          }
          return decl;
        });

        return rule;
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
    visitor: {
      visitRule(rule) {
        if (rule.type !== 'style') {
          return;
        }

        let clone = null;
        for (let selector of rule.value.selectors) {
          for (let [i, component] of selector.entries()) {
            if (component.type === 'pseudo-class' && component.value === 'focus-visible') {
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
      visitRule(rule) {
        if (rule.type !== 'media') {
          return;
        }

        let q = rule.value.query.media_queries[0];
        if (q.condition?.type === 'feature' && q.condition.value.type === 'plain' && q.condition.value.name === 'prefers-color-scheme' && q.condition.value.value.value === 'dark') {
          let clonedRules = [rule];
          for (let r of rule.value.rules) {
            if (r.type === 'style') {
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
                      value: 'not',
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
  });

  assert.equal(res.code.toString(), '@media (prefers-color-scheme:dark){html:not([theme=light]) body{background:#000}}html[theme=dark] body{background:#000}');
});

test('works with style attributes', () => {
  let res = transformStyleAttribute({
    filename: 'test.css',
    minify: true,
    code: Buffer.from('height: calc(100vh - 64px)'),
    visitor: {
      visitLength(length) {
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
      visitLength(length) {
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
      visitLength(length) {
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

test.run();
