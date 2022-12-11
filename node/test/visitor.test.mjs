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
          if (fn.arguments.length === 1 && fn.arguments[0].value.type === 'string') {
            return tokens[fn.arguments[0].value.value];
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:red;padding:16px}');
});

test.skip('env function', () => {
  // https://www.npmjs.com/package/postcss-env-function
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
      Function: {
        env(fn) {
          if (fn.arguments.length === 1 && fn.arguments[0].type === 'dashed-ident') {
            return tokens[fn.arguments[0].value];
          }
        }
      }
    }
  });

  console.log(res.code.toString())
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
      Rule(rule) {
        if (rule.type === 'unknown') {
          declared.set(rule.value.name, rule.value.prelude);
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
      StyleRule(rule) {
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
      StyleRule(rule) {
        let valuesByProperty = new Map();
        for (let decl of rule.value.declarations.declarations) {
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
      StyleRule(rule) {
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
      MediaRule(rule) {
        let q = rule.value.query.mediaQueries[0];
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
      StyleRule(style) {
        let cloned;
        for (let property of style.value.declarations.declarations) {
          if (property.property === 'height' && property.value.type === 'length-percentage' && property.value.value.value.unit === 'vh' && property.value.value.value.value === 100) {
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
                value: '-webkit-touch-callout: none'
              },
              loc: style.value.loc,
              rules: [cloned]
            }
          }];
        }
      }
    }
  });

  assert.equal(res.code.toString(), '.foo{color:red;height:100vh}@supports (-webkit-touch-callout: none){.foo{height:-webkit-fill-available}}')
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
      StyleRule(style) {
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
      MediaRule(media) {
        let mediaQueries = media.value.query.mediaQueries;
        if (
          mediaQueries.length === 1 &&
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
  });

  assert.equal(res.code.toString(), '.hoverable .foo{color:red}');
});

test('momentum scrolling', () => {
  // Similar to https://github.com/yunusga/postcss-momentum-scrolling
  let visitOverflow = property => [property, {
    property: 'custom',
    value: {
      name: '-webkit-overflow-scrolling',
      value: [{
        type: 'token',
        value: {
          type: 'ident',
          value: 'touch'
        }
      }]
    }
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
      Property: {
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
      Property: {
        size(property) {
          if (property.value.value[0].type === 'length') {
            let value = { type: 'length-percentage', value: { type: 'dimension', value: property.value.value[0].value } };
            return [
              { property: 'width', value },
              { property: 'height', value }
            ];
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

test.run();
