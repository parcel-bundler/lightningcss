/// <reference types="vite/types/importMeta.d.ts" />

import fs from 'node:fs';
import {beforeAll, expect, test} from 'vitest';
import type {bundle as Bundle, transform as Transform} from '../index.js';

let bundle: typeof Bundle;
let transform: typeof Transform;

beforeAll(async () => {
  if (import.meta.env.TEST_WASM === 'node') {
    ({bundle, transform} = await import('../../wasm/wasm-node.mjs'));
  } else if (import.meta.env.TEST_WASM === 'browser') {
    let wasm = await import('../../wasm/index.mjs');
    await wasm.default();
    transform = wasm.transform;
    bundle = function (options) {
      return wasm.bundle({
        ...options,
        resolver: {
          read: (filePath) => fs.readFileSync(filePath, 'utf8'),
        },
      });
    };
  } else {
    ({bundle, transform} = await import('../index.mjs'));
  }
});

test('declaration list', () => {
  let definitions = new Map();
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @theme spacing {
        foo: 16px;
        bar: 32px;
      }

      .foo {
        width: theme('spacing.foo');
      }
    `),
    customAtRules: {
      theme: {
        prelude: '<custom-ident>',
        body: 'declaration-list',
      },
    },
    visitor: {
      Rule: {
        custom: {
          theme(rule) {
            for (let decl of rule.body.value.declarations) {
              if (decl.property === 'custom') {
                definitions.set(
                  rule.prelude.value + '.' + decl.value.name,
                  decl.value.value,
                );
              }
            }
            return [];
          },
        },
      },
      Function: {
        theme(f) {
          if (
            f.arguments[0].type === 'token' &&
            f.arguments[0].value.type === 'string'
          ) {
            return definitions.get(f.arguments[0].value.value);
          }
        },
      },
    },
  });

  expect(res.code.toString()).toBe('.foo{width:16px}');
});

test('mixin', () => {
  let mixins = new Map();
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @mixin color {
        color: red;

        &.bar {
          color: yellow;
        }
      }

      .foo {
        @apply color;
      }
    `),
    targets: {chrome: 100 << 16},
    customAtRules: {
      mixin: {
        prelude: '<custom-ident>',
        body: 'style-block',
      },
      apply: {
        prelude: '<custom-ident>',
      },
    },
    visitor: {
      Rule: {
        custom: {
          mixin(rule) {
            mixins.set(rule.prelude.value, rule.body.value);
            return [];
          },
          apply(rule) {
            return mixins.get(rule.prelude.value);
          },
        },
      },
    },
  });

  expect(res.code.toString()).toBe('.foo{color:red}.foo.bar{color:#ff0}');
});

test('rule list', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @breakpoint 1024px {
        .foo { color: yellow; }
      }
    `),
    customAtRules: {
      breakpoint: {
        prelude: '<length>',
        body: 'rule-list',
      },
    },
    visitor: {
      Rule: {
        custom: {
          breakpoint(rule) {
            return {
              type: 'media',
              value: {
                query: {
                  mediaQueries: [
                    {
                      mediaType: 'all',
                      condition: {
                        type: 'feature',
                        value: {
                          type: 'range',
                          name: 'width',
                          operator: 'less-than-equal',
                          value: rule.prelude,
                        },
                      },
                    },
                  ],
                },
                rules: rule.body.value,
                loc: rule.loc,
              },
            };
          },
        },
      },
    },
  });

  expect(res.code.toString()).toBe('@media (width<=1024px){.foo{color:#ff0}}');
});

test('style block', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        @breakpoint 1024px {
          color: yellow;

          &.bar {
            color: red;
          }
        }
      }
    `),
    targets: {
      chrome: 105 << 16,
    },
    customAtRules: {
      breakpoint: {
        prelude: '<length>',
        body: 'style-block',
      },
    },
    visitor: {
      Rule: {
        custom: {
          breakpoint(rule) {
            return {
              type: 'media',
              value: {
                query: {
                  mediaQueries: [
                    {
                      mediaType: 'all',
                      condition: {
                        type: 'feature',
                        value: {
                          type: 'range',
                          name: 'width',
                          operator: 'less-than-equal',
                          value: rule.prelude,
                        },
                      },
                    },
                  ],
                },
                rules: rule.body.value,
                loc: rule.loc,
              },
            };
          },
        },
      },
    },
  });

  expect(res.code.toString()).toBe(
    '@media (width<=1024px){.foo{color:#ff0}.foo.bar{color:red}}',
  );
});

test('style block top level', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @test {
        .foo {
          background: black;
        }
      }
    `),
    customAtRules: {
      test: {
        body: 'style-block',
      },
    },
  });

  expect(res.code.toString()).toBe('@test{.foo{background:#000}}');
});

test('multiple', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @breakpoint 1024px {
        @theme spacing {
          foo: 16px;
          bar: 32px;
        }
      }
    `),
    customAtRules: {
      breakpoint: {
        prelude: '<length>',
        body: 'rule-list',
      },
      theme: {
        prelude: '<custom-ident>',
        body: 'declaration-list',
      },
    },
    visitor: {
      Rule: {
        custom(rule) {
          if (rule.name === 'breakpoint') {
            return {
              type: 'media',
              value: {
                query: {
                  mediaQueries: [
                    {
                      mediaType: 'all',
                      condition: {
                        type: 'feature',
                        value: {
                          type: 'range',
                          name: 'width',
                          operator: 'less-than-equal',
                          value: rule.prelude,
                        },
                      },
                    },
                  ],
                },
                rules: rule.body.value,
                loc: rule.loc,
              },
            };
          } else {
            return {
              type: 'style',
              value: {
                selectors: [[{type: 'pseudo-class', kind: 'root'}]],
                declarations: rule.body.value,
                loc: rule.loc,
              },
            };
          }
        },
      },
    },
  });

  expect(res.code.toString()).toBe(
    '@media (width<=1024px){:root{foo:16px;bar:32px}}',
  );
});

test('bundler', () => {
  let mixins = new Map();
  let res = bundle({
    filename: 'tests/testdata/apply.css',
    minify: true,
    targets: {chrome: 100 << 16},
    customAtRules: {
      mixin: {
        prelude: '<custom-ident>',
        body: 'style-block',
      },
      apply: {
        prelude: '<custom-ident>',
      },
    },
    visitor: {
      Rule: {
        custom: {
          mixin(rule) {
            mixins.set(rule.prelude.value, rule.body.value);
            return [];
          },
          apply(rule) {
            return mixins.get(rule.prelude.value);
          },
        },
      },
    },
  });

  expect(res.code.toString()).toBe('.foo{color:red}.foo.bar{color:#ff0}');
});
