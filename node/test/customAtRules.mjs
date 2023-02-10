// @ts-check

import { test } from 'uvu';
import * as assert from 'uvu/assert';
import { transform } from '../index.mjs';

test('declaration list', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      @theme spacing {
        foo: 16px;
        bar: 32px;
      }
    `),
    customAtRules: {
      theme: {
        prelude: '<custom-ident>',
        body: 'declaration-list'
      }
    },
    visitor: {
      Rule: {
        custom: {
          theme(rule) {
            return {
              type: 'style',
              value: {
                selectors: [[{ type: 'pseudo-class', kind: 'root' }]],
                declarations: rule.body.value,
                loc: rule.loc
              }
            }
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), ':root{foo:16px;bar:32px}');
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
        body: 'rule-list'
      }
    },
    visitor: {
      Rule: {
        custom: {
          breakpoint(rule) {
            return {
              type: 'media',
              value: {
                query: {
                  mediaQueries: [{ mediaType: 'all', condition: { type: 'feature', value: { type: 'range', name: 'width', operator: 'less-than-equal', value: rule.prelude } } }]
                },
                rules: rule.body.value,
                loc: rule.loc
              }
            }
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '@media (width<=1024px){.foo{color:#ff0}}');
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
    drafts: {
      nesting: true
    },
    targets: {
      chrome: 105 << 16
    },
    customAtRules: {
      breakpoint: {
        prelude: '<length>',
        body: 'style-block'
      }
    },
    visitor: {
      Rule: {
        custom: {
          breakpoint(rule) {
            return {
              type: 'media',
              value: {
                query: {
                  mediaQueries: [{ mediaType: 'all', condition: { type: 'feature', value: { type: 'range', name: 'width', operator: 'less-than-equal', value: rule.prelude } } }]
                },
                rules: rule.body.value,
                loc: rule.loc
              }
            }
          }
        }
      }
    }
  });

  assert.equal(res.code.toString(), '@media (width<=1024px){.foo{color:#ff0}.foo.bar{color:red}}');
});
