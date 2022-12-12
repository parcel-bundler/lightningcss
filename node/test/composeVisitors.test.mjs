import { test } from 'uvu';
import * as assert from 'uvu/assert';
import { bundle, bundleAsync, transform, transformStyleAttribute } from '../index.mjs';
import composeVisitors from '../composeVisitors.js';
import path from 'path';

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

  assert.equal(res.code.toString(), '.foo{width:1rem;color:#0f0}');
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
        Property: {
          size(v) {
            return [
              { property: 'unparsed', value: { propertyId: { property: 'width' }, value: v.value.value } },
              { property: 'unparsed', value: { propertyId: { property: 'height' }, value: v.value.value } }
            ];
          }
        }
      },
      {
        Property: {
          bg(v) {
            if (v.value.value[0].type === 'color') {
              return { property: 'background-color', value: v.value.value[0].value };
            }
          }
        }
      }
    ])
  });

  assert.equal(res.code.toString(), '.foo{width:16px;height:16px;background-color:#ff0}');
});

test('composed properties', () => {
  let visitors = [
    {
      Property: {
        size(v) {
          return [
            { property: 'width', value: { type: 'length-percentage', value: { type: 'dimension', value: v.value.value[0].value } } },
            { property: 'height', value: { type: 'length-percentage', value: { type: 'dimension', value: v.value.value[0].value } } },
          ];
        }
      }
    },
    {
      Property: {
        width(v) {
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
        Property: {
          color(v) {
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
      },
      {
        Property: {
          color(v) {
            if (v.value.g > 0) {
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

test('tokens and functions', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: f1(f2(f1(test)));
      }
    `),
    visitor: composeVisitors([
      {
        Function: {
          f1(f) {
            if (f.arguments.length === 1 && f.arguments[0].value.type === 'ident') {
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
        Function(f) {
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

test.run();
