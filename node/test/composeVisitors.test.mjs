import { test } from 'uvu';
import * as assert from 'uvu/assert';
import { bundle, bundleAsync, transform, transformStyleAttribute } from '../index.mjs';
import composeVisitors from '../composeVisitors.js';
import path from 'path';

test('px to rem', () => {
  let res = transform({
    filename: 'test.css',
    minify: true,
    code: Buffer.from(`
      .foo {
        width: f1(f2(f1(10px)));
      }
    `),
    visitor: composeVisitors([
      // {
      //   Function: {
      //     f1(f) {
      //       console.log('a', f)
      //       if (f.arguments.length === 1 && f.arguments[0].type === 'length') {
      //         return {
      //           type: 'length',
      //           value: {
      //             unit: f.arguments[0].value.unit,
      //             value: f.arguments[0].value.value * 2
      //           }
      //         }
      //       }
      //     }
      //   }
      // },
      // {
      //   Function(f) {
      //     console.log('b', f)
      //     return f.arguments[0];
      //   }
      // },

      {
        Length(l) {
          console.log('a', l)
          return {
            unit: l.unit,
            value: l.value * 2
          }
        }
      },
      {
        Length(l) {
          console.log('b', l)
          return {
            unit: 'rem',
            value: l.value
          }
        }
      }

    ])
  });

  console.log(res.code.toString());
});

test.run();
