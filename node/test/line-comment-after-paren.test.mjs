import { test } from 'uvu';
import * as assert from 'uvu/assert';
import {webcrypto as crypto} from 'node:crypto';

let transform, Features;
if (process.env.TEST_WASM === 'node') {
  ({transform, Features} = await import('../../wasm/wasm-node.mjs'));
} else if (process.env.TEST_WASM === 'browser') {
  // Define crypto globally for old node.
  // @ts-ignore
  globalThis.crypto ??= crypto;
  let wasm = await import('../../wasm/index.mjs');
  await wasm.default();
  ({transform, Features} = wasm);
} else {
  ({transform, Features} = await import('../index.mjs'));
}

test('should parse line comments after closing parenthesis', () => {
  let css = `div {
  width: calc(
    5
  );//hi
}`;

  let res = transform({
    filename: 'test.css',
    code: Buffer.from(css),
    minify: true
  });

  assert.equal(res.code.toString(), 'div{width:calc(5)}');
});

test.run();