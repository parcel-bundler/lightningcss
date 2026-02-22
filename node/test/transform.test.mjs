import { test } from 'uvu';
import * as assert from 'uvu/assert';
import {webcrypto as crypto} from 'node:crypto';

let transform, transformStyleAttribute, Features;
if (process.env.TEST_WASM === 'node') {
  ({transform, transformStyleAttribute, Features} = await import('../../wasm/wasm-node.mjs'));
} else if (process.env.TEST_WASM === 'browser') {
  // Define crypto globally for old node.
  // @ts-ignore
  globalThis.crypto ??= crypto;
  let wasm = await import('../../wasm/index.mjs');
  await wasm.default();
  ({transform, transformStyleAttribute, Features} = wasm);
} else {
  ({transform, transformStyleAttribute, Features} = await import('../index.mjs'));
}

test('can enable non-standard syntax', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo >>> .bar { color: red }'),
    nonStandard: {
      deepSelectorCombinator: true
    },
    minify: true
  });

  assert.equal(res.code.toString(), '.foo>>>.bar{color:red}');
});

test('can enable features without targets', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { .bar { color: red }}'),
    minify: true,
    include: Features.Nesting
  });

  assert.equal(res.code.toString(), '.foo .bar{color:red}');
});

test('can disable features', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { color: lch(50.998% 135.363 338) }'),
    minify: true,
    targets: {
      chrome: 80 << 16
    },
    exclude: Features.Colors
  });

  assert.equal(res.code.toString(), '.foo{color:lch(50.998% 135.363 338)}');
});

test('can disable prefixing', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { user-select: none }'),
    minify: true,
    targets: {
      safari: 15 << 16
    },
    exclude: Features.VendorPrefixes
  });

  assert.equal(res.code.toString(), '.foo{user-select:none}');
});

test('minifyWhitespace can compact output without semantic minification', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.a { color: red; } .a { color: blue; }'),
    minify: false,
    minifyWhitespace: true
  });

  assert.equal(res.code.toString(), '.a{color:red}.a{color:#00f}');
});

test('minifyWhitespace can force pretty output with semantic minification', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.a { color: red; } .a { color: blue; }'),
    minify: true,
    minifyWhitespace: false
  });

  let code = res.code.toString();
  assert.ok(code.includes('\n'));
  assert.ok(code.includes('color: #00f;'));
  assert.ok(!code.includes('color: red;'));
});

test('minifyWhitespace decouples formatting when minify is omitted', () => {
  let compact = transform({
    filename: 'test.css',
    code: Buffer.from('.a { color: red; } .a { color: blue; }'),
    minifyWhitespace: true
  });
  assert.equal(compact.code.toString(), '.a{color:red}.a{color:#00f}');

  let pretty = transform({
    filename: 'test.css',
    code: Buffer.from('.a { color: red; } .a { color: blue; }'),
    minifyWhitespace: false
  });
  let code = pretty.code.toString();
  assert.ok(code.includes('\n'));
  assert.ok(code.includes('color: red;'));
  assert.ok(code.includes('color: #00f;'));
});

test('style attributes support minifyWhitespace override', () => {
  let compact = transformStyleAttribute({
    filename: 'test.css',
    code: Buffer.from('color: yellow; flex: 1 1 auto'),
    minify: false,
    minifyWhitespace: true
  });
  assert.equal(compact.code.toString(), 'color:#ff0;flex:auto');

  let pretty = transformStyleAttribute({
    filename: 'test.css',
    code: Buffer.from('color: yellow; flex: 1 1 auto'),
    minify: true,
    minifyWhitespace: false
  });
  assert.equal(pretty.code.toString(), 'color: #ff0; flex: auto');

  let omittedMinify = transformStyleAttribute({
    filename: 'test.css',
    code: Buffer.from('color: yellow; flex: 1 1 auto'),
    minifyWhitespace: false
  });
  assert.equal(omittedMinify.code.toString(), 'color: #ff0; flex: auto');
});

test.run();
