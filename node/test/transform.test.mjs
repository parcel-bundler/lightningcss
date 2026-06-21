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

test('can enable scroll navigation controls draft syntax', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('a:target-current { color: red }'),
    drafts: {
      scrollNavigationControls: true
    },
    minify: true
  });

  assert.equal(res.code.toString(), 'a:target-current{color:red}');
  assert.equal(res.warnings, []);
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

test('minification works as expected on older yet modern android versions', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { color: transparent; }'),
    minify: true,
    targets: {
      // According to MDN (https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/hex-color#browser_compatibility)
      // the Android WebView gained RGBA support alongside Chrome, on version
      // 62. Thus version 90 should minify 'transparent' to '#0000'.
      android: 95 << 16
    }
  });

  assert.equal(res.code.toString(), '.foo{color:#0000}');
})

test.run();
