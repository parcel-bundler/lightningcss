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

test('throws on an invalid pseudo-element inside :has()', () => {
  // Pseudo-elements are not valid within :has(), and :has() is a non-forgiving
  // relative selector list, so the whole selector is invalid rather than
  // producing an empty :has(). See https://github.com/parcel-bundler/lightningcss/issues/1239
  let error;
  try {
    transform({
      filename: 'test.css',
      code: Buffer.from('video:not(:has(::backdrop)) { color: red }'),
    });
  } catch (err) {
    error = err;
  }

  assert.ok(error, 'expected transform to throw');
  assert.equal(error.message, 'Invalid state');
});

test('throws on an empty or malformed :has()', () => {
  const invalid = {
    // An empty :has() is invalid (non-forgiving relative selector list).
    'foo:has() { color: red }': 'Unexpected end of input',
    // `slot="selection"` is not a valid selector (missing attribute brackets).
    'foo:has(slot="selection") { color: red }': `Unexpected token Delim('=')`,
  };

  for (let [code, message] of Object.entries(invalid)) {
    let error;
    try {
      transform({ filename: 'test.css', code: Buffer.from(code) });
    } catch (err) {
      error = err;
    }
    assert.ok(error, `expected transform to throw for ${code}`);
    assert.equal(error.message, message, `wrong message for ${code}`);
  }
});

test('drops the rule and warns for an invalid :has() when error recovery is enabled', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('video:not(:has(::backdrop)) { color: red } a { color: green }'),
    errorRecovery: true,
    minify: true,
  });

  // The invalid rule is dropped entirely (no invalid empty `:has()` is emitted),
  // the valid rule is preserved, and a warning is surfaced.
  assert.equal(res.code.toString(), 'a{color:green}');
  assert.equal(res.warnings.length, 1);
  assert.equal(res.warnings[0].message, 'Invalid state');
});

test.run();
