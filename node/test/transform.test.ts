/// <reference types="vite/types/importMeta.d.ts" />

import {beforeAll, expect, test} from 'vitest';
import type {
  Features as FeaturesType,
  transform as Transform,
} from '../index.js';

let transform: typeof Transform;
let Features: typeof FeaturesType;

beforeAll(async () => {
  if (import.meta.env.TEST_WASM === 'node') {
    ({transform, Features} = await import('../../wasm/wasm-node.mjs'));
  } else if (import.meta.env.TEST_WASM === 'browser') {
    let wasm = await import('../../wasm/index.mjs');
    await wasm.default();
    ({transform, Features} = wasm);
  } else {
    ({transform, Features} = await import('../index.mjs'));
  }
});

test('can enable non-standard syntax', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo >>> .bar { color: red }'),
    nonStandard: {
      deepSelectorCombinator: true,
    },
    minify: true,
  });

  expect(res.code.toString()).toBe('.foo>>>.bar{color:red}');
});

test('can enable features without targets', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { .bar { color: red }}'),
    minify: true,
    include: Features.Nesting,
  });

  expect(res.code.toString()).toBe('.foo .bar{color:red}');
});

test('can disable features', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { color: lch(50.998% 135.363 338) }'),
    minify: true,
    targets: {
      chrome: 80 << 16,
    },
    exclude: Features.Colors,
  });

  expect(res.code.toString()).toBe('.foo{color:lch(50.998% 135.363 338)}');
});

test('can disable prefixing', () => {
  let res = transform({
    filename: 'test.css',
    code: Buffer.from('.foo { user-select: none }'),
    minify: true,
    targets: {
      safari: 15 << 16,
    },
    exclude: Features.VendorPrefixes,
  });

  expect(res.code.toString()).toBe('.foo{user-select:none}');
});
