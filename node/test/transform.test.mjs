import { transform, Features } from 'lightningcss';
import { test } from 'uvu';
import * as assert from 'uvu/assert';

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
    drafts: {
      nesting: true
    },
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
