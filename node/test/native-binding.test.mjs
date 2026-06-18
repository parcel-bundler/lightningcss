import {test} from 'uvu';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import vm from 'node:vm';
import {fileURLToPath} from 'node:url';

const dirname = path.dirname(fileURLToPath(import.meta.url));
const indexPath = path.join(dirname, '..', 'index.js');
const indexSource = fs.readFileSync(indexPath, 'utf8');

function loadWithMissingNativeBinding(platform, arch, family) {
  let context = {
    process: {platform, arch},
    module: {exports: {}},
    require(name) {
      if (name === 'detect-libc') {
        return {MUSL: 'musl', familySync: () => family};
      }

      let error = new Error(`Cannot find module '${name}'`);
      error.code = 'MODULE_NOT_FOUND';
      throw error;
    }
  };
  context.exports = context.module.exports;

  try {
    vm.runInNewContext(indexSource, context, {filename: indexPath});
  } catch (err) {
    return err;
  }

  throw new Error('Expected native binding load to fail');
}

test('reports the missing arm64 musl native package', () => {
  let error = loadWithMissingNativeBinding('linux', 'arm64', 'musl');

  assert.match(error.message, /linux-arm64-musl/);
  assert.match(error.message, /lightningcss-linux-arm64-musl/);
  assert.match(error.message, /\.\.\/lightningcss\.linux-arm64-musl\.node/);
  assert.equal(error.code, 'MODULE_NOT_FOUND');
  assert.equal(error.errors.length, 2);
});

test.run();
