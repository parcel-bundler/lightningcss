import { Environment, napi } from 'napi-wasm';
import { await_promise_sync, createBundleAsync } from './async.mjs';
import fs from 'fs';
import {webcrypto as crypto} from 'node:crypto';

let wasmBytes = fs.readFileSync(new URL('lightningcss_node.wasm', import.meta.url));
let wasmModule = new WebAssembly.Module(wasmBytes);
let instance = new WebAssembly.Instance(wasmModule, {
  env: {
    ...napi,
    await_promise_sync,
    __getrandom_v03_custom: (ptr, len) => {
      let buf = env.memory.subarray(ptr, ptr + len);
      crypto.getRandomValues(buf);
    },
  },
});
instance.exports.register_module();
let env = new Environment(instance);
let wasm = env.exports;
let bundleAsyncInternal = createBundleAsync(env);

export default async function init() {
  // do nothing. for backward compatibility.
}

export function transform(options) {
  return wasm.transform(options);
}

export function transformStyleAttribute(options) {
  return wasm.transformStyleAttribute(options);
}

export function bundle(options) {
  return wasm.bundle({
    ...options,
    resolver: {
      read: (filePath) => fs.readFileSync(filePath, 'utf8')
    }
  });
}

export async function bundleAsync(options) {
  if (!options.resolver?.read) {
    options.resolver = {
      ...options.resolver,
      read: (filePath) => fs.readFileSync(filePath, 'utf8')
    };
  }

  return bundleAsyncInternal(options);
}

export { browserslistToTargets } from './browserslistToTargets.js'
export { Features } from './flags.js'
export { composeVisitors } from './composeVisitors.js';
