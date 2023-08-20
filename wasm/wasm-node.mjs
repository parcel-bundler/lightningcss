import { Environment, napi } from 'napi-wasm';
import fs from 'fs';

let wasmBytes = fs.readFileSync(new URL('lightningcss_node.wasm', import.meta.url));
let wasmModule = new WebAssembly.Module(wasmBytes);
let instance = new WebAssembly.Instance(wasmModule, {
  env: napi
});
let env = new Environment(instance);
let wasm = env.exports;

export default async function init() {
  // do nothing. for backward compatibility.
}

export function transform(options) {
  return wasm.transform(options);
}

export function transformStyleAttribute(options) {
  return wasm.transformStyleAttribute(options);
}

export { browserslistToTargets } from './browserslistToTargets.js'
export { Features } from './flags.js'
