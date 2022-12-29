// @ts-check

import { Environment, napi } from './node_modules/napi-wasm/index.mjs';

const url = new URL('target/wasm32-unknown-unknown/debug/lightningcss_node.wasm', import.meta.url);

const { instance } = await WebAssembly.instantiateStreaming(fetch(url), {
  env: napi
});

let env = new Environment(instance);
console.log(env.instance.exports)
let encoder = new TextEncoder();
let decoder = new TextDecoder();
let code = encoder.encode('.foo { color: yellow }');

let res = env.exports.transform({
  filename: 'test.js',
  code,
  minify: true,
  cssModules: true,
  // sourceMap: true,
  // targets: {
  //   chrome: 95 << 16
  // }
  visitor: {
    Color(color) {
      console.log(color)
      color.g = 0;
      return color;
    }
  }
});

console.log(res, decoder.decode(res.code))
