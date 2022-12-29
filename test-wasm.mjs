import init, { transform } from './wasm/index.mjs';

await init();

let encoder = new TextEncoder();
let decoder = new TextDecoder();
let code = encoder.encode('.foo { color: yellow }');

let res = transform({
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
