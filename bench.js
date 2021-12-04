const css = require('./native');
const cssnano = require('cssnano');
const postcss = require('postcss');
const esbuild = require('esbuild');
const csso = require('csso');

// node_modules/animate.css/animate.css
// node_modules/bootstrap/dist/css/bootstrap.css
// node_modules/tailwindcss/dist/tailwind.css

let opts = {
  filename: process.argv[process.argv.length - 1],
  code: require('fs').readFileSync(process.argv[process.argv.length - 1]),
  minify: true,
  // source_map: true,
  targets: {
    chrome: 95 << 16
  }
};

async function run() {
  await doCssNano();

  console.time('csso');
  let r = csso.minify(opts.code.toString(), {
    filename: opts.filename,
  });
  console.timeEnd('csso');
  console.log(r.css.length + ' bytes');
  console.log('');

  console.time('esbuild');
  r = await esbuild.transform(opts.code.toString(), {
    sourcefile: opts.filename,
    loader: 'css',
    minify: true,
  });
  console.timeEnd('esbuild');
  console.log(r.code.length + ' bytes');
  console.log('');

  console.time('parcel-css');
  r = css.transform(opts);
  console.timeEnd('parcel-css');
  console.log(r.code.length + ' bytes');
}

async function doCssNano() {
  console.time('cssnano');
  const result = await postcss([cssnano]).process(opts.code, {
    from: opts.filename,
  });
  console.timeEnd('cssnano');
  console.log(result.css.length + ' bytes');
  console.log('');
}

run();
