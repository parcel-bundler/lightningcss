const css = require('./');
const cssnano = require('cssnano');
const postcssMinify = require('@csstools/postcss-minify');
const postcssPresetEnv = require('postcss-preset-env');
const postcss = require('postcss');
const esbuild = require('esbuild');

let opts = {
  filename: process.argv[process.argv.length - 1],
  code: require('fs').readFileSync(process.argv[process.argv.length - 1]),
  minify: true,
  // source_map: true,
  targets: {
    chrome: 80 << 16,
    firefox: 80 << 16,
    safari: 12 << 16,
  }
};

async function bench(label, job) {
  let results = [];
  let percentageOfOriginalSize = 0;
  for (let i = 0; i < 100; i++) {
    const start = performance.now();
    percentageOfOriginalSize = await job();
    results.push(performance.now() - start);
  }

  results.sort();

  return {
    label: label,
    percentile50: `${results[49].toFixed(1)}ms`,
    percentile95: `${results[Math.floor(results.length * 0.95)].toFixed(1)}ms`,
    percentile99: `${results[98].toFixed(1)}ms`,
    max: `${results[99].toFixed(1)}ms`,
    percentageOfOriginalSize: `${percentageOfOriginalSize.toFixed(2)}%`
  };
}

async function sleep(duration) {
  return new Promise(resolve => setTimeout(resolve, duration));
}

async function run() {
  const cssnanoResult = await bench('cssnano', async () => {
    const result = await postcss([
      cssnano({ presets: [{ mergeLonghand : false} ] }),
    ]).process(opts.code, { from: opts.filename });
    return result.css.length / opts.code.length * 100;
  });

  await sleep(500);

  const postcssMinifyResult = await bench('postcss-preset-env + @csstools/postcss-minify', async () => {
    const result = await postcss([
      postcssPresetEnv({
        browsers: 'Chrome 80, Firefox 80, Safari 12',
      }),
      postcssMinify,
    ]).process(opts.code, { from: opts.filename });
    return result.css.length / opts.code.length * 100;
  });

  await sleep(500);

  const esbuildResult = await bench('esbuild', async () => {
    let r = await esbuild.transform(opts.code.toString(), {
      sourcefile: opts.filename,
      loader: 'css',
      minify: true
    });
    return r.code.length / opts.code.length * 100;
  });

  await sleep(500);

  const lightningcssResult = await bench('lightningcss', async () => {
    let res = css.transform(opts);
    return res.code.length / opts.code.length * 100;
  });

  console.table([cssnanoResult, postcssMinifyResult, esbuildResult, lightningcssResult]);
}

run();
