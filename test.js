const css = require('./native');

if (process.argv[process.argv.length - 1] !== __filename) {
  let opts = {
    filename: process.argv[process.argv.length - 1],
    code: require('fs').readFileSync(process.argv[process.argv.length - 1]),
    minify: true,
    targets: {
      chrome: 95 << 16
    }
  };

  console.time('optimize');
  let r = css.transform(opts);
  console.timeEnd('optimize')
  console.log(r.toString());
  return;
}

let res = css.transform({
  filename: __filename,
  minify: true,
  targets: {
    safari: 4 << 16,
    firefox: 3 << 16 | 5 << 8,
    opera: 10 << 16 | 5 << 8
  },
  code: Buffer.from(`

.foo + .bar:not(.baz) {
  -webkit-box-shadow: 2px 0 0 blue;
  box-shadow: 2px 0 0 red;
  box-sizing: border-box;
}
`)});

console.log(res.toString());
