const css = require('./native');

if (process.argv[process.argv.length - 1] !== __filename) {
  let opts = {
    filename: process.argv[process.argv.length - 1],
    code: require('fs').readFileSync(process.argv[process.argv.length - 1]),
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
  targets: {
    safari: 5 << 16,
    firefox: 14 << 16,
    opera: 10 << 16 | 5 << 8
  },
  code: Buffer.from(`

.foo + .bar:not(.baz) {
  transition: opacity 200ms;
}
`)});

console.log(res.toString());
