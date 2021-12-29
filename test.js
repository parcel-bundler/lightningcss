const css = require('./');
const fs = require('fs');

if (process.argv[process.argv.length - 1] !== __filename) {
  let opts = {
    filename: process.argv[process.argv.length - 1],
    code: fs.readFileSync(process.argv[process.argv.length - 1]),
    minify: true,
    sourceMap: true,
    targets: {
      chrome: 95 << 16
    }
  };

  console.time('optimize');
  let r = css.transform(opts);
  console.timeEnd('optimize')
  // console.log(r.toString());
  console.log(r);
  let code = r.code;
  if (r.map) {
    code = code.toString() + `\n/*# sourceMappingURL=out.css.map */\n`;
  }
  fs.writeFileSync('out.css', code);
  if (r.map) {
    fs.writeFileSync('out.css.map', r.map);
  }
  return;
}

let res = css.transform({
  filename: __filename,
  minify: false,
  targets: {
    safari: 4 << 16,
    firefox: 3 << 16 | 5 << 8,
    opera: 10 << 16 | 5 << 8
  },
  code: Buffer.from(`
  @import "foo.css";
  @import "bar.css" print;
  @import "baz.css" supports(display: grid);

  .foo {
    composes: bar;
    composes: baz from "baz.css";
    color: pink;
  }

  .bar {
    color: red;
    background: url(test.jpg);
  }
`),
  drafts: {
    nesting: true
  },
  cssModules: true,
  analyzeDependencies: true
});

console.log(res.code.toString());
console.log(res.exports);
console.log(require('util').inspect(res.dependencies, { colors: true, depth: 50 }));
