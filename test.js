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
  minify: true,
  sourceMap: true,
  targets: {
    safari: 4 << 16,
    firefox: 3 << 16 | 5 << 8,
    opera: 10 << 16 | 5 << 8
  },
  code: Buffer.from(`.imported {
    content: "yay, file support!";
  }
  
  .selector {
    margin: 1em;
    background-color: #f60;
  }
  
  .selector .nested {
    margin: 0.5em;
  }
`),
  inputSourceMap: JSON.stringify({
    "version": 3,
    "sourceRoot": "root",
    "file": "stdout",
    "sources": [
      "stdin",
      "sass/_variables.scss",
      "sass/_demo.scss"
    ],
    "sourcesContent": [
      "@import \"_variables\";\n@import \"_demo\";\n\n.selector {\n  margin: $size;\n  background-color: $brandColor;\n\n  .nested {\n    margin: $size / 2;\n  }\n}",
      "$brandColor: #f60;\n$size: 1em;",
      ".imported {\n  content: \"yay, file support!\";\n}"
    ],
    "mappings": "AEAA,SAAS,CAAC;EACR,OAAO,EAAE,oBAAqB;CAC/B;;AFCD,SAAS,CAAC;EACR,MAAM,ECHD,GAAG;EDIR,gBAAgB,ECLL,IAAI;CDUhB;;AAPD,SAAS,CAIP,OAAO,CAAC;EACN,MAAM,ECPH,KAAG;CDQP",
    "names": []
  }),
});

console.log(res.code.toString());
console.log(res.map.toString())
