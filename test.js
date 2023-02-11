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
  code: Buffer.from(`
  @breakpoints {
    .foo { color: yellow; }
  }

  .foo {
    color: red;
    @bar {
      width: 25px;
    }
  }
`),
  drafts: {
    nesting: true
  },
  targets: {
    safari: 16 << 16
  },
  customAtRules: {
    breakpoints: {
      // Syntax string defining the at rule prelude.
      // https://drafts.css-houdini.org/css-properties-values-api/#syntax-strings
      prelude: null,
      // Type of the at rule block.
      // Can be declaration-list, rule-list, or style-block.
      // https://www.w3.org/TR/css-syntax-3/#declaration-rule-list
      body: 'rule-list'
    },
    bar: {
      body: 'style-block'
    }
  },
  visitor: {
    Rule: {
      custom(rule) {
        console.log(rule.body);
      }
    },
    Length(length) {
      length.value *= 2;
      return length;
    }
  }
});

console.log(res.code.toString());
