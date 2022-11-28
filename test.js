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
  // minify: true,
  // targets: {
  //   safari: 4 << 16,
  //   firefox: 3 << 16 | 5 << 8,
  //   opera: 10 << 16 | 5 << 8
  // },
  code: Buffer.from(`
  .selector .nested {
    width: 32px;
    --foo: var(--bar, 30px);
    background: linear-gradient(red, green);
  }

  @media (hover) and (width > 50px) {
    .foo {
      color: red;
      background: inline('.gitignore');
    }
  }
`),
  visitor: {
    visitLength(length) {
      if (length.unit === 'px') {
        return {
          unit: 'rem',
          value: length.value / 16
        }
      }
      return length;
    },
    visitColor(color) {
      console.log(color);
      return color;
    },
    visitImage(image) {
      // console.log(image.value.value);
      image.value.value[1].push('moz');
      return image;
    },
    visitProperty(property) {
      // console.log(require('util').inspect(property, {depth: 50}))
      if (property.property === 'background') {
        property.value[0].repeat.x = 'no-repeat';
        property.value[0].repeat.y = 'no-repeat';
      }

      return property;
    },
    // visitRule(rule) {
    //   console.log(require('util').inspect(rule, {depth: 10}));
    //   if (rule.type === 'style') {
    //     for (let selector of rule.value.selectors) {
    //       for (let component of selector) {
    //         if (component.type === 'class') {
    //           component.value = 'tw-' + component.value;
    //         }
    //       }
    //     }
    //   }
    //   return rule;
    // },
    visitMediaQuery(query) {
      // console.log(query);
      query.media_type = 'print';
      return query;
    },
    visitFunction(fn) {
      // console.log(require('util').inspect(fn, {depth: 50}));
      if (fn.name === 'inline') {
        return {
          name: 'url',
          arguments: [{
            type: 'token',
            value: {
              type: 'string',
              value: fs.readFileSync(fn.arguments[0].value.value).toString('base64'),
            }
          }]
        }
      }
      return fn;
    },
    visitSelector(selector) {
      console.log(require('util').inspect(selector, { depth: 10 }));
      for (let component of selector) {
        if (component.type === 'class') {
          component.value = 'tw-' + component.value;
        }
      }
      return selector;
    }
  },
});

console.log(res.code.toString());
