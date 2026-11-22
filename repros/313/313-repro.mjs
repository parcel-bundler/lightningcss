// #313: when minifying, choose the quote character that needs fewer escapes.
// check(): true when the minified output has no escaped double quotes and still means the
// same thing (re-serializing it non-minified gives the same result as the input).
const norm = (l, css) => l.transform({filename: 'a.css', code: Buffer.from(css)}).code.toString();
const mk = (name, css) => ({
  name,
  async run(l) {
    let min = l.transform({filename: 'a.css', code: Buffer.from(css), minify: true}).code.toString();
    let same = norm(l, min) === norm(l, css);
    return `${min}\n/* round-trips: ${same} */`;
  },
  check: (o) => !o.split('\n')[0].includes('\\"') && o.includes('round-trips: true'),
});
const svg = `data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><path fill="red" d="M0 0h16v16H0z"/></svg>`;
export const cases = [
  mk('url-in-content', `.a{content:url('${svg}')}`),
  mk('url-in-background', `.a{background-image:url('${svg}')}`),
  mk('string-in-content', `.a::before{content:'say "hi"'}`),
  mk('font-family-string', `.a{font-family:'My "Quoted" Font'}`),
  mk('attr-selector', `a[title='say "hi"']{color:red}`),
  // Controls: no quotes inside, or more single quotes than double: double quotes stay shortest.
  {name: 'control-plain-string', css: `.a::before{content:'hi'}`, options: {minify: true}, check: (o) => o === '.a:before{content:"hi"}'},
  {name: 'control-apostrophes', css: `.a::before{content:"rock'n'roll"}`, options: {minify: true}, check: (o) => o === `.a:before{content:"rock'n'roll"}`},
];
