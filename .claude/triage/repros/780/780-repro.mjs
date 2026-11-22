// #780: source map generated columns are counted in UTF-8 bytes instead of
// UTF-16 code units (Source Map v3), so after any non-ASCII character on an
// output line, later mappings drift right. With minified output (one line) the
// browser then resolves a rule to an earlier rule's original line.
// check(): the rule `.b` (original line 2, 0-based 1) maps back to line 1, col 0.

function lookup(lightningcss, require, css, opts = {}) {
  const {SourceMapConsumer} = require('source-map-js');
  const res = lightningcss.transform({filename: 'input.css', code: Buffer.from(css), sourceMap: true, minify: true, ...opts});
  const code = res.code.toString();
  const map = new SourceMapConsumer(JSON.parse(res.map.toString()));
  const idx = code.indexOf('.b');
  // JS string index == UTF-16 column on a single-line output
  const lineStart = code.lastIndexOf('\n', idx) + 1;
  const pos = map.originalPositionFor({line: code.slice(0, idx).split('\n').length, column: idx - lineStart, bias: SourceMapConsumer.GREATEST_LOWER_BOUND});
  return JSON.stringify({line: pos.line, column: pos.column});
}

const want = (line) => (out) => {
  const p = JSON.parse(out);
  return p.line === line && p.column === 0;
};

export const cases = [
  {
    name: 'ascii-control',
    run: (l, {require}) => lookup(l, require, `.a { content: 'x'; }\n.b { color: red; }`),
    check: want(2),
  },
  {
    name: 'latin1-accent',
    run: (l, {require}) => lookup(l, require, `.a { content: 'é'; }\n.b { color: red; }`),
    check: want(2),
  },
  {
    // Common real-world case: icon glyphs / arrows in `content`
    name: 'many-non-ascii-before',
    run: (l, {require}) =>
      lookup(l, require, Array.from({length: 20}, (_, i) => `.a${i}::before { content: '❯→✓'; }`).join('\n') + `\n.b { color: red; }`),
    check: want(21),
  },
  {
    name: 'emoji-surrogate-pair',
    run: (l, {require}) => lookup(l, require, `.a { content: '😀😀'; }\n.b { color: red; }`),
    check: want(2),
  },
  {
    // Reporter's setup: Sass output + inputSourceMap, then minify.
    name: 'input-source-map-chain',
    run: (l, {require}) => {
      const src = `.a { content: '→ é'; }\n.b { color: red; }`;
      const first = l.transform({filename: 'input.scss', code: Buffer.from(src), sourceMap: true});
      return lookup(l, require, first.code.toString(), {inputSourceMap: first.map.toString()});
    },
    check: want(2),
  },
];
