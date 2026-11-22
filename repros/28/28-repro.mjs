// #28: split a stylesheet by media query. Not a built-in feature; this pins the
// userland workaround (lower nesting with targets in one transform(), then a second
// transform() whose StyleSheet visitor keeps or drops top-level `@media print`).
// check() = the workaround still produces the expected split.
const css = `.foo { color: rebeccapurple; @media print { color: black; } }
.bar { margin: 0 }
@media print { .bar { display: none } }`;
const targets = {chrome: 100 << 16}; // lowers nesting

const isPrint = (r) => r.type === 'media'
  && r.value.query.mediaQueries.length === 1
  && JSON.stringify(r.value.query.mediaQueries[0]) === JSON.stringify({qualifier: null, mediaType: 'print', condition: null});

// Pass 1 lowers nesting (visitors run before lowering); pass 2 filters top-level rules.
const split = (l, keepPrint) => l.transform({
  filename: 'input.css', minify: true,
  code: l.transform({filename: 'input.css', code: Buffer.from(css), targets}).code,
  visitor: {StyleSheet(ss) { ss.rules = ss.rules.filter((r) => isPrint(r) === keepPrint); return ss; }},
}).code.toString();

export const cases = [
  {
    name: 'split-screen-part',
    run: (l) => split(l, false),
    check: (out) => out === '.foo{color:#639}.bar{margin:0}',
  },
  {
    name: 'split-print-part',
    run: (l) => split(l, true),
    check: (out) => out === '@media print{.foo{color:#000}.bar{display:none}}',
  },
];
