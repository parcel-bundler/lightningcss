// #660: a cssModules pattern without [local] (e.g. '[hash]') maps every class,
// keyframes name etc. in a file to the SAME scoped name, silently merging styles.
// Wanted (triage decision): distinct locals get distinct scoped names, or the pattern is rejected with an error.
const css = `.a { color: red } .b { color: blue } @keyframes k {} .c { animation: k 1s }`;
function names(l, pattern) {
  try {
    const r = l.transform({filename: 'button.module.css', code: Buffer.from(css), cssModules: {pattern}});
    return JSON.stringify(Object.fromEntries(Object.entries(r.exports).map(([k, v]) => [k, v.name])));
  } catch (e) {
    return 'ERROR: ' + e.message;
  }
}
const distinctOrError = (out) => {
  if (out.startsWith('ERROR')) return true;
  const v = Object.values(JSON.parse(out));
  return new Set(v).size === v.length;
};
export const cases = [
  {name: 'pattern [hash] alone', run: (l) => names(l, '[hash]'), check: distinctOrError},
  {name: 'pattern [name]-[hash]', run: (l) => names(l, '[name]-[hash]'), check: distinctOrError},
  {name: 'pattern [content-hash]', run: (l) => names(l, '[content-hash]'), check: distinctOrError},
  // control: default pattern is unique per local
  {name: 'control: default pattern', run: (l) => names(l, undefined), check: (out) => !out.startsWith('ERROR') && distinctOrError(out)},
];
