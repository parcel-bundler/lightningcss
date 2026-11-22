// #1065: identity StyleSheet visitor on a var() declaration
export function run({transform}) {
  return transform({filename: 'a.css', code: Buffer.from('.foo { color: var(--color-red); }'), minify: true,
    visitor: {StyleSheet(s) { return s; }}}).code.toString();
}
export const check = (out) => out === '.foo{color:var(--color-red)}';
