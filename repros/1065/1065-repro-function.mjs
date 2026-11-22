// #1065 comment: Function visitor returning a var() argument unchanged
export function run({transform}) {
  return transform({filename: 'a.css', code: Buffer.from('.a{padding: foo(var(--a))}'), minify: true,
    visitor: {Function: {foo(f) { return f.arguments[0]; }}}}).code.toString();
}
export const check = (out) => out === '.a{padding:var(--a)}';
