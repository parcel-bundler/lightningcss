// Control: same as 1065-repro.mjs but strips null fields before returning (reporters' workaround)
const strip = (v) => Array.isArray(v) ? v.map(strip) : (v && typeof v === 'object') ? Object.fromEntries(Object.entries(v).filter(([, x]) => x !== null).map(([k, x]) => [k, strip(x)])) : v;
export function run({transform}) {
  return transform({filename: 'a.css', code: Buffer.from('.foo { color: var(--color-red, blue); }'), minify: true,
    visitor: {StyleSheet(s) { return strip(s); }}}).code.toString();
}
export const check = (out) => out === '.foo{color:var(--color-red,blue)}';
