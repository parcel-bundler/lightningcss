export async function run(l) {
  const css = `.c::scroll-button(right){content:">"} .c::scroll-button(*):disabled{opacity:.5}`;
  let out = [];
  try { out.push('strict: ' + l.transform({filename:'in.css', code: Buffer.from(css), minify: true}).code.toString()); } catch (e) { out.push('strict ERROR: ' + e.message); }
  let r = l.transform({filename:'in.css', code: Buffer.from(css), minify: true, errorRecovery: true});
  out.push('recovery: ' + r.code.toString() + ' warnings: ' + r.warnings.map(w=>w.message).join('; '));
  return out.join('\n');
}
export const check = (out) => !out.includes('not recognized');
