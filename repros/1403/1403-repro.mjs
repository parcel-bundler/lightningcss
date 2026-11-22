// transform-origin's optional third value (z) is read as the y side's offset.
const cases = ["left top 30px", "top left 30px", "center top 30px", "right bottom 30px", "0% 0% 30px", "center left 10px", "left 10px center", "left top", "50% 50% 0"];
export async function run(l) {
  return cases.map(c => c.padEnd(18) + ' -> ' + (() => { try { return l.transform({filename: 'a.css', code: Buffer.from(`.a{transform-origin:${c}}`), minify: true}).code.toString(); } catch (e) { return 'ERROR: ' + e.message; } })()).join('\n');
}
export const check = (out) => {
  const o = Object.fromEntries(out.split('\n').map(l => [l.slice(0, 18).trim(), l.slice(22)]));
  return !/transform-origin:0 30px}/.test(o['left top 30px']) && !/:30px 0}/.test(o['top left 30px']) && !/:50% 30px}/.test(o['center top 30px']);
};
