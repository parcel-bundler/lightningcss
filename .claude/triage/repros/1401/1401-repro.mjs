// Repeated properties in a transition list: the LAST occurrence applies (css-transitions-1 §2.1).
const cases = [
  ".a{transition:width 1s, width 2s}",
  ".a{transition:1s, 2s}",
  ".a{transition-property:all, all; transition-duration:1s, 2s}",
  ".a{transition:width 1s, height 3s, width 2s}",
  ".a{transition:width 1s, all 2s}",
];
export async function run(l) {
  const out = [];
  for (const c of cases) for (const minify of [false, true]) {
    out.push(l.transform({filename: 'a.css', code: Buffer.from(c), minify}).code.toString().replace(/\s+/g, ' ').trim());
  }
  return out.join('\n');
}
// correct = no case keeps only a 1s duration for the repeated property
export const check = (out) => !/transition:width 1s}|transition:all 1s}|transition-property:all;transition-duration:1s,2s/.test(out.replace(/ /g, ''));
