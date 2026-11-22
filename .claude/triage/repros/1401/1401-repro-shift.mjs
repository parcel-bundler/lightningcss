// Merging a repeated (or prefixed) property shifts the remaining entries' durations.
const cases = [
  ".a{transition:width 1s, width 2s, height 3s}",
  ".a{transition:-webkit-transform .3s, transform .3s, opacity .5s}",
  ".a{transition:-webkit-transform .3s ease-out, transform .3s ease-out, opacity 1s linear, color 2s}",
];
export async function run(l) {
  return cases.map(c => c + '  ->  ' + l.transform({filename: 'a.css', code: Buffer.from(c), minify: true}).code.toString()).join('\n');
}
export const check = (out) => { const o = out.split("\n").map(l => l.split("  ->  ")[1]).join("\n"); return /opacity \.5s/.test(o) && /height 3s/.test(o) && /opacity 1s linear/.test(o); };
