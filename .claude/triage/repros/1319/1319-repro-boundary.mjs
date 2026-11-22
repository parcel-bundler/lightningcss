// Boundary check against MDN data for overflow.multiple_keywords (Safari 13.1, Chrome 68, Firefox 61)
export async function run(l) {
  const r = (css, targets) => l.transform({ filename: 'in.css', code: Buffer.from(css), minify: true, targets }).code.toString();
  const cases = {'safari 13': {safari: 13 << 16}, 'safari 13.1': {safari: (13 << 16) | (1 << 8)}, 'chrome 67': {chrome: 67 << 16}, 'chrome 68': {chrome: 68 << 16}, 'firefox 60': {firefox: 60 << 16}};
  return Object.entries(cases).map(([k, t]) => `${k}: ${r('.a{overflow-x:hidden;overflow-y:scroll}', t)} | ${r('.a{overflow:hidden scroll}', t)}`).join('\n');
}
export const check = (out) => out.split('\n').every(line => /^(safari 13|chrome 67|firefox 60):/.test(line) ? !line.includes('overflow:hidden scroll') : line.includes('overflow:hidden scroll'));
