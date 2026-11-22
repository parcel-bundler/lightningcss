export const css = `body {
  background-image: image-set(
    url('./bg.jpg') 1x,
    url('./bg@2x.jpg') 2x
  );
}`;
export async function run(l, {require}) {
  const targets = l.browserslistToTargets(require('browserslist')('>=0.0%'));
  return l.transform({filename: 'styles.css', code: Buffer.from(css), minify: false, targets}).code.toString();
}
// correct = no duplicate declaration
export const check = (out) => { const d = out.split('\n').map(s=>s.trim()).filter(s=>s.startsWith('background-image')); return new Set(d).size === d.length; };
