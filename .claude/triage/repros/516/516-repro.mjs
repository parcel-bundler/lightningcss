// #516: cssModules pattern with [name] and no filename
export function run(l) {
  const r = l.transform({cssModules: {pattern: 'my-company-[name]-[hash]-[local]'}, code: Buffer.from(`.logo { background: skyblue; }`)});
  return r.code.toString() + '\n' + JSON.stringify(r.exports);
}
export const check = (out) => !out.startsWith('ERROR');
