export function run(l) {
  const r = l.transform({filename: 'src/logo.module.css', cssModules: {pattern: 'my-company-[name]-[hash]-[local]'}, code: Buffer.from(`.logo { background: skyblue; }`)});
  return r.code.toString();
}
export const check = (out) => out.includes('my-company-logo-module-');
