// #491: more flexible cssModules patterns (truncation like [hash:5] / [local:4], or a function).
// Feature request; cases marked "feature:" are the requested syntax (pending maintainer decision),
// the controls pin down the placeholders that work today.
function run(pattern) {
  return (l) => {
    try {
      const r = l.transform({filename: 'src/very-long-component-name.module.css', code: Buffer.from('.navigation-item { color: red }'), cssModules: {pattern}});
      return r.exports['navigation-item'].name;
    } catch (e) {
      return 'ERROR: ' + e.message;
    }
  };
}
export const cases = [
  {name: 'feature: [hash:5] truncation', run: run('x-[hash:5]-[local]'), check: (out) => /^x-[\w-]{5}-navigation-item$/.test(out)},
  {name: 'feature: [local:4] truncation', run: run('[hash]-[local:4]'), check: (out) => !out.startsWith('ERROR') && out.endsWith('-navi')},
  {name: 'control: [name]-[hash]-[local]', run: run('w-[name]-[hash]-[local]'), check: (out) => out.startsWith('w-very-long-component-name-module-') && out.endsWith('-navigation-item')},
  {name: 'control: unknown placeholder errors', run: run('[foo]-[local]'), check: (out) => out.startsWith('ERROR') && out.includes('unknown placeholder')},
];
