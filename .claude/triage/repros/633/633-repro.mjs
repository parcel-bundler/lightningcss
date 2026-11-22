// #633: localsConvention-style export key transforms (e.g. camelCaseOnly).
// Controls only: Lightning CSS keys `exports` by the original name; any convention
// would have to be opt-in, so the default must stay as-is.
export function run(l) {
  const r = l.transform({filename: 'button.module.css', code: Buffer.from('.button-base { display: flex } .x_y { color: red }'), cssModules: true});
  return JSON.stringify(Object.keys(r.exports).sort());
}
export const check = (out) => out === JSON.stringify(['button-base', 'x_y']);
