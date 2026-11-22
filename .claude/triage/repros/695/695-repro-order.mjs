// #695 control: prefixed first
export const css = `.a { -webkit-backdrop-filter: blur(10px); backdrop-filter: blur(10px); }`;
export const options = {minify: true, targets: {chrome: 120 << 16, safari: 15 << 16}};
export const check = (out) => /(^|[;{])backdrop-filter:blur\(10px\)/.test(out);
