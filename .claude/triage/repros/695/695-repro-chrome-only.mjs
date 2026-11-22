// #695 variant: Chrome-only targets (no browser needing -webkit-)
export const css = `.a { backdrop-filter: blur(10px); -webkit-backdrop-filter: blur(10px); }`;
export const options = {minify: true, targets: {chrome: 120 << 16}};
export const check = (out) => /(^|[;{])backdrop-filter:blur\(10px\)/.test(out);
