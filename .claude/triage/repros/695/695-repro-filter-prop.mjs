// #695 sibling: same handler for `filter` with -webkit-filter after it
export const css = `.a { filter: blur(10px); -webkit-filter: blur(10px); }`;
export const options = {minify: true, targets: {chrome: 120 << 16, safari: 15 << 16}};
export const check = (out) => /(^|[;{])filter:blur\(10px\)/.test(out);
