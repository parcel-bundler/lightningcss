// #695 variant: different values (unprefixed blur, -webkit- none) -- what wins?
export const css = `.a { backdrop-filter: blur(10px); -webkit-backdrop-filter: blur(4px); }`;
export const options = {minify: true, targets: {chrome: 120 << 16, safari: 15 << 16}};
export const check = (out) => /(^|[;{])backdrop-filter:blur\(10px\)/.test(out);
