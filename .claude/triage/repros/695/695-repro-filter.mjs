// #695 comparison: same pattern for another prefixed property (user-select) -- is it backdrop-filter-specific?
export const css = `.a { user-select: none; -webkit-user-select: none; mask-image: url(a.png); -webkit-mask-image: url(a.png) }`;
export const options = {minify: true, targets: {chrome: 120 << 16, safari: 15 << 16}};
export const check = (out) => /(^|[;{])user-select:none/.test(out);
