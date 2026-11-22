// Safari newer than the compat data at build time: does the prefix survive?
export const css = `pre { user-select: all; }`;
export const options = {minify: true, targets: {safari: (26 << 16) | (5 << 8)}};
export const check = (out) => out.includes('-webkit-user-select');
