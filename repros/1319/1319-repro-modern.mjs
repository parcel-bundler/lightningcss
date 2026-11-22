export const css = `.a{overflow-x:hidden;overflow-y:scroll}`;
export const options = {minify: true, targets: {safari: 16 << 16}};
export const check = (out) => out.includes('overflow:hidden scroll');
