export const css = `.a{font-weight:bold;font-weight:9000}.b{font-style:normal;font-variant-caps:normal;font-weight:9000;font-stretch:normal;font-size:10px;line-height:normal;font-family:serif}.c{font-weight:0}`;
export const options = {minify: true};
export const check = (out) => !out.includes('9000') && !/font-weight:0\b/.test(out);
