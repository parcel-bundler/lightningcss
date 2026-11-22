// A valid y-keyword-first 3-value origin prints as an INVALID value (Chromium drops it).
export const css = `.a{transform-origin:bottom left 0}.b{transform-origin:top right 20px}`;
export const options = {minify: true};
export const check = (out) => !/left 0 bottom|right 20px top/.test(out);
