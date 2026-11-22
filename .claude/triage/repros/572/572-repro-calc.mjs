export const css = `.d{block-size:auto;block-size:calc-size(auto)}`;
export const options = {minify: true};
export const check = o => o.indexOf('block-size:auto') < o.indexOf('calc-size');
