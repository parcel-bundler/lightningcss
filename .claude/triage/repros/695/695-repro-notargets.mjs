// #695 variant: no targets (Vite 8 cssMinify default passes targets though)
export const css = `.a { backdrop-filter: blur(10px); -webkit-backdrop-filter: blur(10px); }`;
export const options = {minify: true};
export const check = (out) => /(^|[;{])backdrop-filter:blur\(10px\)/.test(out);
