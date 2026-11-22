export const css = `.a{font-weight:calc(2000)}.b{font-weight:calc(-5)}.c{font-weight:calc(1000 + 1)}.d{font-weight:calc(500)}`;
export const options = {minify: true};
export const check = (out) => !/font-weight:(2000|-5|1001)\b/.test(out);
