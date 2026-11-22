// #949: numbers are printed with 6 significant digits, so authored precision is lost.
export const css = `.a{line-height:1.3333333333}.b{line-height:calc(4/3)}.c{width:33.333333%}.d{line-height:1.428571429}`;
export const options = {minify: true};
// correct = enough digits kept that the f32 value round-trips (Chromium then lays out the same)
export const check = (out) => !/1\.33333[;}]/.test(out) && !out.includes('33.3333%') && !out.includes('1.42857}');
