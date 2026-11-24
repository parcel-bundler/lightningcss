// atan2() with percentages must not fold to a valid angle where % is invalid.
const opts = {minify: true};
export const cases = [
  {name: 'rotate', css: '.a{rotate:atan2(10%, 20%)}', options: opts, check: (out) => !/rotate:-?[\d.]+deg/.test(out)},
  {name: 'hue-rotate', css: '.a{filter:hue-rotate(atan2(10%, 20%))}', options: opts, check: (out) => !/hue-rotate\(-?[\d.]+deg\)/.test(out)},
  {name: 'control-lengths', css: '.a{rotate:atan2(10px, 20px)}', options: opts, check: (out) => out.includes('26.5651deg')},
];
