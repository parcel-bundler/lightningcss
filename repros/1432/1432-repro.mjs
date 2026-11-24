// NaN from asin() must keep its <angle> type; mixed-type expressions must not fold.
const opts = {minify: true, errorRecovery: true};
export const cases = [
  {name: 'atan2-angle-number', css: '.a{rotate:atan2(asin(2), 1)}', options: opts, check: (out) => !out.includes('5.10424e38')},
  {name: 'asin-times-px', css: '.a{margin-left:calc(asin(2) * 1px)}', options: opts, check: (out) => !out.includes('5.10424e38')},
  {name: 'control-finite', css: '.a{rotate:atan2(asin(0.5), 1)}', options: opts, check: (out) => out.includes('atan2')},
];
