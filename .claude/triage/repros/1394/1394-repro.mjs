// #1394 (duplicate of #949): numbers are printed with 6 significant digits.
// check() = behavior we want per the #949 direction: don't lose precision the
// author wrote / keep repeating fractions exact (calc kept or fraction kept).
const opt = {minify: true};
export const cases = [
  {
    // Reporter's case: an author-written value with 8 significant digits.
    name: 'eight-digit-length',
    css: `.e{width:1234.5678px}`,
    options: opt,
    check: (out) => !out.includes('1234.57px'),
  },
  {
    // Reporter's case inside calc() that can't fold.
    name: 'eight-digit-in-calc',
    css: `.d{width:calc(1234.5678px + 1em)}`,
    options: opt,
    check: (out) => !out.includes('1234.57px'),
  },
  {
    // Cosmetic: rounding 9.9999999e-23 up prints `10e-23` instead of `1e-22`.
    name: 'exponent-rounds-to-10e',
    css: `.a{width:9.9999999e-23px}`,
    options: opt,
    check: (out) => !/\b10e-/.test(out),
  },
  {
    // #949: repeating decimal from calc(4 / 3); ideally keep the fraction.
    name: 'repeating-fraction-line-height',
    css: `.a{line-height:calc(4 / 3)}`,
    options: opt,
    check: (out) => !out.includes('1.33333}'),
  },
  {
    // #949-style percentage from a fraction.
    name: 'repeating-fraction-percent',
    css: `.a{width:calc(100% / 3)}`,
    options: opt,
    check: (out) => !out.includes('33.3333%}'),
  },
  {
    // Control: short values are unchanged.
    name: 'control-short-values',
    css: `.a{width:12.5px;line-height:1.25}`,
    options: opt,
    check: (out) => out === '.a{width:12.5px;line-height:1.25}',
  },
];
