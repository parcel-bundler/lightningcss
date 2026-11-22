// #1406: math functions in resolution media features (and other <resolution> contexts).
// Desired: parsed and resolved (or kept); never a parse error. Controls: plain resolution values.
const m = (q) => `@media ${q}{.a{color:red}}`;
export const cases = [
  {name: 'resolution-calc-product', css: m('(resolution: calc(2 * 1x))'), options: {minify: true},
    check: (out) => !out.startsWith('ERROR') && /resolution(:|=)\s*(2x|2dppx|calc\(2 \* 1x\))/.test(out)},
  {name: 'min-resolution-calc-sum', css: m('(min-resolution: calc(1dppx + 1x))'), options: {minify: true},
    check: (out) => !out.startsWith('ERROR') && /(2x|2dppx|calc\()/.test(out)},
  {name: 'range-resolution-calc', css: m('(resolution >= calc(96dpi * 2))'), options: {minify: true},
    check: (out) => !out.startsWith('ERROR') && /resolution/.test(out)},
  {name: 'image-set-calc', css: '.a{background-image:image-set("a.png" calc(2 * 1x))}', options: {minify: true},
    check: (out) => !out.startsWith('ERROR')},
  {name: 'control-plain', css: m('(resolution: 2x)'), options: {minify: true},
    check: (out) => out === '@media (resolution:2x){.a{color:red}}'},
  {name: 'control-min-dpi', css: m('(min-resolution: 192dpi)'), options: {minify: true},
    check: (out) => out.includes('192dpi')},
];
