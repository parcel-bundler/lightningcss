// #1402: calc() type checking (number + length is invalid) and sign() of
// font/viewport-relative lengths folded at build time.
const er = {minify: true, errorRecovery: true};
export const cases = [
  {name: 'number-plus-length-strict', css: '.a { width: calc(1 + 1px) }', options: {minify: true},
    check: (out) => out.startsWith('ERROR')},
  {name: 'number-plus-length-recovery', css: '.a { width: calc(1 + 1px) }', options: er,
    check: (out) => /warnings/.test(out)},
  {name: 'margin-merge-invalid', css: '.b { margin-top: 1px; margin-right: 1px; margin-bottom: 1px; margin-left: calc(1 + 1px) }', options: er,
    check: (out) => !/margin:1px 1px 1px calc/.test(out)},
  // Plausible typo (missing unit): the invalid declaration must not replace/absorb valid ones.
  {name: 'typo-dedupe-drops-fallback', css: '.a{width:100px;width:calc(100% - 20)}', options: er,
    check: (out) => out.includes('100px')},
  {name: 'typo-merge-into-margin', css: '.a{margin:0 auto;margin-left:calc(50% - 10)}', options: er,
    check: (out) => !/margin:0 auto 0 calc/.test(out)},
  // Same root cause: hue channel keyword h is a <number> (css-color-5), so h + 1deg is invalid; Chromium rejects it.
  {name: 'relative-color-h-plus-deg', css: '.a{color:hsl(from red calc(h + 1deg) s l)}', options: {minify: true},
    check: (out) => out.startsWith('ERROR')},
  {name: 'sign-em', css: '.c { width: calc(sign(1em) * 10px) }', options: {minify: true},
    check: (out) => out.includes('sign(1em)')},
  {name: 'sign-em-sum', css: '.d { margin-left: calc(5px + 10px * sign(1em)) }', options: {minify: true},
    check: (out) => out.includes('sign(1em)')},
  {name: 'sign-vw', css: '.d { margin-left: calc(10px * sign(1vw)) }', options: {minify: true},
    check: (out) => out.includes('sign(1vw)')},
  // Controls
  {name: 'control-sign-px', css: '.e { width: calc(sign(2px) * 10px) }', options: {minify: true},
    check: (out) => out === '.e{width:10px}'},
  {name: 'control-sign-sum', css: '.e { width: calc(sign(100vw - 600px) * 10px) }', options: {minify: true},
    check: (out) => out.includes('sign(100vw - 600px)')},
  {name: 'control-valid-sum', css: '.e { width: calc(1px + 1em) }', options: {minify: true},
    check: (out) => out === '.e{width:calc(1px + 1em)}'},
  {name: 'control-relative-color-h-plus-number', css: '.a{color:hsl(from red calc(h + 1) s l)}', options: {minify: true},
    check: (out) => !out.startsWith('ERROR') && out.includes('color:')},
  {name: 'control-number-sum', css: '.e { line-height: calc(1 + 0.5) }', options: {minify: true},
    check: (out) => out === '.e{line-height:1.5}'},
];
