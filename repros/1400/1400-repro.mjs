// #1400: `none` is only valid as the whole transition-property value, and a multi-item `transition`
// with a `none` item is invalid (css-transitions-1 §2.1, §2.5). Lightning CSS parses these as typed
// values, so the handler merges/overrides with them and changes meaning (Chromium-verified).
// Correct = the invalid declaration is kept as written and does not merge with or replace valid ones.
const mk = (name, css, check) => ({name, css, options: {minify: true}, check});
const has = (s) => (o) => o.replace(/\s/g, '').includes(s);
export const cases = [
  mk('earlier transition survives invalid transition-property', '.a{transition:opacity 1s;transition-property:none,width}', has('transition:opacity1s;')),
  mk('earlier transition survives invalid list shorthand', '.a{transition:width 1s;transition:none 1s,width 2s}', has('transition:width1s;')),
  mk('invalid longhands are not merged into a shorthand', '.a{transition-property:none,width;transition-duration:1s;transition-timing-function:ease;transition-delay:0s}', o => !/transition:none/.test(o)),
  // the visitor sees it as unparsed (as transition-property:5px is)
  {name: 'visitor sees transition-property:none,width as unparsed', options: {}, check: o => o === 'unparsed',
   async run(l) { let p; l.transform({filename: 'a.css', code: Buffer.from('.a{transition-property:none,width}'), visitor: {Declaration(d) { p = d.property; }}}); return p; }},
  // controls: correct today
  mk('control: transition:none 1s is valid', '.a{transition:none 1s}', has('transition:none1s')),
  mk('control: transition-property:none is valid', '.a{transition-property:none}', has('transition-property:none')),
  mk('control: invalid 5px keeps the earlier transition', '.a{transition:opacity 1s;transition-property:5px}', has('transition:opacity1s;transition-property:5px')),
];
