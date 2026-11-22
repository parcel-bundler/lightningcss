// #1404: a declaration whose value fails its property's grammar (no var()) is kept as written as an
// Unparsed property, in strict and errorRecovery mode alike. That is deliberate (src/properties/mod.rs:
// "err on the safe side"), and the browser drops it, so keeping it preserves meaning.
// What must also hold: such a declaration must not REPLACE an earlier valid one, because the browser
// keeps the earlier one. FallbackHandler (color, fill, stroke, filter, text-shadow, ...) gets this
// wrong; same root cause as #1392.
const mk = (name, css, check, extra) => ({name, css, options: {minify: true, errorRecovery: true, ...(extra || {})}, check});
const keeps = (decl) => (o) => o.replace(/\s/g, '').includes(decl);
export const cases = [
  // controls (correct today): kept as written
  mk('control: background-color:bananas kept', '.a{background-color:bananas}', keeps('background-color:bananas')),
  mk('control: width fallback kept before invalid', '.a{width:10px;width:notalength}', keeps('width:10px;width:notalength')),
  mk('control: display fallback kept before invalid', '.a{display:flex;display:nonsense}', keeps('display:flex;display:nonsense')),
  mk('control: strict mode passes through too', '.a{width:notalength}', keeps('width:notalength'), {errorRecovery: false}),
  // bugs (same root cause as #1392): earlier valid declaration is dropped
  mk('color fallback before invalid value', '.a{color:red;color:bananas}', keeps('color:red;')),
  mk('color fallback before unknown function', '.a{color:red;color:contrast-color(red)}', keeps('color:red;')),
  mk('color fallback before empty value', '.a{color:red;color:}', keeps('color:red;')),
  mk('fill fallback before invalid value', '.a{fill:red;fill:bananas}', keeps('fill:red;')),
  mk('filter fallback before invalid value', '.a{filter:blur(2px);filter:bananas(1)}', keeps('filter:blur(2px);')),
  mk('text-shadow fallback before invalid value', '.a{text-shadow:1px 1px red;text-shadow:bananas}', keeps('text-shadow:1px 1px red;')),
  // control: var() is valid at parse time, so dropping the earlier value is right
  mk('control: color before var() may be dropped', '.a{color:red;color:var(--x)}', o => o.replace(/\s/g,'').includes('color:var(--x)')),
];
