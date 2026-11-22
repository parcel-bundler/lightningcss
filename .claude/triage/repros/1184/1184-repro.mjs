// #1184 (also #1177, #1072): CSS Overflow 5 scroll navigation controls.
// Decision: support behind the existing `drafts.scrollNavigationControls` flag.
// Without the flag, the selectors must keep passing through unchanged (a warning is fine).
const t = (l, css, opts = {}) => {
  try {
    const r = l.transform({filename: 'in.css', code: Buffer.from(css), minify: true, ...opts});
    return r.code.toString() + (r.warnings.length ? ' WARN: ' + r.warnings.map(w => w.message).join('; ') : '');
  } catch (e) {
    return 'ERROR: ' + e.message;
  }
};
const flag = {drafts: {scrollNavigationControls: true}};
const clean = (sel) => (o) => !o.includes('ERROR') && !o.includes('WARN') && o.includes(sel);
export const cases = [
  {name: 'flag: ::scroll-marker', run: (l) => t(l, `.c li::scroll-marker{content:""}`, flag), check: clean('li::scroll-marker{')},
  {name: 'flag: ::scroll-marker-group', run: (l) => t(l, `.c::scroll-marker-group{display:flex}`, flag), check: clean('.c::scroll-marker-group{')},
  {name: 'flag: ::scroll-button(*)', run: (l) => t(l, `.c::scroll-button(*){content:"x"}`, flag), check: clean('.c::scroll-button(*){')},
  {name: 'flag: ::scroll-button(right)', run: (l) => t(l, `.c::scroll-button(right){content:">"}`, flag), check: clean('.c::scroll-button(right){')},
  // css-overflow-5: "The :disabled pseudo-class can apply to ::scroll-button()". Chromium accepts it.
  {name: 'flag: ::scroll-button(*):disabled', run: (l) => t(l, `.c::scroll-button(*):disabled{opacity:.5}`, flag), check: clean('.c::scroll-button(*):disabled{')},
  {name: 'flag: ::scroll-marker:target-current', run: (l) => t(l, `li::scroll-marker:target-current{color:red}`, flag), check: clean('li::scroll-marker:target-current{')},
  {name: 'flag: ::column::scroll-marker', run: (l) => t(l, `.c::column::scroll-marker{content:""}`, flag), check: clean('.c::column::scroll-marker{')},
  // Controls: without the flag these must still pass through unchanged, in both modes.
  {name: 'control: no flag passthrough', run: (l) => t(l, `.c::scroll-button(*):disabled{opacity:.5}li::scroll-marker:target-current{color:red}`), check: (o) => !o.startsWith('ERROR') && o.includes('.c::scroll-button(*):disabled{opacity:.5}') && o.includes('li::scroll-marker:target-current{color:red}')},
  {name: 'control: no flag errorRecovery passthrough', run: (l) => t(l, `.c::scroll-marker-group{display:flex}`, {errorRecovery: true}), check: (o) => o.startsWith('.c::scroll-marker-group{display:flex}')},
  {name: 'control: properties pass through', run: (l) => t(l, `.c{scroll-marker-group:after;scroll-target-group:auto}`), check: (o) => o === '.c{scroll-marker-group:after;scroll-target-group:auto}'},
];
