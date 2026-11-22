// #1391: a later `position: <css-wide keyword>` (parsed as Unparsed, like var()) is emitted
// before an earlier buffered `position` value, inverting the cascade. Same root cause as #1225.
const last = (prop, out) => { const m = [...out.matchAll(new RegExp(`(?:^|[;{\\s])${prop}:\\s*([^;}\\s]+)`, 'g'))]; return m.length ? m[m.length - 1][1] : null; };
const lastPos = (out) => last('position', out);
export const cases = [
  ...['unset', 'initial', 'inherit', 'revert', 'revert-layer'].map((kw) => ({
    name: `absolute then ${kw} (minify)`,
    css: `.x{position:absolute;position:${kw}}`,
    options: {minify: true},
    check: (out) => lastPos(out) === kw,
  })),
  {
    name: 'not minified',
    css: `.x{position:absolute;position:unset}`,
    options: {minify: false},
    check: (out) => lastPos(out) === 'unset',
  },
  {
    name: 'merged adjacent rules',
    css: `.x{position:absolute;top:1px}.x{position:unset}`,
    options: {minify: true},
    check: (out) => lastPos(out) === 'unset',
  },
  {
    name: 'sticky + unset with prefixes',
    css: `.x{position:sticky;position:unset}`,
    options: {minify: true, targets: {safari: 12 << 16}},
    check: (out) => lastPos(out) === 'unset',
  },
  {
    name: 'direction rtl then unset (same family as #1225)',
    css: `.x{direction:rtl;direction:unset}`,
    options: {minify: true},
    check: (out) => last('direction', out) === 'unset',
  },
  {
    name: 'unicode-bidi isolate then initial',
    css: `.x{unicode-bidi:isolate;unicode-bidi:initial}`,
    options: {minify: true},
    check: (out) => last('unicode-bidi', out) === 'initial',
  },
  {
    name: 'control: unset then absolute',
    css: `.x{position:unset;position:absolute}`,
    options: {minify: true},
    check: (out) => lastPos(out) === 'absolute',
  },
  {
    name: 'control: display flex then inherit',
    css: `.x{display:flex;display:inherit}`,
    options: {minify: true},
    check: (out) => last('display', out) === 'inherit',
  },
];
