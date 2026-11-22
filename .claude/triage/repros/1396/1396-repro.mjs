// #1396: a negated media range feature with `=` must keep its `not` (equality has no inverse range operator).
const t = (css, extra = {}) => (l) => l.transform({filename: 'a.css', code: Buffer.from(css), minify: true, ...extra}).code.toString();
const mk = (name, css, check, extra) => ({name, css, options: {minify: true, ...(extra || {})}, check});
export const cases = [
  mk('not (width = 10px)', '@media not (width = 10px){.a{color:red}}', o => /not/.test(o)),
  mk('screen and not (width = 10px)', '@media screen and (not (width = 10px)){.a{color:red}}', o => /not/.test(o)),
  mk('nested (not (width = 10px)) and (height > 5px)', '@media (not (width = 10px)) and (height > 5px){.a{color:red}}', o => /not/.test(o)),
  mk('not (10px = width) reversed', '@media not (10px = width){.a{color:red}}', o => /not/.test(o)),
  mk('not (resolution = 2dppx)', '@media not (resolution = 2dppx){.a{color:red}}', o => /not/.test(o)),
  mk('not (width = 10px), safari 12 lowering', '@media not (width = 10px){.a{color:red}}', o => /not/.test(o), {targets: {safari: 12 << 16}}),
  mk('not screen and (width = 10px) qualifier', '@media not screen and (width = 10px){.a{color:red}}', o => /not screen/.test(o)),
  // controls: these are correct today
  mk('control: not (width >= 10px) -> (width<10px)', '@media not (width >= 10px){.a{color:red}}', o => /width<10px|not \(width>=10px\)/.test(o)),
  mk('control: not (width: 10px) keeps not', '@media not (width: 10px){.a{color:red}}', o => /not \(width:10px\)/.test(o)),
  mk('control: @container not (width = 10px) keeps not', '.c{container-type:inline-size}@container not (width = 10px){.a{color:red}}', o => /not/.test(o)),
];
