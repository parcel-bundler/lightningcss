// #572 umbrella: declaration order inside one rule. Each case: [name, input, predicate on minified output (true = correct), tracked-by].
const cases = [
  ['anim-timeline (orig report)', `.item{animation:fade both;animation-timeline:scroll(root block)}`,
    // timeline must stay a separate longhand after the shorthand: no engine accepts a timeline inside `animation`
    o => !/animation:[^;}]*(scroll|view)\(/.test(o) && o.indexOf('animation-timeline') > o.indexOf('animation:'), '#1283 #1342 #1369, PR #1306'],
  ['anim-range', `.foo{animation:linear foo;animation-timeline:view();animation-range:entry-crossing 0% exit-crossing 100%}`,
    o => !/animation:[^;}]*view\(/.test(o) && o.indexOf('animation-range') > o.indexOf('animation:'), '#1283, PR #1306'],
  ['transition-behavior', `.t{transition:ease opacity .3s;transition-behavior:allow-discrete}`,
    o => o.indexOf('transition-behavior') > o.indexOf('transition:') || /transition:[^;}]*allow-discrete/.test(o), '#1084, PR #1365'],
  ['animation-composition', `.a{animation:fade 1s;animation-composition:add}`,
    o => o.indexOf('animation-composition') > o.indexOf('animation:'), '#1084, PR #1365'],
  ['font-variant-numeric', `p{font:12px serif;font-variant-numeric:oldstyle-nums}`,
    o => o.indexOf('font-variant-numeric') > o.indexOf('font:'), '#1207'],
  ['position-var', `.x{position:static;position:var(--x,static)}`, o => o.indexOf('position:static') < o.indexOf('position:var'), '#1225, PR #1387'],
  ['caret', `a{caret-color:red;caret:auto;caret-color:blue}`, o => /caret:auto;caret-color:#00f/.test(o), '#1362'],
  ['height-var', `.h{height:60px;height:var(--h)}`, o => o.indexOf('60px') < o.indexOf('var('), 'fixed 1.28.0 (22a8b6f)'],
  ['calc-size', `.d{block-size:auto;block-size:calc-size(auto)}`, o => o.indexOf('block-size:auto') < o.indexOf('calc-size'), 'fixed 1.28.0 (22a8b6f)'],
];
const targetSets = {none: undefined, chrome120: {chrome: 120 << 16}, safari16: {safari: 16 << 16}};
export async function run(l) {
  const lines = [];
  for (const [name, css, ok, ref] of cases) for (const [tn, targets] of Object.entries(targetSets)) {
    let out;
    try { out = l.transform({filename: 'a.css', code: Buffer.from(css), minify: true, targets}).code.toString(); }
    catch (e) { out = 'ERROR ' + e.message; }
    lines.push(`${ok(out) ? 'ok  ' : 'BAD '} ${name}/${tn} [${ref}]: ${out}`);
  }
  return lines.join('\n');
}
export const check = out => !out.includes('BAD ');
