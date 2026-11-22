// #1235: stale StyleRuleKey hash in CssRuleList::minify (src/rules/mod.rs).
// The key's hash is computed once (ahash, random per-process seed) but merges and
// pops change/remove the rule it points at, so lookups go wrong
// nondeterministically. Amplify with many distinct copies in ONE rule list
// (each nested block is its own list, so the copies must share a parent) and
// run with --repeat N. check() = true means no rule/declaration lost.
const N = 300;

// Dropped-rule pattern from the issue comment, N distinct copies, top level.
const flat = Array.from({length: N}, (_, i) => `
.a${i}:disabled { pointer-events: none; }
.a${i}:disabled { opacity: 50%; }
.a${i}[aria-disabled="true"] { pointer-events: none; }
.a${i}[aria-disabled="true"] { opacity: 50%; }
.a${i}[aria-hidden="true"] { display: none; }`).join('\n');

// Many small independent lists, each with distinct hashed content. This is the
// effective amplifier: a small map makes the stale entry likely to share a probe
// group, and distinct selectors give each list an independent chance (identical
// `&` copies would hash the same and all behave alike within one process).
const many = Array.from({length: N}, (_, i) => `.p${i} {
  &.a${i}:disabled { pointer-events: none; }
  &.a${i}:disabled { opacity: 50%; }
  &.a${i}[aria-disabled="true"] { pointer-events: none; }
  &.a${i}[aria-disabled="true"] { opacity: 50%; }
  &.a${i}[aria-hidden="true"] { display: none; }
}`).join('\n');

// Same, nested under one parent (one nested rule list).
const nested = `.p {${Array.from({length: N}, (_, i) => `
  &.a${i}:disabled { pointer-events: none; }
  &.a${i}:disabled { opacity: 50%; }
  &.a${i}[aria-disabled="true"] { pointer-events: none; }
  &.a${i}[aria-disabled="true"] { opacity: 50%; }
  &.a${i}[aria-hidden="true"] { display: none; }`).join('\n')}
}`;

const count = (s, re) => (s.match(re) || []).length;
// Summarize instead of printing ~30 KB of CSS; identical runs give identical summaries.
const summarize = (out) => {
  let hidden = count(out, /\[aria-hidden=true\]\{display:none\}/g);
  let merged = count(out, /\{pointer-events:none;opacity:\.5\}/g);
  let lost = [];
  for (let i = 0; i < N; i++) if (!out.includes(`.a${i}[aria-hidden=true]{display:none}`)) lost.push(i);
  return `hidden=${hidden}/${N} merged=${merged}/${N}` + (lost.length ? ` lost=[${lost.join(',')}]` : '');
};
const ok = (s) => s.startsWith(`hidden=${N}/${N} merged=${N}/${N}`) && !s.includes('lost=');

export const cases = [
  {
    name: 'no-rule-dropped-flat-x300',
    run: (l) => summarize(l.transform({filename: 'input.css', code: Buffer.from(flat), minify: true}).code.toString()),
    check: ok,
  },
  {
    name: 'no-rule-dropped-many-lists-x300',
    run: (l) => summarize(l.transform({filename: 'input.css', code: Buffer.from(many), minify: true}).code.toString()),
    check: ok,
  },
  {
    name: 'no-rule-dropped-nested-x300',
    run: (l) => summarize(l.transform({filename: 'input.css', code: Buffer.from(nested), minify: true}).code.toString()),
    check: ok,
  },
  {
    // Missed dedup from the issue body: longhands merge into rules[0] and collapse to
    // the shorthand, so the trailing identical rule isn't found under the old hash.
    name: 'dedup-after-longhand-collapse',
    css: `.a { border-top-left-radius: 16px }
.a { border-top-right-radius: 16px }
.a { border-bottom-left-radius: 16px }
.a { border-bottom-right-radius: 16px }
.x { color: red }
.a { border-radius: 16px }`,
    options: {minify: true},
    check: (out) => out.trim() === '.x{color:red}.a{border-radius:16px}',
  },
  {
    // control: plain identical rules dedup today
    name: 'control-plain-dedup',
    css: `.a{color:red}.x{color:blue}.a{color:red}`,
    options: {minify: true},
    check: (out) => out.trim() === '.x{color:#00f}.a{color:red}',
  },
];
