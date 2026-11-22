// #271: factor declarations shared by adjacent rules into a combined rule.
// Safe only when no shared declaration overlaps (shorthand/longhand, logical/physical
// group, `all`) a non-shared declaration in either rule; worth it only when the
// shared text is longer than the extra selectors. check() = safe factoring happens.
const opts = {minify: true};
export const cases = [
  {name: 'reporter-example', options: opts,
    css: '.foo{background-color:red;padding:20px}.bar{background-color:red;padding:40px}',
    check: (out) => out.length < '.foo{background-color:red;padding:20px}.bar{background-color:red;padding:40px}'.length},
  {name: 'custom-properties-theme', options: opts,
    // PR #1333 scope: shared custom properties in adjacent theme rules
    css: '.t1{--a:#123456;--b:1px solid red;--c:calc(1rem + 2px);--d:x}.t2{--a:#123456;--b:1px solid red;--c:calc(1rem + 2px);--d:y}',
    check: (out) => (out.match(/--a:/g) || []).length === 1},
  // controls: unsafe factorings must not happen (browser-verified as different).
  // (today the in-rule shorthand/longhand overlap is already collapsed by minify, so this
  // one mainly guards ordering; the logical/physical one survives minification.)
  {name: 'control-overlapping-longhand', options: opts,
    css: '.foo{margin:0;padding:1px}.bar{margin-top:5px;margin:0}',
    check: (out) => !out.includes('.foo,.bar{margin:0}') && !out.includes('.bar,.foo{margin:0}')},
  {name: 'control-logical-physical', options: opts,
    css: '.foo{width:10px;color:red}.bar{inline-size:20px;width:10px}',
    check: (out) => !/\.foo,\.bar\{width:10px\}|\.bar,\.foo\{width:10px\}/.test(out)},
  {name: 'control-longer-selectors-not-factored', options: opts,
    css: '.somelongselectorname{background-color:red;padding:20px}.someotherlongselectorname{background-color:red;padding:40px}',
    check: (out) => out === '.somelongselectorname{background-color:red;padding:20px}.someotherlongselectorname{background-color:red;padding:40px}'},
];
