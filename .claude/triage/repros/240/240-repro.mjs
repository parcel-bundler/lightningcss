// #240: remove exact duplicate selectors from a selector list (minify).
// Decision: wanted (safe: identical selectors have identical matching and specificity).
// Only *exact* duplicates may be removed; a subsumed selector (div.c vs .c) has
// higher specificity and must stay (browser-verified during triage).
const opts = {minify: true};
export const cases = [
  {name: 'reporter-exact-duplicates', css: 'h1,h2,h3,h1,h3{color:blue}', options: opts,
    check: (out) => out === 'h1,h2,h3{color:#00f}'},
  {name: 'duplicate-created-by-adjacent-merge', css: '.a,.b{color:red}.b{color:red}', options: opts,
    check: (out) => out === '.a,.b{color:red}'},
  {name: 'duplicate-created-by-merge-reordered', css: '.a{color:red}.b,.a{color:red}', options: opts,
    check: (out) => out === '.a,.b{color:red}' || out === '.b,.a{color:red}'},
  // control: div.class has higher specificity than .class; it must be kept
  {name: 'control-subsumed-selector-kept', css: 'div.class,div.class,.class{color:blue}', options: opts,
    check: (out) => out.includes('div.class') && out.includes(',.class')},
  // control: different pseudo-elements are not duplicates
  {name: 'control-distinct-kept', css: '.a::before,.a::after{content:""}', options: opts,
    check: (out) => out.includes('.a:before') && out.includes('.a:after')},
];
