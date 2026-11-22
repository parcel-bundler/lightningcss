// #225: merge non-adjacent rules with the same selector (or same declarations).
// Only safe when no rule in between (including rules inside @media/@supports/@layer
// blocks) declares a property in the same shorthand/longhand or logical/physical group.
// check() = the safe merge happens. Controls pin down that unsafe merges never happen.
const opts = {minify: true};
export const cases = [
  {name: 'reporter-same-selector-safe', options: opts,
    css: 'body{color:red}body{background-color:blue}h1{font-weight:normal}body{margin:0}',
    check: (out) => (out.match(/body\{/g) || []).length === 1},
  {name: 'same-body-safe-var', options: opts,
    // bulma-style Sass loop output: intervening rules only set a different custom property
    css: '.h.w{--h:0}.h.w .n{color:hsl(var(--h),50%,50%)}.h.k{--h:120}.h.k .n{color:hsl(var(--h),50%,50%)}',
    check: (out) => out.includes('.h.w .n,.h.k .n{')},
  // controls: must stay correct (red wins for an element with both classes)
  {name: 'control-maintainer-example', options: opts,
    css: '.a{color:green}.b{color:aqua}.a{color:red}',
    check: (out) => /\.b\{color:(aqua|#0ff)\}.*\.a\{color:red\}/.test(out) && !out.includes('green')},
  {name: 'control-shorthand-in-between', options: opts,
    css: '.a{margin-top:5px}.b{margin:0}.a{margin-top:5px;color:red}',
    check: (out) => out.indexOf('.b{margin:0}') < out.lastIndexOf('.a{')},
  {name: 'control-media-in-between', options: opts,
    css: '.a{color:red}@media (min-width:1px){.b{color:blue}}.a{color:green}',
    check: (out) => out.indexOf('@media') < out.indexOf('.a{color:green}')},
  {name: 'control-logical-in-between', options: opts,
    css: '.a{width:10px}.b{inline-size:20px}.a{width:10px;color:red}',
    check: (out) => out.indexOf('.b{inline-size:20px}') < out.lastIndexOf('width:10px')},
];
