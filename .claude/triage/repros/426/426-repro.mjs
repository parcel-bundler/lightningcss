// #426 (dup #1099): translate/rotate/scale merged into `transform` (wrong composition order, breaks
// independent cascade/transitions) or dropped when `transform` follows them. Fixed on master by #1340
// (40fb4ea, after v1.33.0). check() = each individual property survives as its own declaration.
const keep = (...props) => (o) => props.every((p) => new RegExp(`(^|[;{\\s])${p}:`).test(o));
export const cases = [
  {name: 'individual before transform (dropped)', css: `.a{translate:-50% -50%;rotate:-45deg;transform:scale(98%) scaleX(-1)}`,
    options: {minify: true}, check: keep('translate', 'rotate', 'transform')},
  {name: 'individual after transform (merged, wrong order)', css: `.box{transform:translateY(-30px);scale:1.4}`,
    options: {minify: true}, check: (o) => keep('scale', 'transform')(o) && !/translateY\(-30px\)\s*scale/.test(o)},
  {name: 'reporter: transition on scale', css: `.bar{transition:scale .3s linear;transform:skew(10deg);scale:1.5;translate:1em}.bar:hover{scale:2}`,
    options: {minify: true, targets: {chrome: 95 << 16}}, check: keep('transform', 'scale', 'translate')},
  {name: 'dialog centering (Next.js report)', css: `.a{translate:-50% -50%;transform:translateY(.75rem)}`,
    options: {minify: false}, check: keep('translate', 'transform')},
  {name: 'none vs identity', css: `.box{transform:none;translate:none}`,
    options: {minify: true}, check: (o) => /translate:none/.test(o) && !/translate\(0/.test(o)},
  // Controls: duplicate individual properties still collapse; standalone ones unchanged.
  {name: 'control: duplicate scale collapses', css: `.a{scale:1.5;scale:2}`, options: {minify: true}, check: (o) => o === '.a{scale:2}'},
  {name: 'control: transform alone', css: `.a{transform:translate(10px) rotate(5deg)}`, options: {minify: true}, check: (o) => /transform:translate\(10px\)\s*rotate\(5deg\)/.test(o)},
];
