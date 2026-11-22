// #1081: mixin/apply custom at-rules via bundle-like transform, var() in nested rule
export function run({transform, Features}) {
  const mixins = new Map();
  return transform({filename: 'a.css', minify: false, exclude: Features.Nesting,
    code: Buffer.from(`@mixin --base { color: red; &:not(:has(header)) { padding-block-start: var(--s); } }
.page { @apply --base; }`),
    customAtRules: {mixin: {prelude: '<custom-ident>', body: 'style-block'}, apply: {prelude: '<custom-ident>'}},
    visitor: {Rule: {custom: {
      mixin(rule) { mixins.set(rule.prelude.value, rule.body.value); return []; },
      apply(rule) { return mixins.has(rule.prelude.value) ? mixins.get(rule.prelude.value) : []; },
    }}}}).code.toString();
}
export const check = (out) => !out.startsWith('ERROR') && out.includes('var(--s)');
