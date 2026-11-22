// #1081 comment: @theme declaration-list -> :root style rule
export function run({transform}) {
  return transform({filename: 'a.css', code: Buffer.from('@theme { --blue: blue; --primary: var(--blue); }'),
    customAtRules: {theme: {prelude: null, body: 'declaration-list'}},
    visitor: {Rule: {custom: {theme(rule) {
      return {type: 'style', value: {selectors: [[{type: 'pseudo-class', kind: 'root'}]], loc: rule.loc, declarations: rule.body.value}};
    }}}}}).code.toString();
}
export const check = (out) => out.includes('--primary: var(--blue)');
