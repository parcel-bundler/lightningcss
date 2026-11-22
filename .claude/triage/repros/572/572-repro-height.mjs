export const css = `.h{height:60px;height:var(--h)}`;
export const options = {minify: true};
export const check = o => o.indexOf('60px') < o.indexOf('var(');
