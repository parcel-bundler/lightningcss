// Current handling of @function / dashed-function calls (feature request, not a bug).
export const css = `@function --double(--x <length>) returns <length> {
  result: calc(var(--x) * 2);
}
@mixin --m(--c) { color: var(--c); }
.box { width: --double(10px); @apply --m(red); }`;
export const options = {minify: true};
// "correct" for the request = function call resolved or preserved; we check it's at least passed through unchanged.
export const check = (out) => out.includes('--double(10px)') && out.includes('@function');
