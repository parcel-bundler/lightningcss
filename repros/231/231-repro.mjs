// #231: request for an option to keep source order of declarations (the reporter ships
// unminified bundled CSS). These cases record what non-minified transform() does today.
// Cases marked "order" check that source order is preserved (the desired behavior if the
// feature is accepted); controls check cascade-equivalent outputs that must stay valid.
const idx = (out, ...needles) => needles.map((n) => out.indexOf(n));
const inOrder = (out, ...needles) => {
  const p = idx(out, ...needles);
  return p.every((x) => x >= 0) && p.every((x, i) => i === 0 || p[i - 1] < x);
};
export const cases = [
  {
    // reporter's complaint: custom properties move relative to normal declarations
    name: 'order-custom-props-interleaved',
    css: `.a {\n  color: red;\n  --x: 1px;\n  margin: 0;\n  --y: 2px;\n  padding: 0;\n}`,
    options: {minify: false},
    check: (out) => inOrder(out, 'color', '--x', 'margin', '--y', 'padding'),
  },
  {
    // comment by chriscoyier: !important declarations are emitted after normal ones
    name: 'order-important-moved-last',
    css: `hr {\n  border: 0 !important;\n  border-bottom: 1px solid red;\n}`,
    options: {minify: false},
    check: (out) => inOrder(out, 'border: 0', 'border-bottom'),
  },
  {
    name: 'order-unrelated-props',
    css: `.a {\n  display: flex;\n  color: red;\n  width: 10px;\n  background: blue;\n  margin-top: 1px;\n}`,
    options: {minify: false},
    check: (out) => inOrder(out, 'display', 'color', 'width', 'background', 'margin-top'),
  },
  // controls: semantics must be preserved regardless of order
  {
    name: 'control-important-still-wins',
    css: `hr {\n  border: 0 !important;\n  border-bottom: 1px solid red;\n}`,
    options: {minify: false},
    check: (out) => /border: 0 ?!important/.test(out) || /border-bottom: 0 ?!important/.test(out),
  },
  {
    name: 'control-rule-order-kept',
    css: `.b { color: red }\n.a { color: blue }\n.c { color: green }`,
    options: {minify: false},
    check: (out) => inOrder(out, '.b', '.a', '.c'),
  },
];
