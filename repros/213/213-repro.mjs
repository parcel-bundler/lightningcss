export const css = `@custom-media --supports-hover screen and (pointer: fine);
@media not (--supports-hover) { .a { color: red } }`;
export const options = {drafts: {customMedia: true}, targets: {chrome: 100 << 16}};
export const check = (out) => /@media not screen and \(pointer: fine\)/.test(out);
