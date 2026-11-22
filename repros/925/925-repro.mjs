export const css = `pre { user-select: all; }`;
export const options = {minify: true, targets: {safari: (18 << 16) | (2 << 8), ios_saf: (18 << 16) | (2 << 8), chrome: 132 << 16}};
export const check = (out) => out.includes('-webkit-user-select');
