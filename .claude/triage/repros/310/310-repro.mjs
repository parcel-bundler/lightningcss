// #310: escapes like content:"\f67f" are decoded and written as raw UTF-8, and @charset is dropped.
// Per CSS Syntax "determine the fallback encoding", a stylesheet served without a charset
// parameter and without @charset is decoded with the referring document's encoding, so the
// output can mean something else than the input (Chromium 153, windows-1252 page:
// input "\2713" => U+2713, output => "âœ“").
// check(): true when the output decodes the same under any ASCII-compatible encoding, i.e. it is
// pure ASCII, or it starts with @charset "UTF-8"; (either fix would satisfy it).
const charsetSafe = (out) => !out.startsWith('ERROR') && (/^[\x00-\x7f]*$/.test(out) || out.startsWith('@charset "UTF-8";'));
const asciiOnly = (out) => /^[\x00-\x7f]*$/.test(out);
export const cases = [
  {name: 'pua-escape-minify', css: `.bi-123::before{content:"\\f67f"}`, options: {minify: true}, check: charsetSafe},
  {name: 'pua-escape-no-minify', css: `.icon-home::before{content:"\\e600"}`, options: {minify: false}, check: charsetSafe},
  {name: 'alt-text-escape', css: `.t::before{content:"\\2713" / ""}`, options: {minify: true}, check: charsetSafe},
  {name: 'raw-utf8-with-charset-rule', css: `@charset "UTF-8";\n.a::before{content:"✓"}`, options: {minify: true}, check: charsetSafe},
  {name: 'ident-escape', css: `.caf\\e9{color:red}`, options: {minify: true}, check: charsetSafe},
  {name: 'font-family-escape', css: `.a{font-family:"caf\\e9"}`, options: {minify: true}, check: charsetSafe},
  // Controls: ASCII escapes are fine to decode, and ASCII-only input must not gain an @charset rule.
  {name: 'control-ascii-escape', css: `.foo::before{content:"\\31"}`, options: {minify: true}, check: (o) => o === '.foo:before{content:"1"}'},
  {name: 'control-ascii-stays-ascii', css: `.a{color:red}.b::before{content:"x"}`, options: {minify: true}, check: (o) => asciiOnly(o) && !o.includes('@charset')},
];
