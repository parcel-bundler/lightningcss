// #43: comment preservation. Current design (since 5cec8be, June 2023): `/*! ... */`
// comments at the very start of a file are kept (also per-file in bundle());
// all other comments are dropped. Preserving arbitrary comments is a separate,
// undecided feature and is NOT asserted here.
// Controls pin the existing behavior; the last two cases are start-of-file
// license comments that are still dropped (things that precede them and are
// not rules: @charset, a UTF-8 BOM).
import fs from 'fs';
import os from 'os';
import path from 'path';

const opts = {minify: true};

export const cases = [
  {name: 'control-start-of-file', css: '/*! a */\n.a { color: red }', options: opts, check: (o) => o.startsWith('/*! a */')},
  {name: 'control-multiple-at-start', css: '/*! a */\n/*! b */\n.a { color: red }', options: opts, check: (o) => o.includes('/*! a */') && o.includes('/*! b */')},
  {name: 'control-unminified', css: '/*! a */ .a { color: red }', options: {minify: false}, check: (o) => o.startsWith('/*! a */')},
  {
    name: 'control-bundle-keeps-each-files-license',
    run: (l) => {
      const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'lcss-43-'));
      fs.writeFileSync(path.join(dir, 'a.css'), '/*! main */\n@import "b.css";\n.a{color:red}\n');
      fs.writeFileSync(path.join(dir, 'b.css'), '/*! lib b */\n.b{color:red}\n');
      const out = l.bundle({filename: path.join(dir, 'a.css'), minify: true}).code.toString();
      fs.rmSync(dir, {recursive: true, force: true});
      return out;
    },
    check: (o) => o.includes('/*! main */') && o.includes('/*! lib b */'),
  },
  {
    // dart-sass expanded output starts with @charset when the file has non-ASCII chars,
    // and @charset must be first, so a license comment can only come after it.
    name: 'license-after-charset',
    css: '@charset "UTF-8";\n/*! license */\n.a { content: "é" }',
    options: opts,
    check: (o) => o.includes('/*! license */'),
  },
  {
    // dart-sass compressed output uses a BOM instead of @charset.
    name: 'license-after-bom',
    css: '﻿/*! license */\n.a { content: "é" }',
    options: opts,
    check: (o) => o.includes('/*! license */'),
  },
];
