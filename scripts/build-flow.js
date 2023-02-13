const fs = require('fs');
const { compiler, beautify } = require('flowgen');

let dir = `${__dirname}/../`;
let contents = fs.readFileSync(dir + '/node/index.d.ts', 'utf8').replace('`${PropertyStart}${string}`', 'string');
contents = contents.replace(/`.*`/g, 'string');
contents = contents.replace(/(string & \{\})/g, 'string');
let index = beautify(compiler.compileDefinitionString(contents, { inexact: false, interfaceRecords: true }));
index = index.replace('{ code: any }', '{| code: any |}');
index = index.replace(/from "(.*?)";/g, 'from "$1.js.flow";');
// This Exclude type isn't right at all, but idk how to get it working for real...
fs.writeFileSync(dir + '/node/index.js.flow', '// @flow\n\ntype Exclude<A, B> = A;\n' + index)

let ast = beautify(compiler.compileDefinitionFile(dir + '/node/ast.d.ts', { inexact: false }));
fs.writeFileSync(dir + '/node/ast.js.flow', '// @flow\n\n' + ast)

let targets = beautify(compiler.compileDefinitionFile(dir + '/node/targets.d.ts', { inexact: false }));
fs.writeFileSync(dir + '/node/targets.js.flow', '// @flow\n\n' + targets)
