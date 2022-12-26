const { compileFromFile } = require('json-schema-to-typescript');
const fs = require('fs');

compileFromFile('node/ast.json', {
  additionalProperties: false
}).then(ts => fs.writeFileSync('node/ast.d.ts', ts.replaceAll('For_DefaultAtRule', '')));
