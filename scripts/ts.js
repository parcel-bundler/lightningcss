const { compileFromFile } = require('json-schema-to-typescript');
const fs = require('fs');

compileFromFile('selector.json', {
  additionalProperties: false
}).then(ts => fs.writeFileSync('ast.d.ts', ts))
