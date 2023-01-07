const { compileFromFile } = require('json-schema-to-typescript');
const fs = require('fs');

const skip = {
  FillRule: true,
  ImportRule: true,
  FontFaceRule: true,
  FontPaletteValuesRule: true,
  NamespaceRule: true,
  CustomMediaRule: true,
  LayerStatementRule: true,
  PropertyRule: true,
  UnknownAtRule: true,
  DefaultAtRule: true
}

compileFromFile('node/ast.json', {
  additionalProperties: false
}).then(ts => {
  ts = ts.replaceAll('For_DefaultAtRule', '')
    .replaceAll('interface DeclarationBlock', 'interface DeclarationBlock<D = Declaration>')
    .replaceAll('DeclarationBlock;', 'DeclarationBlock<D>;')
    .replaceAll('Declaration[]', 'D[]')
    .replace(/(interface )?([A-Z][a-zA-Z]*Rule)/g, (m, x, n) => skip[n] ? m : `${m}<D${x ? ' = Declaration' : ''}>`)
    .replaceAll(': Rule', ': Rule<D>')
    .replaceAll('type Rule =', 'type Rule<D = Declaration> =')
    .replace(/Keyframe(?![a-zA-Z])/g, 'Keyframe<D>')
    .replaceAll('StyleSheet', 'StyleSheet<D = Declaration>');
  fs.writeFileSync('node/ast.d.ts', ts)
});
