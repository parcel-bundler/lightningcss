const { compileFromFile } = require('json-schema-to-typescript');
const fs = require('fs');
const recast = require('recast');
const traverse = require('@babel/traverse').default;
const {parse} = require('@babel/parser');
const t = require('@babel/types');

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
  ts = ts.replaceAll('For_DefaultAtRule', '');

  // Use recast/babel to make some types generic so we can replace them in index.d.ts.
  let ast = recast.parse(ts, {
    parser: {
      parse() {
        return parse(ts, {
          sourceType: 'module',
          plugins: ['typescript'],
          tokens: true
        });
      }
    }
  });

  traverse(ast, {
    Program(path) {
      process(path.scope.getBinding('Declaration'));
      process(path.scope.getBinding('MediaQuery'));
    }
  });

  ts = recast.print(ast, {objectCurlySpacing: false}).code;
  fs.writeFileSync('node/ast.d.ts', ts)
});

function process(binding) {
  // Follow the references upward from the binding to add generics.
  for (let reference of binding.referencePaths) {
    if (reference.node !== binding.identifier) {
      genericize(reference, binding.identifier.name);
    }
  }
}

function genericize(path, name, seen = new Set()) {
  if (seen.has(path.node)) return;
  seen.add(path.node);

  // Find the parent declaration of the reference, and add a generic if needed.
  let parent = path.findParent(p => p.isDeclaration());
  if (!parent.node.typeParameters) {
    parent.node.typeParameters = t.tsTypeParameterDeclaration([]);
  }
  let params = parent.get('typeParameters');
  let param = params.node.params.find(p => p.default.typeName.name === name);
  if (!param) {
    params.pushContainer('params', t.tsTypeParameter(null, t.tsTypeReference(t.identifier(name)), name[0]));
  }

  // Replace the reference with the generic, or add a type parameter.
  if (path.node.name === name) {
    path.replaceWith(t.identifier(name[0]));
  } else {
    if (!path.parent.typeParameters) {
      path.parent.typeParameters = t.tsTypeParameterInstantiation([]);
    }
    let param = path.parent.typeParameters.params.find(p => p.typeName.name === name[0]);
    if (!param) {
      path.parentPath.get('typeParameters').pushContainer('params', t.tsTypeReference(t.identifier(name[0])));
    }
  }

  // Keep going to all references of this reference.
  let binding = path.scope.getBinding(parent.node.id.name);
  for (let reference of binding.referencePaths) {
    if (reference.node !== binding.identifier) {
      genericize(reference, name, seen);
    }
  }
}
