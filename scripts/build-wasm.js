const esbuild = require('esbuild');
const exec = require('child_process').execSync;
const fs = require('fs');
const pkg = require('../package.json');

const dir = `${__dirname}/..`;

let b = fs.readFileSync(`${dir}/node/browserslistToTargets.js`, 'utf8');
b = b.replace('module.exports = browserslistToTargets;', 'export {browserslistToTargets};');
fs.writeFileSync(`${dir}/wasm/browserslistToTargets.js`, b);

let flags = fs.readFileSync(`${dir}/node/flags.js`, 'utf8');
flags = flags.replace('exports.Features =', 'export const Features =');
fs.writeFileSync(`${dir}/wasm/flags.js`, flags);

let composeVisitors = fs.readFileSync(`${dir}/node/composeVisitors.js`, 'utf8');
composeVisitors = composeVisitors.replace('module.exports = composeVisitors', 'export { composeVisitors }');
fs.writeFileSync(`${dir}/wasm/composeVisitors.js`, composeVisitors);

let dts = fs.readFileSync(`${dir}/node/index.d.ts`, 'utf8');
dts = dts.replace(/: Buffer/g, ': Uint8Array');
dts += `
/** Initializes the web assembly module. */
export default function init(input?: string | URL | Request): Promise<void>;
`;
fs.writeFileSync(`${dir}/wasm/index.d.ts`, dts);
fs.copyFileSync(`${dir}/node/targets.d.ts`, `${dir}/wasm/targets.d.ts`);
fs.copyFileSync(`${dir}/node/ast.d.ts`, `${dir}/wasm/ast.d.ts`);
fs.cpSync(`${dir}/node_modules/napi-wasm`, `${dir}/wasm/node_modules/napi-wasm`, {recursive: true});

let readme = fs.readFileSync(`${dir}/README.md`, 'utf8');
readme = readme.replace('# ⚡️ Lightning CSS', '# ⚡️ lightningcss-wasm');
fs.writeFileSync(`${dir}/wasm/README.md`, readme);

const cjsBuild = {
  entryPoints: [
    `${dir}/wasm/wasm-node.mjs`,
    `${dir}/wasm/index.mjs`,
  ],
  bundle: true,
  format: 'cjs',
  platform: 'node',
  packages: 'external',
  outdir: `${dir}/wasm`,
  outExtension: { '.js': '.cjs' },
  inject: [`${dir}/wasm/import.meta.url-polyfill.js`],
  define: { 'import.meta.url': 'import_meta_url' },
};
esbuild.build(cjsBuild).catch(console.error);

let wasmPkg = { ...pkg };
wasmPkg.name = 'lightningcss-wasm';
wasmPkg.type = 'module';
wasmPkg.main = 'index.mjs';
wasmPkg.module = 'index.mjs';
wasmPkg.exports = {
  '.': {
    types: './index.d.ts',
    node: {
      import: './wasm-node.mjs',
      require: './wasm-node.cjs'
    },
    default: {
      import: './index.mjs',
      require: './index.cjs'
    }
  },
  // Allow esbuild to import the wasm file
  // without copying it in the src directory.
  // Simplifies loading it in the browser when used in a library.
  './lightningcss_node.wasm': './lightningcss_node.wasm'
};
wasmPkg.types = 'index.d.ts';
wasmPkg.sideEffects = false;
wasmPkg.files = ['*.js', '*.cjs', '*.mjs', '*.d.ts', '*.flow', '*.wasm'];
wasmPkg.dependencies = {
  'napi-wasm': pkg.devDependencies['napi-wasm']
};
wasmPkg.bundledDependencies = ['napi-wasm']; // for stackblitz
delete wasmPkg.napi;
delete wasmPkg.devDependencies;
delete wasmPkg.optionalDependencies;
delete wasmPkg.targets;
delete wasmPkg.scripts;
fs.writeFileSync(`${dir}/wasm/package.json`, JSON.stringify(wasmPkg, false, 2) + '\n');
