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

let dts = fs.readFileSync(`${dir}/node/index.d.ts`, 'utf8');
dts = dts.replace(/: Buffer/g, ': Uint8Array');
dts += `
/** Initializes the web assembly module. */
export default function init(input?: string | URL | Request): Promise<void>;
`;
fs.writeFileSync(`${dir}/wasm/index.d.ts`, dts);
fs.copyFileSync(`${dir}/node/targets.d.ts`, `${dir}/wasm/targets.d.ts`);
fs.copyFileSync(`${dir}/node/ast.d.ts`, `${dir}/wasm/ast.d.ts`);

let readme = fs.readFileSync(`${dir}/README.md`, 'utf8');
readme = readme.replace('# ⚡️ Lightning CSS', '# ⚡️ lightningcss-wasm');
fs.writeFileSync(`${dir}/wasm/README.md`, readme);

let wasmPkg = { ...pkg };
wasmPkg.name = 'lightningcss-wasm';
wasmPkg.type = 'module';
wasmPkg.main = 'index.mjs';
wasmPkg.module = 'index.mjs';
wasmPkg.types = 'index.d.ts';
wasmPkg.sideEffects = false;
wasmPkg.files = ['*.js', '*.mjs', '*.d.ts', '*.flow', '*.wasm'];
wasmPkg.dependencies = {
  'napi-wasm': pkg.devDependencies['napi-wasm']
};
delete wasmPkg.exports;
delete wasmPkg.napi;
delete wasmPkg.devDependencies;
delete wasmPkg.optionalDependencies;
delete wasmPkg.targets;
delete wasmPkg.scripts;
fs.writeFileSync(`${dir}/wasm/package.json`, JSON.stringify(wasmPkg, false, 2) + '\n');
