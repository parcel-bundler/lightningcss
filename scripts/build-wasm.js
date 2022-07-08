const exec = require('child_process').execSync;
const fs = require('fs');
const pkg = require('../package.json');

const dir = `${__dirname}/..`;

try {
  fs.mkdirSync(dir + '/npm');
} catch (err) {}

exec(`cp -R ${dir}/artifacts/wasm ${dir}/npm/.`);
fs.writeFileSync(`${dir}/npm/wasm/index.js`, `export {default, transform, transformStyleAttribute} from './parcel_css_node.js';\nexport {browserslistToTargets} from './browserslistToTargets.js'`);

let b = fs.readFileSync(`${dir}/node/browserslistToTargets.js`, 'utf8');
b = b.replace('module.exports = browserslistToTargets;', 'export {browserslistToTargets};');
fs.writeFileSync(`${dir}/npm/wasm/browserslistToTargets.js`, b);
fs.unlinkSync(`${dir}/npm/wasm/parcel_css_node.d.ts`);

let dts = fs.readFileSync(`${dir}/node/index.d.ts`, 'utf8');
dts = dts.replace(/: Buffer/g, ': Uint8Array');
dts += `
/** Initializes the web assembly module. */
export default function init(): Promise<void>;
`;
fs.writeFileSync(`${dir}/npm/wasm/index.d.ts`, dts);
fs.copyFileSync(`${dir}/node/targets.d.ts`, `${dir}/npm/wasm/targets.d.ts`);

let readme = fs.readFileSync(`${dir}/README.md`, 'utf8');
readme = readme.replace('# @parcel/css', '# @parcel/css-wasm');
fs.writeFileSync(`${dir}/npm/wasm/README.md`, readme);

fs.unlinkSync(`${dir}/npm/wasm/.gitignore`);

let wasmPkg = {...pkg};
wasmPkg.name = '@parcel/css-wasm';
wasmPkg.type = 'module';
wasmPkg.main = 'index.js';
wasmPkg.module = 'index.js';
wasmPkg.types = 'index.d.ts';
wasmPkg.sideEffects = false;
delete wasmPkg.exports;
delete wasmPkg.files;
delete wasmPkg.napi;
delete wasmPkg.devDependencies;
delete wasmPkg.dependencies;
delete wasmPkg.optionalDependencies;
delete wasmPkg.targets;
delete wasmPkg.scripts;
fs.writeFileSync(`${dir}/npm/wasm/package.json`, JSON.stringify(wasmPkg, false, 2) + '\n');
