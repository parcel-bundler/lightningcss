const fs = require('fs');
const pkg = require('../package.json');

const dir = `${__dirname}/..`;
const triples = [
  'x86_64-apple-darwin',
  'x86_64-unknown-linux-gnu',
  'x86_64-pc-windows-msvc',
  'aarch64-apple-darwin',
  'aarch64-unknown-linux-gnu',
  'armv7-unknown-linux-gnueabihf',
  'aarch64-unknown-linux-musl',
  'x86_64-unknown-linux-musl'
];
const cpuToNodeArch = {
  x86_64: 'x64',
  aarch64: 'arm64',
  i686: 'ia32',
  armv7: 'arm',
};
const sysToNodePlatform = {
  linux: 'linux',
  freebsd: 'freebsd',
  darwin: 'darwin',
  windows: 'win32',
};

let optionalDependencies = {};
let cliOptionalDependencies = {};

try {
  fs.mkdirSync(dir + '/npm');
} catch (err) {}

for (let triple of triples) {
  let [cpu, , os, abi] = triple.split('-');
  cpu = cpuToNodeArch[cpu] || cpu;
  os = sysToNodePlatform[os] || os;

  let t = `${os}-${cpu}`;
  if (abi) {
    t += '-' + abi;
  }

  buildNode(triple, cpu, os, abi, t);
  buildCLI(triple, cpu, os, abi, t);
}

pkg.optionalDependencies = optionalDependencies;
fs.writeFileSync(`${dir}/package.json`, JSON.stringify(pkg, false, 2) + '\n');

let cliPkg = {...pkg};
cliPkg.name += '-cli';
cliPkg.bin = {
  'parcel-css': 'parcel_css'
};
delete cliPkg.main;
delete cliPkg.napi;
delete cliPkg.devDependencies;
delete cliPkg.targets;
delete cliPkg.types;
cliPkg.files = ['parcel_css', 'postinstall.js'];
cliPkg.optionalDependencies = cliOptionalDependencies;
cliPkg.scripts = {
  postinstall: 'node postinstall.js'
};

fs.writeFileSync(`${dir}/cli/package.json`, JSON.stringify(cliPkg, false, 2) + '\n');
fs.copyFileSync(`${dir}/README.md`, `${dir}/cli/README.md`);

function buildNode(triple, cpu, os, abi, t) {
  let name = `parcel-css.${t}.node`;

  let pkg2 = {...pkg};
  pkg2.name += '-' + t;
  pkg2.os = [os];
  pkg2.cpu = [cpu];
  if (abi) {
    pkg2.libc = [abi];
  }
  pkg2.main = name;
  pkg2.files = [name];
  delete pkg2.napi;
  delete pkg2.devDependencies;
  delete pkg2.dependencies;
  delete pkg2.optionalDependencies;
  delete pkg2.targets;
  delete pkg2.scripts;
  delete pkg2.types;

  optionalDependencies[pkg2.name] = pkg.version;
  
  try {
    fs.mkdirSync(dir + '/npm/node-' + t);
  } catch (err) {}
  fs.writeFileSync(`${dir}/npm/node-${t}/package.json`, JSON.stringify(pkg2, false, 2) + '\n');
  fs.copyFileSync(`${dir}/artifacts/bindings-${triple}/${name}`, `${dir}/npm/node-${t}/${name}`);
  fs.writeFileSync(`${dir}/npm/node-${t}/README.md`, `This is the ${triple} build of @parcel/css. See https://github.com/parcel-bundler/parcel-css for details.`);
}

function buildCLI(triple, cpu, os, abi, t) {
  let binary = os === 'win32' ? 'parcel_css.exe' : 'parcel_css';
  let pkg2 = {...pkg};
  pkg2.name += '-cli-' + t;
  pkg2.os = [os];
  pkg2.cpu = [cpu];
  pkg2.files = [binary];
  if (abi) {
    pkg2.libc = [abi];
  }
  delete pkg2.main;
  delete pkg2.napi;
  delete pkg2.devDependencies;
  delete pkg2.dependencies;
  delete pkg2.optionalDependencies;
  delete pkg2.targets;
  delete pkg2.scripts;
  delete pkg2.types;

  cliOptionalDependencies[pkg2.name] = pkg.version;
  
  try {
    fs.mkdirSync(dir + '/npm/cli-' + t);
  } catch (err) {}
  fs.writeFileSync(`${dir}/npm/cli-${t}/package.json`, JSON.stringify(pkg2, false, 2) + '\n');
  fs.copyFileSync(`${dir}/artifacts/bindings-${triple}/${binary}`, `${dir}/npm/cli-${t}/${binary}`);
  fs.chmodSync(`${dir}/npm/cli-${t}/${binary}`, 0o755); // Ensure execute bit is set.
  fs.writeFileSync(`${dir}/npm/cli-${t}/README.md`, `This is the ${triple} build of @parcel/css-cli. See https://github.com/parcel-bundler/parcel-css for details.`);
}
