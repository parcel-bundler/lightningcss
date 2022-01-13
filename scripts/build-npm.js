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

for (let triple of triples) {
  let [cpu, , os, abi] = triple.split('-');
  cpu = cpuToNodeArch[cpu] || cpu;
  os = sysToNodePlatform[os] || os;

  let t = `${os}-${cpu}`;
  if (abi) {
    t += '-' + abi;
  }

  let name = `parcel-css.${t}.node`;

  let pkg2 = {...pkg};
  pkg2.name += '-' + t;
  pkg2.os = [os];
  pkg2.cpu = [cpu];
  pkg2.main = name;
  pkg2.files = [name];
  delete pkg2.napi;
  delete pkg2.devDependencies;
  delete pkg2.dependencies;
  delete pkg2.optionalDependencies;
  delete pkg2.targets;
  delete pkg2.scripts;
  delete pkg2.types;

  optionalDependencies[pkg2.name] = '^' + pkg.version;
  
  try {
    fs.mkdirSync(dir + '/npm/' + t);
  } catch (err) {}
  fs.writeFileSync(`${dir}/npm/${t}/package.json`, JSON.stringify(pkg2, false, 2) + '\n');
  fs.copyFileSync(`${dir}/artifacts/bindings-${triple}/${name}`, `${dir}/npm/${t}/${name}`);
  fs.writeFileSync(`${dir}/npm/${t}/README.md`, `This is the ${triple} build of @parcel/css. See https://github.com/parcel-bundler/parcel-css for details.`);
}

pkg.optionalDependencies = optionalDependencies;
fs.writeFileSync(`${dir}/package.json`, JSON.stringify(pkg, false, 2) + '\n');
