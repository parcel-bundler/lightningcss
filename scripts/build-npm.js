const fs = require('fs');
const pkg = require('../package.json');

const dir = `${__dirname}/..`;

// Add `libc` fields only to platforms that have libc(Standard C library).
const triples = [
  {
    name: 'x86_64-apple-darwin',
  },
  {
    name: 'x86_64-unknown-linux-gnu',
    libc: 'glibc',
  },
  {
    name: 'x86_64-pc-windows-msvc',
  },
  {
    name: 'aarch64-pc-windows-msvc'
  },
  {
    name: 'aarch64-apple-darwin',
  },
  {
    name: 'aarch64-unknown-linux-gnu',
    libc: 'glibc',
  },
  {
    name: 'armv7-unknown-linux-gnueabihf',
  },
  {
    name: 'aarch64-unknown-linux-musl',
    libc: 'musl',
  },
  {
    name: 'x86_64-unknown-linux-musl',
    libc: 'musl',
  },
  {
    name: 'x86_64-unknown-freebsd'
  },
  {
    name: 'aarch64-linux-android'
  }
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
  android: 'android'
};

let optionalDependencies = {};
let cliOptionalDependencies = {};

try {
  fs.mkdirSync(dir + '/npm');
} catch (err) { }

for (let triple of triples) {
  // Add the libc field to package.json to avoid downloading both
  // `gnu` and `musl` packages in Linux.
  const libc = triple.libc;
  let [cpu, , os, abi] = triple.name.split('-');
  cpu = cpuToNodeArch[cpu] || cpu;
  os = sysToNodePlatform[os] || os;

  let t = `${os}-${cpu}`;
  if (abi) {
    t += '-' + abi;
  }

  buildNode(triple.name, cpu, os, libc, t);
  buildCLI(triple.name, cpu, os, libc, t);
}

pkg.optionalDependencies = optionalDependencies;
fs.writeFileSync(`${dir}/package.json`, JSON.stringify(pkg, false, 2) + '\n');

let cliPkg = { ...pkg };
cliPkg.name += '-cli';
cliPkg.bin = {
  'lightningcss': 'lightningcss'
};
delete cliPkg.main;
delete cliPkg.napi;
delete cliPkg.exports;
delete cliPkg.devDependencies;
delete cliPkg.targets;
delete cliPkg.types;
cliPkg.files = ['lightningcss', 'postinstall.js'];
cliPkg.optionalDependencies = cliOptionalDependencies;
cliPkg.scripts = {
  postinstall: 'node postinstall.js'
};

fs.writeFileSync(`${dir}/cli/package.json`, JSON.stringify(cliPkg, false, 2) + '\n');
fs.copyFileSync(`${dir}/README.md`, `${dir}/cli/README.md`);
fs.copyFileSync(`${dir}/LICENSE`, `${dir}/cli/LICENSE`);

function buildNode(triple, cpu, os, libc, t) {
  let name = `lightningcss.${t}.node`;

  let pkg2 = { ...pkg };
  pkg2.name += '-' + t;
  pkg2.os = [os];
  pkg2.cpu = [cpu];
  if (libc) {
    pkg2.libc = [libc];
  }
  pkg2.main = name;
  pkg2.files = [name];
  delete pkg2.exports;
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
  } catch (err) { }
  fs.writeFileSync(`${dir}/npm/node-${t}/package.json`, JSON.stringify(pkg2, false, 2) + '\n');
  fs.copyFileSync(`${dir}/artifacts/bindings-${triple}/${name}`, `${dir}/npm/node-${t}/${name}`);
  fs.writeFileSync(`${dir}/npm/node-${t}/README.md`, `This is the ${triple} build of lightningcss. See https://github.com/parcel-bundler/lightningcss for details.`);
  fs.copyFileSync(`${dir}/LICENSE`, `${dir}/npm/node-${t}/LICENSE`);
}

function buildCLI(triple, cpu, os, libc, t) {
  let binary = os === 'win32' ? 'lightningcss.exe' : 'lightningcss';
  let pkg2 = { ...pkg };
  pkg2.name += '-cli-' + t;
  pkg2.os = [os];
  pkg2.cpu = [cpu];
  pkg2.files = [binary];
  if (libc) {
    pkg2.libc = [libc];
  }
  delete pkg2.main;
  delete pkg2.exports;
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
  } catch (err) { }
  fs.writeFileSync(`${dir}/npm/cli-${t}/package.json`, JSON.stringify(pkg2, false, 2) + '\n');
  fs.copyFileSync(`${dir}/artifacts/bindings-${triple}/${binary}`, `${dir}/npm/cli-${t}/${binary}`);
  fs.chmodSync(`${dir}/npm/cli-${t}/${binary}`, 0o755); // Ensure execute bit is set.
  fs.writeFileSync(`${dir}/npm/cli-${t}/README.md`, `This is the ${triple} build of lightningcss-cli. See https://github.com/parcel-bundler/lightningcss for details.`);
  fs.copyFileSync(`${dir}/LICENSE`, `${dir}/npm/cli-${t}/LICENSE`);
}
