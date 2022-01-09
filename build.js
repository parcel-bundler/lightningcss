const { spawn, execSync } = require('child_process');
const fs = require('fs');

let release = process.argv.includes('--release');
build().catch((err) => {
  console.error(err);
  process.exit(1);
});

async function build() {
  if (process.platform === 'darwin') {
    setupMacBuild();
  }

  await new Promise((resolve, reject) => {
    let args = ['build', '--platform', '--cargo-cwd', 'node'];
    if (release) {
      args.push('--release');
    }

    if (process.env.RUST_TARGET) {
      args.push('--target', process.env.RUST_TARGET);
    }

    let yarn = spawn('napi', args, {
      stdio: 'inherit',
      cwd: __dirname,
      shell: true,
    });

    yarn.on('error', reject);
    yarn.on('close', resolve);
  });

  buildFlowTypes();
}

// This forces Clang/LLVM to be used as a C compiler instead of GCC.
// This is necessary for cross-compilation for Apple Silicon in GitHub Actions.
function setupMacBuild() {
  process.env.CC = execSync('xcrun -f clang', { encoding: 'utf8' }).trim();
  process.env.CXX = execSync('xcrun -f clang++', { encoding: 'utf8' }).trim();

  let sysRoot = execSync('xcrun --sdk macosx --show-sdk-path', {
    encoding: 'utf8',
  }).trim();
  process.env.CFLAGS = `-isysroot ${sysRoot} -isystem ${sysRoot}`;
  process.env.MACOSX_DEPLOYMENT_TARGET = '10.9';
}

function buildFlowTypes() {
  let index = fs.readFileSync(__dirname + '/node/index.d.ts', 'utf8');
  index = '// @flow\n' + index;
  index = index.replace(/export interface (.*?) \{((?:.|\n)*?)\}/g, 'export type $1 = {|$2|};');
  index = index.replace(/export declare function/g, 'declare export function');

  let targets = fs.readFileSync(__dirname + '/node/targets.d.ts', 'utf8');
  targets = targets.replace(/export interface (.*?) \{((?:.|\n)*?)\}/g, 'export type $1 = {|$2|};');
  index = index.replace("import type {Targets} from './targets';", targets);

  fs.writeFileSync(__dirname + '/node/index.js.flow', index);
}
