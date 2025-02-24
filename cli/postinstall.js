let fs = require('fs');
let path = require('path');

let parts = [process.platform, process.arch];
if (process.platform === 'linux') {
  const {MUSL, familySync} = require('detect-libc');
  const family = familySync();
  if (family === MUSL) {
    parts.push('musl');
  } else if (process.arch === 'arm') {
    parts.push('gnueabihf');
  } else {
    parts.push('gnu');
  }
} else if (process.platform === 'win32') {
  parts.push('msvc');
}

let binary = process.platform === 'win32' ? 'lightningcss.exe' : 'lightningcss';

let pkgPath;
try {
  pkgPath = path.dirname(require.resolve(`lightningcss-cli-${parts.join('-')}/package.json`));
} catch (err) {
  pkgPath = path.join(__dirname, '..', 'target', 'release');
  if (!fs.existsSync(path.join(pkgPath, binary))) {
    pkgPath = path.join(__dirname, '..', 'target', 'debug');
  }
}

try {
  fs.linkSync(path.join(pkgPath, binary), path.join(__dirname, binary));
} catch (err) {
  try {
    fs.copyFileSync(path.join(pkgPath, binary), path.join(__dirname, binary));
  } catch (err) {
    console.error('Failed to move lightningcss-cli binary into place.');
    process.exit(1);
  }
}

if (process.platform === 'win32') {
  try {
    fs.unlinkSync(path.join(__dirname, 'lightningcss'));
  } catch (err) { }
}
