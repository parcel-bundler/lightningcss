let fs = require('fs');
let path = require('path');

let parts = [process.platform, process.arch];
if (process.platform === 'linux') {
  const {MUSL, family} = require('detect-libc');
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

let pkgPath;
try {
  pkgPath = path.dirname(require.resolve(`@parcel/css-cli-${parts.join('-')}/package.json`));
} catch (err) {
  pkgPath = path.join(__dirname, '..', 'target', 'release');
  if (!fs.existsSync(pkgPath)) {
    pkgPath = path.join(__dirname, '..', 'target', 'debug');
  }
}

try {
  fs.linkSync(path.join(pkgPath, 'parcel_css'), path.join(__dirname, 'parcel_css'));
} catch (err) {
  try {
    fs.copyFileSync(path.join(pkgPath, 'parcel_css'), path.join(__dirname, 'parcel_css'));
  } catch (err) {
    console.error('Failed to move @parcel/css-cli binary into place.');
    process.exit(1);
  }
}
