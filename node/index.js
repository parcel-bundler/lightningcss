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

let bindings;

if (process.env.CSS_TRANSFORMER_WASM) {
  bindings = require(`../pkg`);
} else {
  try {
    bindings = require(`@parcel/css-${parts.join('-')}`);
  } catch (err) {
    bindings = require(`../parcel-css.${parts.join('-')}.node`);
  }
}

// Workaround for https://github.com/parcel-bundler/parcel-css/issues/150
module.exports = {
  transform(opts) {
    let result = bindings.transform(opts);
    if (result.code.length === 0) {
      result.code = Buffer.alloc(0);
    }
    return result;
  },
  transformStyleAttribute(opts) {
    let result = bindings.transformStyleAttribute(opts);
    if (result.code.length === 0) {
      result.code = Buffer.alloc(0);
    }
    return result;
  },
  bundle(opts) {
    let result = bindings.bundle(opts);
    if (result.code.length === 0) {
      result.code = Buffer.alloc(0);
    }
    return result;
  },
};

module.exports.browserslistToTargets = require('./browserslistToTargets');
