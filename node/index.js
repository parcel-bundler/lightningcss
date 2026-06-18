let parts = [process.platform, process.arch];
if (process.platform === 'linux') {
  const { MUSL, familySync } = require('detect-libc');
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

let native;
let platformKey = parts.join('-');
let packageName = `lightningcss-${platformKey}`;
let localName = `../lightningcss.${platformKey}.node`;
try {
  native = require(packageName);
} catch (packageError) {
  try {
    native = require(localName);
  } catch (localError) {
    let error = new Error(
      `Unable to load the Lightning CSS native binding for ${platformKey}. ` +
      `Tried ${packageName} and ${localName}. ` +
      `This usually means the optional dependency for your platform was not installed. ` +
      `Try reinstalling with optional dependencies enabled, or install ${packageName} directly.\n\n` +
      `Original errors:\n` +
      `- ${packageName}: ${packageError.message}\n` +
      `- ${localName}: ${localError.message}`
    );
    error.code = localError.code || packageError.code;
    error.errors = [packageError, localError];
    error.cause = packageError;
    throw error;
  }
}

module.exports.transform = wrap(native.transform);
module.exports.transformStyleAttribute = wrap(native.transformStyleAttribute);
module.exports.bundle = wrap(native.bundle);
module.exports.bundleAsync = wrap(native.bundleAsync);
module.exports.browserslistToTargets = require('./browserslistToTargets');
module.exports.composeVisitors = require('./composeVisitors');
module.exports.Features = require('./flags').Features;

function wrap(call) {
  return (options) => {
    if (typeof options.visitor === 'function') {
      let deps = [];
      options.visitor = options.visitor({
        addDependency(dep) {
          deps.push(dep);
        }
      });

      let result = call(options);
      if (result instanceof Promise) {
        result = result.then(res => {
          if (deps.length) {
            res.dependencies ??= [];
            res.dependencies.push(...deps);
          }
          return res;
        });
      } else if (deps.length) {
        result.dependencies ??= [];
        result.dependencies.push(...deps);
      }
      return result;
    } else {
      return call(options);
    }
  };
}
