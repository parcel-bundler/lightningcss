let parts = [process.platform, process.arch];
if (process.platform === 'linux') {
  const {MUSL, family} = await import('detect-libc');
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

let default_export;
if (process.env.CSS_TRANSFORMER_WASM) {
  default_export = await import('./pkg/parcel_css_node.js');
} else {
  const {createRequire} = await import('module');
  const require = createRequire(import.meta.url);

  try {
    default_export = require(`@parcel/css-${parts.join('-')}`);
  } catch (err) {
    default_export = require(`../parcel-css.${parts.join('-')}.node`);
  }
}

export default default_export;

import browserslistToTargetsImport from './browserslistToTargets.js';
export const browserslistToTargets = browserslistToTargetsImport;
