const prefixes = require('autoprefixer/data/prefixes');
const browsers = require('caniuse-lite').agents;
const unpack = require('caniuse-lite').feature;
const features = require('caniuse-lite').features;
const mdn = require('@mdn/browser-compat-data');
const fs = require('fs');

const BROWSER_MAPPING = {
  and_chr: 'chrome',
  and_ff: 'firefox',
  ie_mob: 'ie',
  op_mob: 'opera',
  and_qq: null,
  and_uc: null,
  baidu: null,
  bb: null,
  kaios: null,
  op_mini: null,
};

const MDN_BROWSER_MAPPING = {
  chrome_android: 'chrome',
  firefox_android: 'firefox',
  opera_android: 'opera',
  safari_ios: 'ios_saf',
  samsunginternet_android: 'samsung',
  webview_android: 'android'
};

// Fix data, autoprefixer seems wrong.
// See https://developer.mozilla.org/en-US/docs/Web/CSS/::file-selector-button
prefixes['::file-selector-button'].browsers = [
  'chrome 1',
  'chrome 88',
  'edge 12',
  'edge 88',
  'safari 3',
  'safari 14',
  'ie 10',
  'ie 11',
  'ios_saf 1',
  'ios_saf 14',
  'opera 15',
  'opera 74',
  'samsung 1',
  'samsung 14'
];

let flexSpec = {};
let oldGradient = {};
let p = new Map();
for (let prop in prefixes) {
  let browserMap = {};
  for (let b of prefixes[prop].browsers) {
    let [name, version, variant] = b.split(' ');
    if (BROWSER_MAPPING[name] === null) {
      continue;
    }
    let prefix = browsers[name].prefix_exceptions?.[version] || browsers[name].prefix;
    name = BROWSER_MAPPING[name] || name;
    let v = parseVersion(version);
    if (v == null) {
      console.log('BAD VERSION', prop, name, version);
      continue;
    }
    if (browserMap[name]?.[prefix] == null) {
      browserMap[name] = browserMap[name] || {};
      browserMap[name][prefix] = [v, v];
    } else {
      if (v < browserMap[name][prefix][0]) {
        browserMap[name][prefix][0] = v;
      }

      if (v > browserMap[name][prefix][1]) {
        browserMap[name][prefix][1] = v;
      }
    }

    if (variant === '2009') {
      if (flexSpec[name] == null) {
        flexSpec[name] = [v, v];
      } else {
        if (v < flexSpec[name][0]) {
          flexSpec[name][0] = v;
        }

        if (v > flexSpec[name][1]) {
          flexSpec[name][1] = v;
        }
      }
    } else if (variant === 'old' && prop.includes('gradient')) {
      if (oldGradient[name] == null) {
        oldGradient[name] = [v, v];
      } else {
        if (v < oldGradient[name][0]) {
          oldGradient[name][0] = v;
        }

        if (v > oldGradient[name][1]) {
          oldGradient[name][1] = v;
        }
      }
    }
  }
  // p[prop] = browserMap;
  let s = JSON.stringify(browserMap);
  let found = false;
  for (let [key, val] of p) {
    if (JSON.stringify(val) === s) {
      key.push(prop);
      found = true;
      break;
    }
  }
  if (!found) {
    p.set([prop], browserMap);
  }
}

let cssFeatures = [
  'css-sel2',
  'css-sel3',
  'css-gencontent',
  'css-first-letter',
  'css-first-line',
  'css-in-out-of-range',
  'form-validation',
  'css-any-link',
  'css-default-pseudo',
  'css-dir-pseudo',
  'css-focus-within',
  'css-focus-visible',
  'css-indeterminate-pseudo',
  'css-matches-pseudo',
  'css-optional-pseudo',
  'css-placeholder-shown',
  'dialog',
  'fullscreen',
  'css-marker-pseudo',
  'css-placeholder',
  'css-selection',
  'css-case-insensitive',
  'css-read-only-write',
  'css-autofill',
  'css-namespaces',
  'shadowdomv1',
  'css-rrggbbaa'
];

let compat = {};
for (let feature of cssFeatures) {
  let data = unpack(features[feature]);
  let browserMap = {};
  for (let name in data.stats) {
    if (BROWSER_MAPPING[name] === null) {
      continue;
    }

    name = BROWSER_MAPPING[name] || name;
    for (let version in data.stats[name]) {
      if (data.stats[name][version] === 'y') {
        let v = parseVersion(version);
        if (v == null) {
          console.log('BAD VERSION', feature, name, version);
          continue;
        }

        if (browserMap[name] == null || v < browserMap[name]) {
          browserMap[name] = v;
        }
      }
    }
  }

  compat[feature] = browserMap;
}

let mdnFeatures = {
  doublePositionGradients: mdn.css.types.image.gradient['radial-gradient'].doubleposition.__compat.support
};

for (let feature in mdnFeatures) {
  let browserMap = {};
  for (let name in mdnFeatures[feature]) {
    if (MDN_BROWSER_MAPPING[name] === null) {
      continue;
    }
  
    let version = mdnFeatures[feature][name].version_added;
    if (!version) {
      continue;
    }
    
    let v = parseVersion(version);
    if (v == null) {
      console.log('BAD VERSION', feature, name, version);
      continue;
    }

    name = MDN_BROWSER_MAPPING[name] || name;
    browserMap[name] = v;
  }

  compat[feature] = browserMap;
}

let prefixMapping = {
  webkit: 'WebKit',
  moz: 'Moz',
  ms: 'Ms',
  o: 'O'
};

let enumify = (f) => f.replace(/^@([a-z])/, (_, x) => 'At' + x.toUpperCase()).replace(/^::([a-z])/, (_, x) => 'PseudoElement' + x.toUpperCase()).replace(/^:([a-z])/, (_, x) => 'PseudoClass' + x.toUpperCase()).replace(/(^|-)([a-z])/g, (_, a, x) => x.toUpperCase())

let s = `// This file is autogenerated by build-prefixes.js. DO NOT EDIT!

use serde::{Deserialize, Serialize};
use super::VendorPrefix;

#[derive(Serialize, Debug, Deserialize, Clone, Copy, Default)]
pub struct Browsers {
  pub ${Object.keys(browsers).filter(b => !(b in BROWSER_MAPPING)).sort().join(': Option<u32>,\n  pub ')}: Option<u32>
}

#[allow(dead_code)]
pub enum Feature {
  ${[...p.keys()].flat().map(enumify).sort().join(',\n  ')}
}

impl Feature {
  pub fn prefixes_for(&self, browsers: Browsers) -> VendorPrefix {
    let mut prefixes = VendorPrefix::None;
    match self {
      ${[...p].map(([features, versions]) => {
        return `${features.map(name => `Feature::${enumify(name)}`).join(' |\n      ')} => {
        ${Object.entries(versions).map(([name, prefixes]) => {
          return `if let Some(version) = browsers.${name} {
          ${Object.entries(prefixes).map(([prefix, [min, max]]) => {
            if (!prefixMapping[prefix]) {
              throw new Error('Missing prefix ' + prefix);
            }
            return `if version >= ${min} && version <= ${max} {
            prefixes |= VendorPrefix::${prefixMapping[prefix]};
          }`
          }).join('\n          ')}
        }`;
        }).join('\n        ')}
      }`
      }).join(',\n      ')}
    }
    prefixes
  }
}

pub fn is_flex_2009(browsers: Browsers) -> bool {
  ${Object.entries(flexSpec).map(([name, [min, max]]) => {
    return `if let Some(version) = browsers.${name} {
    if version >= ${min} && version <= ${max} {
      return true;
    }
  }`;
  }).join('\n  ')}
  false
}

pub fn is_webkit_gradient(browsers: Browsers) -> bool {
  ${Object.entries(oldGradient).map(([name, [min, max]]) => {
    return `if let Some(version) = browsers.${name} {
    if version >= ${min} && version <= ${max} {
      return true;
    }
  }`;
  }).join('\n  ')}
  false
}
`;

fs.writeFileSync('src/properties/prefixes.rs', s);

let c = `// This file is autogenerated by build-prefixes.js. DO NOT EDIT!

use crate::properties::prefixes::Browsers;

pub enum Feature {
  ${Object.keys(compat).map(enumify).sort().join(',\n  ')}
}

impl Feature {
  pub fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      ${Object.entries(compat).map(([name, browsers]) => 
        `Feature::${enumify(name)} => {
        ${Object.entries(browsers).map(([browser, min]) => 
            `if let Some(version) = browsers.${browser} {
          if version >= ${min} {
            return true
          }
        }`
          ).join('\n        ')}
      }`
      ).join('\n      ')}
    }
    false
  }
}
`;

fs.writeFileSync('src/compat.rs', c);


function parseVersion(version) {
  let [major, minor = '0', patch = '0'] = version
    .split('-')[0]
    .split('.')
    .map(v => parseInt(v, 10));

  if (isNaN(major) || isNaN(minor) || isNaN(patch)) {
    return null;
  }

  return major << 16 | minor << 8 | patch;
}
