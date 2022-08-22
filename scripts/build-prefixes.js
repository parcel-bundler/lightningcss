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
  oculus: null,
};

const MDN_BROWSER_MAPPING = {
  chrome_android: 'chrome',
  firefox_android: 'firefox',
  opera_android: 'opera',
  safari_ios: 'ios_saf',
  samsunginternet_android: 'samsung',
  webview_android: 'android',
  oculus: null,
};

// Caniuse data for clip-path is incorrect.
// https://github.com/Fyrd/caniuse/issues/6209
prefixes['clip-path'].browsers = prefixes['clip-path'].browsers.filter(b => {
  let [name, version] = b.split(' ');
  return !(
    (name === 'safari' && parseVersion(version) >= (9 << 16 | 1 << 8)) ||
    (name === 'ios_saf' && parseVersion(version) >= (9 << 16 | 3 << 8))
  );
});

prefixes['any-pseudo'] = {
  browsers: Object.entries(mdn.css.selectors.is.__compat.support)
    .flatMap(([key, value]) => {
      if (Array.isArray(value)) {
        key = MDN_BROWSER_MAPPING[key] || key;
        let any = value.find(v => v.alternative_name?.includes('-any'))?.version_added;
        let supported = value.find(x => x.version_added && !x.alternative_name)?.version_added;
        if (any && supported) {
          let parts = supported.split('.');
          parts[0]--;
          supported = parts.join('.');
          return [`${key} ${any}}`, `${key} ${supported}`];
        }
      }

      return [];
    })
}

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

    // https://github.com/postcss/autoprefixer/blob/main/lib/hacks/backdrop-filter.js#L11
    if (prefix === 'ms' && prop === 'backdrop-filter') {
      prefix = 'webkit';
    }

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
  addValue(p, browserMap, prop);
}

function addValue(map, value, prop) {
  let s = JSON.stringify(value);
  let found = false;
  for (let [key, val] of map) {
    if (JSON.stringify(val) === s) {
      key.push(prop);
      found = true;
      break;
    }
  }
  if (!found) {
    map.set([prop], value);
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
  'css-rrggbbaa',
  'css-nesting',
  'css-not-sel-list',
  'css-has',
  'font-family-system-ui'
];

let compat = new Map();
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

  addValue(compat, browserMap, feature);
}

// No browser supports custom media queries yet.
addValue(compat, {}, 'custom-media-queries');

let mdnFeatures = {
  doublePositionGradients: mdn.css.types.image.gradient['radial-gradient'].doubleposition.__compat.support,
  clamp: mdn.css.types.clamp.__compat.support,
  placeSelf: mdn.css.properties['place-self'].__compat.support,
  placeContent: mdn.css.properties['place-content'].__compat.support,
  placeItems: mdn.css.properties['place-items'].__compat.support,
  overflowShorthand: mdn.css.properties['overflow'].multiple_keywords.__compat.support,
  mediaRangeSyntax: mdn.css['at-rules'].media.range_syntax.__compat.support,
  mediaIntervalSyntax: {}, // currently no browsers
  logicalBorders: mdn.css.properties['border-inline-start'].__compat.support,
  logicalBorderShorthand: mdn.css.properties['border-inline'].__compat.support,
  logicalBorderRadius: mdn.css.properties['border-start-start-radius'].__compat.support,
  logicalMargin: mdn.css.properties['margin-inline-start'].__compat.support,
  logicalMarginShorthand: mdn.css.properties['margin-inline'].__compat.support,
  logicalPadding: mdn.css.properties['padding-inline-start'].__compat.support,
  logicalPaddingShorthand: mdn.css.properties['padding-inline'].__compat.support,
  logicalInset: mdn.css.properties['inset-inline-start'].__compat.support,
  logicalSize: mdn.css.properties['inline-size'].__compat.support,
  logicalTextAlign: mdn.css.properties['text-align']['flow_relative_values_start_and_end'].__compat.support,
  labColors: mdn.css.types.color.lab.__compat.support,
  oklabColors: {},
  colorFunction: mdn.css.types.color.color.__compat.support,
  spaceSeparatedColorFunction: mdn.css.types.color.rgb.space_separated_parameters.__compat.support,
  textDecorationThicknessPercent: mdn.css.properties['text-decoration-thickness'].percentage.__compat.support,
  textDecorationThicknessShorthand: mdn.css.properties['text-decoration']['text-decoration-thickness'].__compat.support,
  cue: mdn.css.selectors.cue.__compat.support,
  cueFunction: mdn.css.selectors.cue.selector_argument.__compat.support,
  anyPseudo: Object.fromEntries(
    Object.entries(mdn.css.selectors.is.__compat.support)
      .map(([key, value]) => {
        if (Array.isArray(value)) {
          value = value
            .filter(v => v.alternative_name?.includes('-any'))
            .map(({alternative_name, ...other}) => other);
        }

        if (value && value.length) {
          return [key, value];
        } else {
          return [key, {version_added: false}];
        }
      })
  ),
  imageSet: mdn.css.types.image['image-set'].__compat.support,
  xResolutionUnit: mdn.css.types.resolution.x.__compat.support
};

for (let feature in mdnFeatures) {
  let browserMap = {};
  for (let name in mdnFeatures[feature]) {
    if (MDN_BROWSER_MAPPING[name] === null) {
      continue;
    }

    let feat = mdnFeatures[feature][name];
    let version;
    if (Array.isArray(feat)) {
      version = feat.find(x => x.version_added && !x.alternative_name).version_added;
    } else {
      version = feat.version_added;
    }

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

  addValue(compat, browserMap, feature);
}

addValue(compat, {
  safari: parseVersion('10.1'),
  ios_saf: parseVersion('10.3')
}, 'p3Colors');

addValue(compat, {
  // https://github.com/WebKit/WebKit/commit/baed0d8b0abf366e1d9a6105dc378c59a5f21575
  safari: parseVersion('10.1'),
  ios_saf: parseVersion('10.3')
}, 'langList');

let prefixMapping = {
  webkit: 'WebKit',
  moz: 'Moz',
  ms: 'Ms',
  o: 'O'
};

let enumify = (f) => f.replace(/^@([a-z])/, (_, x) => 'At' + x.toUpperCase()).replace(/^::([a-z])/, (_, x) => 'PseudoElement' + x.toUpperCase()).replace(/^:([a-z])/, (_, x) => 'PseudoClass' + x.toUpperCase()).replace(/(^|-)([a-z])/g, (_, a, x) => x.toUpperCase())

let allBrowsers = Object.keys(browsers).filter(b => !(b in BROWSER_MAPPING)).sort();
let targets = `//! Browser target options.
// This file is autogenerated by build-prefixes.js. DO NOT EDIT!

use serde::{Deserialize, Serialize};

/// Browser versions to compile CSS for.
///
/// Versions are represented as a single 24-bit integer, with one byte
/// per \`major.minor.patch\` component.
///
/// # Example
///
/// This example represents a target of Safari 13.2.0.
///
/// \`\`\`
/// use parcel_css::targets::Browsers;
///
/// let targets = Browsers {
///   safari: Some((13 << 16) | (2 << 8)),
///   ..Browsers::default()
/// };
/// \`\`\`
#[derive(Serialize, Debug, Deserialize, Clone, Copy, Default)]
#[allow(missing_docs)]
pub struct Browsers {
  pub ${allBrowsers.join(': Option<u32>,\n  pub ')}: Option<u32>
}
`;

fs.writeFileSync('src/targets.rs', targets);

let targets_dts = `// This file is autogenerated by build-prefixes.js. DO NOT EDIT!

export interface Targets {
  ${allBrowsers.join('?: number,\n  ')}?: number
}
`;

fs.writeFileSync('node/targets.d.ts', targets_dts);

let s = `// This file is autogenerated by build-prefixes.js. DO NOT EDIT!

use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;

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
              return `if ${min === max ? `version == ${min}` : `version >= ${min} && version <= ${max}`} {
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

fs.writeFileSync('src/prefixes.rs', s);

let c = `// This file is autogenerated by build-prefixes.js. DO NOT EDIT!

use crate::targets::Browsers;

#[derive(Clone, Copy, PartialEq)]
pub enum Feature {
  ${[...compat.keys()].flat().map(enumify).sort().join(',\n  ')}
}

impl Feature {
  pub fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      ${[...compat].map(([features, supportedBrowsers]) =>
        `${features.map(name => `Feature::${enumify(name)}`).join(' |\n      ')} => {` + (Object.entries(supportedBrowsers).length === 0 ? '\n        return false\n      }' : `
        ${Object.entries(supportedBrowsers).map(([browser, min]) =>
            `if let Some(version) = browsers.${browser} {
          if version < ${min} {
            return false
          }
        }`).join('\n        ')}${Object.keys(supportedBrowsers).length === allBrowsers.length ? '' : `\n        if ${allBrowsers.filter(b => !supportedBrowsers[b]).map(browser => `browsers.${browser}.is_some()`).join(' || ')} {
          return false
        }`}
      }`
      )).join('\n      ')}
    }
    true
  }

  pub fn is_partially_compatible(&self, targets: Browsers) -> bool {
    let mut browsers = Browsers::default();
    ${allBrowsers.map(browser => `if targets.${browser}.is_some() {
      browsers.${browser} = targets.${browser};
      if self.is_compatible(browsers) {
        return true
      }
      browsers.${browser} = None;
    }\n`).join('    ')}
    false
  }
}
`;

fs.writeFileSync('src/compat.rs', c);


function parseVersion(version) {
  version = version.replace('≤', '');
  let [major, minor = '0', patch = '0'] = version
    .split('-')[0]
    .split('.')
    .map(v => parseInt(v, 10));

  if (isNaN(major) || isNaN(minor) || isNaN(patch)) {
    return null;
  }

  return major << 16 | minor << 8 | patch;
}
