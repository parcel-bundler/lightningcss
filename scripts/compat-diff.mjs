#!/usr/bin/env node
// Summarize what a browser data update changes in the generated compat data
// (src/compat.rs and src/prefixes.rs, produced by scripts/build-prefixes.js),
// in terms a reviewer can check: per feature and browser, the minimum version
// before and after. Also flags anomalies in the new data that past bugs came
// from.
//
// Usage:
//   node scripts/compat-diff.mjs [--base REF] [--json] [--anomalies-only]
//     --base REF   git ref for the old data (default HEAD); the new data is the
//                  working tree. Run build-prefixes.js first.
//
// Change kinds:
//   newly supported     a browser gains support (expected as browsers ship)
//   earlier             a minimum version went down (data correction)
//   LATER / REMOVED     a minimum version went up, or support was removed:
//                       suspicious (#925 was a cap, #568 an Android reset)
//   prefix changes      vendor prefix ranges added, removed or changed
// Anomalies (in the new data):
//   android-lags-chrome   Android WebView has tracked Chrome since 37, so an
//                         Android minimum far above Chrome's is a data artifact
//                         (caniuse lists only the current WebView version, #568)
//   ios-lags-safari       iOS Safari needing a much newer version than Safari
//   prefix-capped-at-latest  a prefix is needed up to exactly the latest known
//                         version, so the next release is assumed unprefixed (#925)
//
// Exit code: 0, or 1 when there are suspicious changes or new anomalies.

import fs from 'fs';
import path from 'path';
import {execFileSync} from 'child_process';
import {fileURLToPath} from 'url';
import {createRequire} from 'module';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const argv = process.argv.slice(2);
const base = argv.includes('--base') ? argv[argv.indexOf('--base') + 1] : 'HEAD';
const json = argv.includes('--json');
const anomaliesOnly = argv.includes('--anomalies-only');

const decode = (v) => (v == null ? '-' : `${v >> 16}${(v >> 8) & 255 ? '.' + ((v >> 8) & 255) : ''}${v & 255 ? '.' + (v & 255) : ''}`);

function parseCompat(text) {
  let body = text.slice(text.indexOf('pub fn is_compatible'), text.indexOf('pub fn is_partially_compatible'));
  let map = new Map();
  let re = /((?:Feature::\w+\s*\|\s*)*Feature::\w+)\s*=>\s*\{([\s\S]*?)\n {6}\}/g;
  for (let m of body.matchAll(re)) {
    let names = [...m[1].matchAll(/Feature::(\w+)/g)].map(x => x[1]);
    let block = m[2];
    let support = {};
    if (/^\s*return false\s*$/.test(block)) {
      support.__none = true;
    } else {
      for (let b of block.matchAll(/browsers\.(\w+) \{\s*if version < (\d+)/g)) support[b[1]] = Number(b[2]);
      let unsupported = block.match(/if ((?:browsers\.\w+\.is_some\(\)(?: \|\| )?)+) \{\s*return false/);
      if (unsupported) for (let b of unsupported[1].matchAll(/browsers\.(\w+)\.is_some/g)) support[b[1]] = null;
    }
    for (let n of names) map.set(n, support);
  }
  return map;
}

function parsePrefixes(text) {
  let body = text.slice(text.indexOf('pub fn prefixes_for'));
  let map = new Map();
  let re = /((?:Feature::\w+\s*\|\s*)*Feature::\w+)\s*=>\s*\{([\s\S]*?)\n {6}\}/g;
  for (let m of body.matchAll(re)) {
    let names = [...m[1].matchAll(/Feature::(\w+)/g)].map(x => x[1]);
    let ranges = [];
    for (let b of m[2].matchAll(/browsers\.(\w+) \{\s*if ([^{]+)\{\s*prefixes \|= VendorPrefix::(\w+)/g)) {
      let min = b[2].match(/version >= (\d+)/), max = b[2].match(/version <= (\d+)/);
      ranges.push(`${b[1]} ${min ? decode(+min[1]) : '*'}-${max ? decode(+max[1]) : '*'} ${b[3]}`);
    }
    for (let n of names) map.set(n, ranges.sort());
  }
  return map;
}

function read(ref, file) {
  if (ref === null) return fs.readFileSync(path.join(ROOT, file), 'utf8');
  return execFileSync('git', ['show', `${ref}:${file}`], {cwd: ROOT, encoding: 'utf8', maxBuffer: 64 << 20});
}

// Latest released version per browser, from caniuse-lite in node_modules.
function latestVersions() {
  let latest = {};
  try {
    let req = createRequire(path.join(ROOT, 'package.json'));
    let agents = req('caniuse-lite').agents;
    for (let [k, v] of Object.entries(agents)) {
      let versions = Object.keys(v.release_date).filter(x => v.release_date[x]).map(x => {
        let [a, b = 0, c = 0] = x.split('-')[0].split('.').map(Number);
        return (a << 16) | (b << 8) | c;
      }).filter(Number.isFinite);
      if (versions.length) latest[k] = Math.max(...versions);
    }
  } catch {}
  return latest;
}

// A prefix range ending exactly at the latest released version means "needs the
// prefix up to the newest browser we know of": if that browser still needs the
// prefix, the next release is wrongly treated as unprefixed (#925).
function prefixAnomalies(prefixes, latest) {
  let out = [];
  for (let [feature, ranges] of prefixes) {
    for (let r of ranges) {
      let [browser, range] = r.split(' ');
      let max = range.split('-')[1];
      if (!max || max === '*' || !latest[browser]) continue;
      if (max === decode(latest[browser])) out.push({kind: 'prefix-capped-at-latest', feature, detail: `${r}: ends at the latest known ${browser}`});
    }
  }
  return out;
}

function anomalies(compat) {
  let out = [];
  for (let [feature, s] of compat) {
    if (s.__none) continue;
    if (s.android && s.chrome && s.android > (37 << 16) && s.chrome <= (140 << 16) && s.android > s.chrome + (5 << 16)) {
      out.push({kind: 'android-lags-chrome', feature, detail: `android ${decode(s.android)} vs chrome ${decode(s.chrome)}`});
    }
    if (s.ios_saf && s.safari && s.ios_saf > s.safari + (1 << 16)) {
      out.push({kind: 'ios-lags-safari', feature, detail: `ios_saf ${decode(s.ios_saf)} vs safari ${decode(s.safari)}`});
    }
  }
  return out;
}

let oldCompat = parseCompat(read(base, 'src/compat.rs'));
let newCompat = parseCompat(read(null, 'src/compat.rs'));
let oldPrefixes = parsePrefixes(read(base, 'src/prefixes.rs'));
let newPrefixes = parsePrefixes(read(null, 'src/prefixes.rs'));

let changes = [];
for (let feature of new Set([...oldCompat.keys(), ...newCompat.keys()])) {
  let a = oldCompat.get(feature), b = newCompat.get(feature);
  if (!a) { changes.push({feature, kind: 'new feature', detail: Object.entries(b).map(([k, v]) => `${k} ${decode(v)}`).join(', ')}); continue; }
  if (!b) { changes.push({feature, kind: 'REMOVED feature', detail: ''}); continue; }
  for (let browser of new Set([...Object.keys(a), ...Object.keys(b)])) {
    if (browser === '__none') continue;
    // A feature no browser supports has no per-browser entries: treat as null.
    let va = a.__none ? null : a[browser] ?? null, vb = b.__none ? null : b[browser] ?? null;
    if (va === vb) continue;
    let kind = va == null && vb != null ? 'newly supported'
      : va != null && vb == null ? 'REMOVED support'
      : vb < va ? 'earlier' : 'LATER';
    changes.push({feature, kind, detail: `${browser} ${decode(va)} -> ${decode(vb)}`});
  }
}
for (let feature of new Set([...oldPrefixes.keys(), ...newPrefixes.keys()])) {
  let a = (oldPrefixes.get(feature) || []).join('; '), b = (newPrefixes.get(feature) || []).join('; ');
  if (a !== b) changes.push({feature, kind: 'prefix change', detail: `${a || '(none)'}  =>  ${b || '(none)'}`});
}

let latest = latestVersions();
let oldAnomalies = new Set([...anomalies(oldCompat), ...prefixAnomalies(oldPrefixes, latest)].map(a => `${a.kind}:${a.feature}:${a.detail}`));
let found = [...anomalies(newCompat), ...prefixAnomalies(newPrefixes, latest)].map(a => ({...a, new: !oldAnomalies.has(`${a.kind}:${a.feature}:${a.detail}`)}));
let suspicious = changes.filter(c => /^[A-Z]/.test(c.kind) || c.kind === 'prefix change');

if (json) {
  console.log(JSON.stringify({base, changes, anomalies: found}, null, 2));
} else {
  if (!anomaliesOnly) {
    let count = (k) => changes.filter(c => c.kind === k).length;
    console.log(`## Browser data changes vs ${base}\n`);
    console.log(`${changes.length} changes: ${count('newly supported')} newly supported, ${count('earlier')} earlier, ${count('LATER') + count('REMOVED support')} later or removed, ${count('prefix change')} prefix changes, ${count('new feature')} new and ${count('REMOVED feature')} removed features.\n`);
    if (suspicious.length) {
      console.log(`### Needs review (${suspicious.length})\n\n| feature | change | detail |\n|---|---|---|`);
      for (let c of suspicious) console.log(`| ${c.feature} | ${c.kind} | ${c.detail} |`);
      console.log();
    }
    let rest = changes.filter(c => !suspicious.includes(c));
    if (rest.length) {
      console.log(`<details><summary>Expected changes (${rest.length})</summary>\n\n| feature | change | detail |\n|---|---|---|`);
      for (let c of rest) console.log(`| ${c.feature} | ${c.kind} | ${c.detail} |`);
      console.log('\n</details>\n');
    }
  }
  let newOnes = found.filter(a => a.new);
  console.log(`### Data anomalies (${found.length}${newOnes.length ? `, ${newOnes.length} new` : ''})\n`);
  if (!found.length) console.log('None.');
  else {
    console.log('| kind | feature | detail | new? |\n|---|---|---|---|');
    for (let a of found) console.log(`| ${a.kind} | ${a.feature} | ${a.detail} | ${a.new ? 'NEW' : ''} |`);
  }
}
process.exit(suspicious.length || found.some(a => a.new) ? 1 : 0);
