#!/usr/bin/env node
// Run an issue repro against the local build and published npm versions of
// Lightning CSS, compare results, and optionally bisect to the release where
// the behavior changed. Each version runs in its own process.
//
// Usage:
//   node scripts/repro.mjs REPRO.mjs [--versions local,1.33.0,1.30.2]
//        [--latest N] [--all] [--range A..B] [--bisect] [--json]
//
// REPRO.mjs is an ES module. Either export `css` and `options` (passed to
// transform() along with filename 'input.css'), or a `run(lightningcss)`
// function returning the output string (may be async; use this for bundle(),
// visitors, browserslistToTargets, etc.). Optionally export `check(output)`,
// returning true when the output is correct; errors count as output too
// ("ERROR: <message>"), so check() can test for them.
//
//   export const css = `.a { margin-inline-start: 4px }`;
//   export const options = {minify: true, targets: {safari: 12 << 16}};
//   export const check = (out) => out.includes('margin-left');
//
// Versions:
//   local       the repo's Node binding (build it with `node scripts/build.js`;
//               needs node_modules/.bin on PATH for napi)
//   1.30.2      a published version, installed from npm into target/review/npm
// Default: local plus the latest 3 releases. --latest N picks the latest N,
// --all every 1.x release. --range A..B limits releases to A through B (e.g.
// the "last good" and "first bad" versions an issue reports). --bisect (needs
// check()) binary-searches the releases in range, oldest to newest, for the
// first one whose check() result differs from the oldest, assuming one
// change. A regression that was later fixed needs a --range inside it.

import fs from 'fs';
import path from 'path';
import {execFileSync, spawnSync} from 'child_process';
import {fileURLToPath, pathToFileURL} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const CACHE = path.join(ROOT, 'target/review/npm');

// Child mode: load one version and run the repro.
if (process.argv[2] === '--child') {
  let [, , , pkg, repro] = process.argv;
  let result;
  try {
    let lightningcss = (await import(pathToFileURL(pkg).href)).default;
    let mod = await import(pathToFileURL(repro).href);
    let output;
    try {
      if (mod.run) {
        output = String(await mod.run(lightningcss));
      } else {
        let res = lightningcss.transform({filename: 'input.css', code: Buffer.from(mod.css), ...(mod.options || {})});
        output = res.code.toString();
        if (res.warnings?.length) output += `\n/* warnings: ${res.warnings.map(w => w.message).join('; ')} */`;
      }
    } catch (err) {
      output = `ERROR: ${err.message || err}`;
    }
    result = {output, check: mod.check ? Boolean(await mod.check(output)) : null};
  } catch (err) {
    result = {output: `LOAD ERROR: ${err.message}`, check: null};
  }
  process.stdout.write(JSON.stringify(result));
  process.exit(0);
}

const cmp = (a, b) => {
  let x = a.split('.').map(Number), y = b.split('.').map(Number);
  return x[0] - y[0] || x[1] - y[1] || x[2] - y[2];
};

let releaseRange = null;
function releases() {
  let list = JSON.parse(execFileSync('npm', ['view', 'lightningcss', 'versions', '--json'], {encoding: 'utf8'}));
  list = list.filter(v => /^1\.\d+\.\d+$/.test(v)).sort(cmp);
  if (releaseRange) list = list.filter(v => cmp(v, releaseRange[0]) >= 0 && cmp(v, releaseRange[1]) <= 0);
  return list;
}

function packageDir(version) {
  if (version === 'local') {
    let entry = path.join(ROOT, 'node/index.js');
    let hasBinary = fs.readdirSync(ROOT).some(f => f.endsWith('.node'));
    if (!hasBinary) throw new Error('No local Node binding. Run: PATH=node_modules/.bin:$PATH node scripts/build.js');
    return entry;
  }
  let dir = path.join(CACHE, version);
  let entry = path.join(dir, 'node_modules/lightningcss/node/index.js');
  if (!fs.existsSync(entry)) {
    // Install into a temporary directory and rename, so concurrent runs
    // never see a half-installed version.
    fs.mkdirSync(CACHE, {recursive: true});
    let tmp = fs.mkdtempSync(path.join(CACHE, `.${version}-`));
    execFileSync('npm', ['install', '--no-save', '--no-audit', '--no-fund', '--prefix', tmp, `lightningcss@${version}`], {stdio: 'ignore'});
    try {
      fs.renameSync(tmp, dir);
    } catch {
      fs.rmSync(tmp, {recursive: true, force: true}); // another run won the race
    }
  }
  return entry;
}

function runVersion(version, repro) {
  let pkg;
  try {
    pkg = packageDir(version);
  } catch (err) {
    return {version, output: `INSTALL ERROR: ${err.message}`, check: null};
  }
  let child = spawnSync(process.execPath, [fileURLToPath(import.meta.url), '--child', pkg, repro], {encoding: 'utf8', timeout: 60000});
  try {
    return {version, ...JSON.parse(child.stdout)};
  } catch {
    return {version, output: `CRASH: ${(child.stderr || child.error?.message || 'no output').trim().split('\n').slice(-3).join(' ')}`, check: null};
  }
}

async function main() {
  let argv = process.argv.slice(2);
  let opts = {repro: null, versions: null, latest: 3, all: false, range: null, bisect: false, json: false};
  for (let i = 0; i < argv.length; i++) {
    switch (argv[i]) {
      case '--versions': opts.versions = argv[++i].split(','); break;
      case '--latest': opts.latest = Number(argv[++i]); break;
      case '--all': opts.all = true; break;
      case '--bisect': opts.bisect = true; break;
      case '--range': opts.range = argv[++i].split('..'); break;
      case '--json': opts.json = true; break;
      case '-h': case '--help':
        console.log(fs.readFileSync(fileURLToPath(import.meta.url), 'utf8').split('\n\n')[1].replace(/^\/\/ ?/gm, ''));
        process.exit(0);
      default: opts.repro = path.resolve(argv[i]);
    }
  }
  if (!opts.repro) throw new Error('Usage: node scripts/repro.mjs REPRO.mjs [--versions ...] [--bisect] (see --help)');

  releaseRange = opts.range;
  let results = [];
  let all = null;
  let versions = opts.versions;
  if (!versions) {
    all = releases();
    versions = opts.range
      ? all.slice().reverse()
      : ['local', ...(opts.all ? all.slice().reverse() : all.slice(-opts.latest).reverse())];
  }
  for (let v of versions) results.push(runVersion(v, opts.repro));

  let bisect = null;
  if (opts.bisect) {
    all ??= releases();
    let cache = new Map(results.map(r => [r.version, r]));
    let at = (i) => {
      let v = all[i];
      if (!cache.has(v)) cache.set(v, runVersion(v, opts.repro));
      return cache.get(v);
    };
    let lo = 0, hi = all.length - 1;
    let first = at(lo).check, last = at(hi).check;
    if (first === null || last === null) {
      bisect = {error: 'check() missing, or the oldest/newest release failed to load'};
    } else if (first === last) {
      bisect = {result: `check() is ${first} in both ${all[lo]} and ${all[hi]}; no change to bisect`};
    } else {
      while (hi - lo > 1) {
        let mid = (lo + hi) >> 1;
        let c = at(mid).check;
        if (c === first) lo = mid; else hi = mid;
      }
      bisect = {result: `check() changed from ${first} to ${!first} between ${all[lo]} and ${all[hi]}`, before: all[lo], after: all[hi]};
    }
    for (let [v, r] of cache) if (!results.some(x => x.version === v)) results.push(r);
  }

  if (opts.json) {
    console.log(JSON.stringify({results, bisect}, null, 2));
    return;
  }
  // Group identical outputs.
  let groups = new Map();
  for (let r of results) {
    let key = r.output;
    if (!groups.has(key)) groups.set(key, {output: r.output, check: r.check, versions: []});
    groups.get(key).versions.push(r.version);
  }
  for (let g of groups.values()) {
    let status = g.check === null ? '' : g.check ? '  [check: correct]' : '  [check: WRONG]';
    console.log(`--- ${g.versions.join(', ')}${status}\n${g.output.trim()}\n`);
  }
  if (results.every(r => /^(ERROR|CRASH|LOAD ERROR|INSTALL ERROR):/.test(r.output))) {
    console.log('note: every version errored; check the repro itself before concluding it is a bug.');
  }
  if (bisect) console.log(`bisect: ${bisect.result || bisect.error}`);
}

main().catch(err => {
  console.error(err.stack || err.message);
  process.exit(2);
});
