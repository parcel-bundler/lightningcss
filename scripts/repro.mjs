#!/usr/bin/env node
// Run an issue repro against the local build and published npm versions of
// Lightning CSS, compare results, and optionally bisect to the release where
// the behavior changed. Each version runs in its own process.
//
// Usage:
//   node scripts/repro.mjs REPRO.mjs [--versions local,1.33.0,1.30.2]
//        [--latest N] [--all] [--range A..B] [--bisect] [--case NAME]
//        (--case limits output to one named case; with --bisect it picks the case to bisect)
//        [--repeat N] [--json]
//
// REPRO.mjs is an ES module exporting one case:
//   export const css = `.a { margin-inline-start: 4px }`;
//   export const options = {minify: true, targets: {safari: 12 << 16}};  // passed to transform()
//   export const check = (out) => out.includes('margin-left');           // true = correct
// or several named cases, each with its own check (preferred when an issue
// has several examples, so one check can't accidentally match another's text):
//   export const cases = [{name: 'basic', css, options, check}, ...];
// A case may export `run(lightningcss, {require})` instead of css/options, for
// bundle(), visitors, browserslistToTargets etc.; it returns the output string.
// `require` resolves packages from the repo's node_modules (e.g. browserslist).
//
// transform() gets filename 'input.css' unless options sets filename (set it
// to undefined to test a missing filename). Warnings are appended to the
// output as a comment. Errors become the output "ERROR: <message>", so check()
// can test for them. Note: releases are built with panic = 'abort', so a Rust
// panic shows as CRASH there but as ERROR in the local debug build. The local
// binding is a debug build, roughly 10x slower than releases: don't compare
// timings across them. Each process has a 60 s timeout; for performance
// repros, time inside run() with small inputs and a generous budget.
//
// Versions:
//   local    the repo's Node binding (build: PATH=node_modules/.bin:$PATH node scripts/build.js)
//   1.30.2   a published version, installed from npm into target/review/npm
// Default: local plus the latest 3 releases. --latest N: the latest N.
// --all: every 1.x release. --range A..B: releases A through B.
// With --all or --range, every change in a case's check() result between
// consecutive releases is listed. --bisect binary-searches instead (fewer
// runs; assumes one change, so prefer --range when a case flipped repeatedly).
// --repeat N runs each version N times in separate processes and reports how
// many distinct outputs each case produced (for nondeterminism).

import fs from 'fs';
import path from 'path';
import {createRequire} from 'module';
import {execFileSync, spawnSync} from 'child_process';
import {fileURLToPath, pathToFileURL} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const CACHE = path.join(ROOT, 'target/review/npm');

// Child mode: load one version and run every case.
if (process.argv[2] === '--child') {
  let [, , , pkg, repro] = process.argv;
  let results;
  try {
    let lightningcss = (await import(pathToFileURL(pkg).href)).default;
    let mod = await import(pathToFileURL(repro).href);
    let cases = mod.cases ?? [{name: 'default', css: mod.css, options: mod.options, run: mod.run, check: mod.check}];
    if (process.env.LCSS_REPRO_CASE) cases = cases.filter(c => c.name === process.env.LCSS_REPRO_CASE);
    let require = createRequire(path.join(ROOT, 'package.json'));
    results = [];
    for (let c of cases) {
      let output;
      try {
        if (c.run) {
          output = String(await c.run(lightningcss, {require}));
        } else {
          let res = lightningcss.transform({filename: 'input.css', code: Buffer.from(c.css), ...(c.options || {})});
          output = res.code.toString();
          if (res.warnings?.length) output += `\n/* warnings: ${res.warnings.map(w => w.message).join('; ')} */`;
        }
      } catch (err) {
        output = `ERROR: ${err.message || err}`;
      }
      let check = null;
      if (c.check) {
        try {
          check = Boolean(await c.check(output));
        } catch {
          check = false;
        }
      }
      results.push({name: c.name, output, check});
    }
  } catch (err) {
    results = [{name: 'default', output: `LOAD ERROR: ${err.message}`, check: null}];
  }
  process.stdout.write(JSON.stringify(results));
  process.exit(0);
}

const cmp = (a, b) => {
  let x = a.split('.').map(Number), y = b.split('.').map(Number);
  return x[0] - y[0] || x[1] - y[1] || x[2] - y[2];
};

function releases(range) {
  // Cache the release list for a day to avoid a registry request per run.
  let cacheFile = path.join(CACHE, 'releases.json');
  let list;
  if (fs.existsSync(cacheFile) && Date.now() - fs.statSync(cacheFile).mtimeMs < 86400e3) {
    list = JSON.parse(fs.readFileSync(cacheFile, 'utf8'));
  } else {
    list = JSON.parse(execFileSync('npm', ['view', 'lightningcss', 'versions', '--json'], {encoding: 'utf8'}));
    fs.mkdirSync(CACHE, {recursive: true});
    fs.writeFileSync(cacheFile, JSON.stringify(list));
  }
  list = list.filter(v => /^1\.\d+\.\d+$/.test(v)).sort(cmp);
  if (range) list = list.filter(v => cmp(v, range[0]) >= 0 && cmp(v, range[1]) <= 0);
  return list;
}

function packageDir(version) {
  if (version === 'local') {
    let hasBinary = fs.readdirSync(ROOT).some(f => f.endsWith('.node'));
    if (!hasBinary) throw new Error('No local Node binding. Run: PATH=node_modules/.bin:$PATH node scripts/build.js');
    return path.join(ROOT, 'node/index.js');
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

function runChild(pkg, repro, caseName) {
  let env = caseName ? {...process.env, LCSS_REPRO_CASE: caseName} : process.env;
  let child = spawnSync(process.execPath, [fileURLToPath(import.meta.url), '--child', pkg, repro], {encoding: 'utf8', timeout: 60000, env});
  try {
    return JSON.parse(child.stdout);
  } catch {
    let msg = (child.stderr || child.error?.message || 'no output').trim().split('\n').slice(-3).join(' ');
    return [{name: caseName || '*', output: `CRASH: ${msg}`, check: caseName ? false : null}];
  }
}

let caseNamesCache;
async function caseNames(repro) {
  caseNamesCache ??= await import(pathToFileURL(repro).href).then(m => (m.cases || []).map(c => c.name), () => []);
  return caseNamesCache;
}

// A release panic aborts the whole process. When that happens, re-run each
// case in its own process so one crashing case doesn't hide the others; the
// crashing case itself counts as check() = false.
async function runOnce(pkg, repro) {
  let results = runChild(pkg, repro);
  if (results.length === 1 && results[0].name === '*') {
    let names = await caseNames(repro);
    if (names.length > 1) return names.flatMap(name => runChild(pkg, repro, name));
  }
  return results;
}

async function runVersion(version, repro, repeat) {
  let pkg;
  try {
    pkg = packageDir(version);
  } catch (err) {
    return [{version, name: '*', output: `INSTALL ERROR: ${err.message}`, check: null}];
  }
  let runs = [];
  for (let i = 0; i < repeat; i++) runs.push(await runOnce(pkg, repro));
  return runs[0].map((r, i) => {
    let distinct = new Set(runs.map(run => run[i]?.output));
    return {version, ...r, distinct: repeat > 1 ? distinct.size : undefined};
  });
}

async function main() {
  let argv = process.argv.slice(2);
  let opts = {repro: null, versions: null, latest: 3, all: false, range: null, bisect: false, caseName: null, repeat: 1, json: false};
  for (let i = 0; i < argv.length; i++) {
    switch (argv[i]) {
      case '--versions': opts.versions = argv[++i].split(','); break;
      case '--latest': opts.latest = Number(argv[++i]); break;
      case '--all': opts.all = true; break;
      case '--range': opts.range = argv[++i].split('..'); break;
      case '--bisect': opts.bisect = true; break;
      case '--case': opts.caseName = argv[++i]; break;
      case '--repeat': opts.repeat = Number(argv[++i]); break;
      case '--json': opts.json = true; break;
      case '-h': case '--help':
        console.log(fs.readFileSync(fileURLToPath(import.meta.url), 'utf8').split('\n\n')[0].replace(/^#!.*\n/, '').replace(/^\/\/ ?/gm, ''));
        process.exit(0);
      default: opts.repro = path.resolve(argv[i]);
    }
  }
  if (!opts.repro) throw new Error('Usage: node scripts/repro.mjs REPRO.mjs [--versions ...] [--range A..B] [--bisect] (see --help)');

  let byVersion = new Map();
  let run = async (v) => {
    if (!byVersion.has(v)) byVersion.set(v, await runVersion(v, opts.repro, opts.repeat));
    return byVersion.get(v);
  };

  let binary = opts.bisect && !opts.versions && !opts.all && !opts.range;
  let list = opts.versions ?? (opts.range
    ? releases(opts.range).reverse()
    : ['local', ...(opts.all ? releases().reverse() : releases().slice(-opts.latest).reverse())]);
  for (let v of binary ? ['local'] : list) await run(v);

  // Changes in check() between consecutive releases, per case.
  let transitions = [];
  let bisect = null;
  let rel = releases(opts.range);
  if (binary) {
    let pick = async (v) => (await run(v)).find(r => r.check !== null && (!opts.caseName || r.name === opts.caseName));
    let lo = 0, hi = rel.length - 1;
    let first = (await pick(rel[lo]))?.check, last = (await pick(rel[hi]))?.check;
    if (first == null || last == null) {
      bisect = 'check() missing, or the oldest/newest release failed to load';
    } else if (first === last) {
      bisect = `check() is ${first} in both ${rel[lo]} and ${rel[hi]}. A change that was later undone needs --range or --all.`;
    } else {
      while (hi - lo > 1) {
        let mid = (lo + hi) >> 1;
        if ((await pick(rel[mid]))?.check === first) lo = mid; else hi = mid;
      }
      bisect = `check() changed from ${first} to ${!first} between ${rel[lo]} and ${rel[hi]} (assuming a single change). Commits: git log --oneline v${rel[lo]}..v${rel[hi]} (if those tags exist; otherwise search commit messages for the version)`;
    }
  } else {
    let ran = rel.filter(v => byVersion.has(v));
    let caseNames = [...new Set([...byVersion.values()].flat().map(r => r.name))];
    for (let name of caseNames) {
      for (let i = 1; i < ran.length; i++) {
        let a = byVersion.get(ran[i - 1]).find(r => r.name === name);
        let b = byVersion.get(ran[i]).find(r => r.name === name);
        if (a && b && a.check !== null && b.check !== null && a.check !== b.check) {
          transitions.push(`${name}: ${a.check ? 'correct' : 'WRONG'} in ${ran[i - 1]} -> ${b.check ? 'correct' : 'WRONG'} in ${ran[i]}`);
        }
      }
    }
  }

  let results = [...byVersion.values()].flat();
  // --case limits the output (and transitions) to one case, in every mode.
  if (opts.caseName) {
    results = results.filter(r => r.name === opts.caseName || r.name === '*');
    transitions = transitions.filter(t => t.startsWith(`${opts.caseName}:`));
  }
  if (opts.json) {
    console.log(JSON.stringify({results, transitions, bisect}, null, 2));
    return;
  }
  let names = [...new Set(results.map(r => r.name))];
  for (let name of names) {
    if (names.length > 1 || name !== 'default') console.log(`===== case: ${name} =====`);
    let groups = new Map();
    for (let r of results.filter(r => r.name === name)) {
      if (!groups.has(r.output)) groups.set(r.output, {output: r.output, check: r.check, versions: [], distinct: []});
      let g = groups.get(r.output);
      g.versions.push(r.version);
      if (r.distinct > 1) g.distinct.push(`${r.version}: ${r.distinct} distinct outputs in ${opts.repeat} runs`);
    }
    for (let g of groups.values()) {
      let status = g.check === null ? '' : g.check ? '  [check: correct]' : '  [check: WRONG]';
      console.log(`--- ${g.versions.join(', ')}${status}\n${g.output.trim()}\n`);
      for (let d of g.distinct) console.log(`  ! nondeterministic: ${d}`);
    }
  }
  if (results.every(r => /^(ERROR|CRASH|LOAD ERROR|INSTALL ERROR):/.test(r.output))) {
    console.log('note: every version errored; check the repro itself before concluding it is a bug.');
  }
  if (transitions.length) console.log(`changes between releases:\n  ${transitions.join('\n  ')}`);
  if (bisect) console.log(`bisect: ${bisect}`);
}

main().catch(err => {
  console.error(err.stack || err.message);
  process.exit(2);
});
