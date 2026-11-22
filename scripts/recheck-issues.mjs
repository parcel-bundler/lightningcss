#!/usr/bin/env node
// Re-run every stored issue repro (.claude/triage/repros/<issue>/*.mjs)
// against the local build (master) and the latest npm release, and report
// which ones changed state since the recorded baseline
// (.claude/triage/repro-status.json). Run it after compat data updates, after
// releases, or on a schedule, to find issues that are now fixed (or broken).
//
// Usage:
//   node scripts/recheck-issues.mjs [--update] [--issues 925,1319] [--json]
//     --update   write the current results as the new baseline
//
// Each repro must export check() (or cases with check()), see scripts/repro.mjs.
// Results per case: "correct", "WRONG", or null (no check / failed to run).
// Statuses reported:
//   fixed on master     master went WRONG -> correct (unreleased)
//   fixed in release    the latest release went WRONG -> correct
//   regressed           master or release went correct -> WRONG
//   new                 no baseline yet

import fs from 'fs';
import path from 'path';
import {spawnSync} from 'child_process';
import {fileURLToPath} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const REPROS = path.join(ROOT, '.claude/triage/repros');
const STATUS = path.join(ROOT, '.claude/triage/repro-status.json');

let argv = process.argv.slice(2);
let update = argv.includes('--update');
let json = argv.includes('--json');
let only = argv.includes('--issues') ? new Set(argv[argv.indexOf('--issues') + 1].split(',')) : null;

let releases = JSON.parse(spawnSync('npm', ['view', 'lightningcss', 'versions', '--json'], {encoding: 'utf8'}).stdout)
  .filter(v => /^1\.\d+\.\d+$/.test(v));
let latest = releases.sort((a, b) => {
  let x = a.split('.').map(Number), y = b.split('.').map(Number);
  return x[0] - y[0] || x[1] - y[1] || x[2] - y[2];
}).at(-1);

let baseline = fs.existsSync(STATUS) ? JSON.parse(fs.readFileSync(STATUS, 'utf8')) : {results: {}};
let state = (check) => (check === null || check === undefined ? null : check ? 'correct' : 'WRONG');

// Closed issues (from the local index, if present) are reported so they can be pruned.
let closed = new Set();
let index = path.join(ROOT, 'target/review/issues.jsonl');
if (fs.existsSync(index)) {
  for (let line of fs.readFileSync(index, 'utf8').trim().split('\n')) {
    let r = JSON.parse(line);
    if (r.state === 'closed') closed.add(String(r.number));
  }
}

let current = {};
let changes = [];
for (let issue of fs.readdirSync(REPROS).sort((a, b) => a - b)) {
  if (only && !only.has(issue)) continue;
  for (let file of fs.readdirSync(path.join(REPROS, issue)).filter(f => f.endsWith('.mjs')).sort()) {
    let res = spawnSync(process.execPath, [path.join(ROOT, 'scripts/repro.mjs'), path.join(REPROS, issue, file), '--versions', `local,${latest}`, '--json'], {encoding: 'utf8', timeout: 600000});
    let parsed;
    try {
      parsed = JSON.parse(res.stdout);
    } catch {
      changes.push({issue, key: file, status: 'failed to run', detail: (res.stderr || '').trim().split('\n').pop()});
      continue;
    }
    // A crash replaces all of a version's cases with one '*' result; it
    // counts as WRONG for every case (crashing is never the desired behavior).
    let crashed = new Set(parsed.results.filter(r => r.name === '*').map(r => r.version));
    for (let name of [...new Set(parsed.results.map(r => r.name))].filter(n => n !== '*')) {
      let key = name === 'default' ? file : `${file} / ${name}`;
      let pick = (v) => crashed.has(v) ? 'WRONG' : state(parsed.results.find(r => r.version === v && r.name === name)?.check);
      let now = {master: pick('local'), release: pick(latest), releaseVersion: latest};
      current[`${issue}: ${key}`] = now;
      let before = baseline.results[`${issue}: ${key}`];
      let note = closed.has(issue) ? ' (issue is closed)' : '';
      if (!before) {
        changes.push({issue, key, status: 'new', detail: `master ${now.master}, ${latest} ${now.release}${note}`});
        continue;
      }
      if (before.master === 'WRONG' && now.master === 'correct') changes.push({issue, key, status: 'fixed on master', detail: `release ${latest}: ${now.release}${note}`});
      if (before.release === 'WRONG' && now.release === 'correct') changes.push({issue, key, status: 'fixed in release', detail: `${before.releaseVersion} -> ${latest}${note}`});
      if (before.master === 'correct' && now.master === 'WRONG') changes.push({issue, key, status: 'regressed on master', detail: note});
      if (before.release === 'correct' && now.release === 'WRONG') changes.push({issue, key, status: 'regressed in release', detail: `${before.releaseVersion} -> ${latest}${note}`});
    }
  }
}

if (json) {
  console.log(JSON.stringify({latest, changes, current}, null, 2));
} else {
  console.log(`Re-checked ${Object.keys(current).length} repro cases against master and ${latest}.`);
  if (!changes.length) console.log('No changes since the baseline.');
  for (let c of changes) console.log(`#${c.issue} ${c.key}: ${c.status}${c.detail ? ` (${c.detail})` : ''}`);
}
if (update) {
  let results = only ? {...baseline.results, ...current} : current;
  fs.writeFileSync(STATUS, JSON.stringify({updated: new Date().toISOString().slice(0, 10), results}, null, 2) + '\n');
  if (!json) console.log(`Baseline written to ${path.relative(ROOT, STATUS)}.`);
}
