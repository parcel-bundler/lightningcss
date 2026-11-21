#!/usr/bin/env node
// Build a local index of the repository's issues and pull requests (open and
// closed) for duplicate and "already fixed?" searches during triage.
// Writes target/review/issues.jsonl, one JSON object per line:
//   {number, kind: "issue"|"pr", state, merged, title, labels, created, closed, body}
// where body is the first 1500 characters. Uses the gh CLI's search API
// (rate-limited, so this takes a minute or two). Refreshes if older than a day
// unless --force or --max-age-hours N is given.
//
// Usage: node scripts/fetch-issues.mjs [--force] [--max-age-hours N]
//   then e.g.: grep -i 'layer' target/review/issues.jsonl | jq -r '"#\(.number) [\(.kind) \(.state)] \(.title)"'

import fs from 'fs';
import path from 'path';
import {execFileSync} from 'child_process';
import {fileURLToPath} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUT = path.join(ROOT, 'target/review/issues.jsonl');
const REPO = 'parcel-bundler/lightningcss';

let force = process.argv.includes('--force');
let maxAge = Number(process.argv[process.argv.indexOf('--max-age-hours') + 1]) || 24;
if (!force && fs.existsSync(OUT) && Date.now() - fs.statSync(OUT).mtimeMs < maxAge * 3600e3) {
  console.log(OUT);
  process.exit(0);
}

const sleep = (ms) => new Promise(r => setTimeout(r, ms));

// The search API returns at most 1000 results per query, so split by year.
async function search(q) {
  let items = [];
  for (let page = 1; page <= 10; page++) {
    let out;
    for (let attempt = 0; ; attempt++) {
      try {
        out = JSON.parse(execFileSync('gh', ['api', '-X', 'GET', 'search/issues', '-f', `q=${q}`, '-f', 'per_page=100', '-f', `page=${page}`], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']}));
        break;
      } catch (err) {
        if (attempt >= 4) throw err;
        await sleep(15000 * (attempt + 1)); // rate limit
      }
    }
    items.push(...out.items);
    if (out.items.length < 100) break;
    await sleep(7000);
  }
  return items;
}

let rows = new Map();
let year = new Date().getFullYear();
for (let y = 2021; y <= year; y++) {
  for (let kind of ['issue', 'pr']) {
    let items = await search(`repo:${REPO} is:${kind} created:${y}-01-01..${y}-12-31`);
    for (let it of items) {
      rows.set(it.number, {
        number: it.number,
        kind,
        state: it.state,
        merged: kind === 'pr' ? Boolean(it.pull_request?.merged_at) : undefined,
        title: it.title,
        labels: it.labels.map(l => l.name),
        created: it.created_at.slice(0, 10),
        closed: it.closed_at?.slice(0, 10) ?? null,
        reactions: it.reactions?.total_count ?? 0,
        comments: it.comments,
        body: (it.body || '').slice(0, 1500),
      });
    }
    process.stderr.write(`${y} ${kind}: ${items.length}\n`);
    await sleep(7000);
  }
}
fs.mkdirSync(path.dirname(OUT), {recursive: true});
fs.writeFileSync(OUT, [...rows.values()].sort((a, b) => a.number - b.number).map(r => JSON.stringify(r)).join('\n') + '\n');
console.log(OUT);
