#!/usr/bin/env node
// Search the local issue/PR index (target/review/issues.jsonl, built by
// scripts/fetch-issues.mjs) and show which PRs reference each match.
//
// Usage:
//   node scripts/find-issues.mjs REGEX [REGEX...]   ALL regexes must match (title or body), case-insensitive
//   node scripts/find-issues.mjs --any REGEX [REGEX...]  ANY regex may match
//   node scripts/find-issues.mjs --refs 1065         PRs/issues that mention #1065 ("fixes #1065", "#1065", ...)
//   options: --open, --closed, --issues, --prs, --limit N
import fs from 'fs';
import path from 'path';
import {fileURLToPath} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const INDEX = path.join(ROOT, 'target/review/issues.jsonl');
if (!fs.existsSync(INDEX)) {
  console.error('No index. Run: node scripts/fetch-issues.mjs');
  process.exit(2);
}
let rows = fs.readFileSync(INDEX, 'utf8').trim().split('\n').map(l => JSON.parse(l));
let byNumber = new Map(rows.map(r => [r.number, r]));

// Which items mention which numbers; "fixes/closes/resolves #N" marks a fix.
let mentions = new Map();
for (let r of rows) {
  for (let m of `${r.title}\n${r.body}`.matchAll(/(\b(?:fix(?:e[sd])?|close[sd]?|resolve[sd]?)\s+)?#(\d+)\b/gi)) {
    let n = Number(m[2]);
    if (n === r.number) continue;
    if (!mentions.has(n)) mentions.set(n, new Map());
    let prev = mentions.get(n).get(r.number);
    mentions.get(n).set(r.number, prev || Boolean(m[1]));
  }
}

let argv = process.argv.slice(2);
let flags = new Set(argv.filter(a => a.startsWith('--') && a !== '--limit' && a !== '--refs'));
let any = flags.has('--any');
let limit = argv.includes('--limit') ? Number(argv[argv.indexOf('--limit') + 1]) : 30;
let fmt = (r) => {
  let kind = r.kind === 'pr' ? (r.merged ? 'PR merged' : `PR ${r.state}`) : `issue ${r.state}`;
  return `#${r.number} [${kind}] ${r.created} r=${r.reactions ?? 0} c=${r.comments ?? 0} ${r.title}`;
};
let refsOf = (n) => [...(mentions.get(n) || [])]
  .map(([num, fixes]) => ({r: byNumber.get(num), fixes}))
  .filter(x => x.r)
  .sort((a, b) => b.fixes - a.fixes || a.r.number - b.r.number);

if (argv.includes('--refs')) {
  let n = Number(argv[argv.indexOf('--refs') + 1]);
  let self = byNumber.get(n);
  if (self) console.log(fmt(self));
  for (let {r, fixes} of refsOf(n)) console.log(`  ${fixes ? 'fixes' : 'mentions'}: ${fmt(r)}`);
  process.exit(0);
}

let patterns = argv.filter((a, i) => !a.startsWith('--') && argv[i - 1] !== '--limit').map(p => new RegExp(p, 'i'));
if (!patterns.length) {
  console.error('Usage: node scripts/find-issues.mjs [--any] REGEX [REGEX...] | --refs N  [--open|--closed] [--issues|--prs] [--limit N]\nWithout --any, all patterns must match.');
  process.exit(2);
}
let matches = rows.filter(r =>
  (any ? patterns.some(p => p.test(r.title) || p.test(r.body)) : patterns.every(p => p.test(r.title) || p.test(r.body))) &&
  (!flags.has('--open') || r.state === 'open') &&
  (!flags.has('--closed') || r.state === 'closed') &&
  (!flags.has('--issues') || r.kind === 'issue') &&
  (!flags.has('--prs') || r.kind === 'pr'));
// Title matches first, then most recent.
matches.sort((a, b) => (patterns.every(p => p.test(b.title)) - patterns.every(p => p.test(a.title))) || b.number - a.number);
for (let r of matches.slice(0, limit)) {
  console.log(fmt(r));
  if (r.kind === 'issue') for (let {r: ref, fixes} of refsOf(r.number).filter(x => x.r.kind === 'pr')) console.log(`    ${fixes ? 'fixed by' : 'mentioned in'} ${fmt(ref)}`);
}
if (matches.length > limit) console.log(`... ${matches.length - limit} more (--limit)`);
