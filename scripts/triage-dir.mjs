#!/usr/bin/env node
// Locate the triage data (issue repros, status baseline, triage records and
// calibration logs). It lives on the `triage` branch, which has no shared
// history with master and is updated directly by the triage automation, so
// repro changes don't need PRs. See the README on that branch.
//
// Resolution: $LCSS_TRIAGE_DIR if set, else a worktree of the `triage` branch
// at target/triage, created from origin/triage on first use.
//
// Usage: node scripts/triage-dir.mjs [--pull]   prints the directory
//   --pull   fast-forward the worktree to origin/triage first
// From JS: import {triageDir} from './triage-dir.mjs'.

import fs from 'fs';
import path from 'path';
import {execFileSync} from 'child_process';
import {fileURLToPath} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

export function triageDir({pull = false} = {}) {
  if (process.env.LCSS_TRIAGE_DIR) return path.resolve(process.env.LCSS_TRIAGE_DIR);
  let dir = path.join(ROOT, 'target/triage');
  let git = (...args) => execFileSync('git', args, {cwd: ROOT, stdio: ['ignore', 'pipe', 'inherit'], encoding: 'utf8'});
  if (!fs.existsSync(path.join(dir, 'repros'))) {
    git('fetch', '--quiet', 'origin', 'triage');
    git('worktree', 'prune');
    let hasBranch = git('branch', '--list', 'triage').trim() !== '';
    if (hasBranch) git('worktree', 'add', '--quiet', dir, 'triage');
    else git('worktree', 'add', '--quiet', '-b', 'triage', dir, 'FETCH_HEAD');
    execFileSync('git', ['branch', '--quiet', '--set-upstream-to=origin/triage', 'triage'], {cwd: dir, stdio: 'ignore'});
  } else if (pull) {
    execFileSync('git', ['pull', '--quiet', '--ff-only', 'origin', 'triage'], {cwd: dir, stdio: ['ignore', 'ignore', 'inherit']});
  }
  return dir;
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  console.log(triageDir({pull: process.argv.includes('--pull')}));
}
