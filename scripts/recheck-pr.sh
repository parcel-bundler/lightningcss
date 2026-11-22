#!/usr/bin/env bash
# Show which stored issue repros (.claude/triage/repros) a branch or PR would
# change compared with a base (default: current master). Both refs get their
# own worktree and Node binding; the repros and tooling come from this
# checkout. Use it for browser data update PRs ("does this update fix or
# break any triaged issue?") or any other PR.
#
# Usage: scripts/recheck-pr.sh <pr-number|git-ref> [<base-ref>]
# Prints a markdown report; exit 0. Builds take a few minutes each
# (target/review/cargo is shared, so repeated runs are incremental).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
REVIEW="$ROOT/target/review"
export CARGO_TARGET_DIR="$REVIEW/cargo-node"
mkdir -p "$REVIEW"

ref="${1:?usage: scripts/recheck-pr.sh <pr-number|git-ref> [base-ref]}"
base="${2:-origin/master}"
git -C "$ROOT" fetch --quiet origin master
if [[ "$ref" =~ ^[0-9]+$ ]]; then
  git -C "$ROOT" fetch --quiet origin "pull/$ref/head:refs/review/pr-$ref" --force
  ref="refs/review/pr-$ref"
fi

build() { # <commit> -> prints the worktree dir
  local commit dir
  commit="$(git -C "$ROOT" rev-parse "$1")"
  dir="$REVIEW/recheck-${commit:0:12}"
  if [[ ! -d "$dir" ]]; then
    git -C "$ROOT" worktree add --quiet --detach "$dir" "$commit" >&2
  fi
  # Tooling and repros from this checkout, so both sides run the same checks.
  mkdir -p "$dir/scripts" "$dir/.claude"
  cp "$ROOT/scripts/repro.mjs" "$ROOT/scripts/recheck-issues.mjs" "$dir/scripts/"
  rm -rf "$dir/.claude/triage" && cp -r "$ROOT/.claude/triage" "$dir/.claude/triage"
  ln -sfn "$ROOT/node_modules" "$dir/node_modules"
  if [[ "$(cat "$dir/.node-built" 2>/dev/null)" != "$commit" ]]; then
    (cd "$dir" && PATH="$ROOT/node_modules/.bin:$PATH" node scripts/build.js >&2)
    echo "$commit" > "$dir/.node-built"
  fi
  echo "$dir"
}

base_dir="$(build "$base")"
pr_dir="$(build "$ref")"
(cd "$base_dir" && node scripts/recheck-issues.mjs --json) > "$REVIEW/recheck-base.json"
(cd "$pr_dir" && node scripts/recheck-issues.mjs --json) > "$REVIEW/recheck-pr.json"

# Re-run issues whose results differ, twice more on each side, so a
# nondeterministic repro isn't reported as a fix or a break.
changed="$(node -e '
const fs = require("fs");
const a = JSON.parse(fs.readFileSync(process.argv[1], "utf8")).current;
const b = JSON.parse(fs.readFileSync(process.argv[2], "utf8")).current;
const issues = new Set(Object.keys(b).filter(k => (a[k]?.master ?? null) !== b[k].master).map(k => k.split(":")[0]));
console.log([...issues].join(","));
' "$REVIEW/recheck-base.json" "$REVIEW/recheck-pr.json")"
for i in 1 2; do
  if [[ -n "$changed" ]]; then
    (cd "$base_dir" && node scripts/recheck-issues.mjs --issues "$changed" --json) > "$REVIEW/recheck-base-$i.json"
    (cd "$pr_dir" && node scripts/recheck-issues.mjs --issues "$changed" --json) > "$REVIEW/recheck-pr-$i.json"
  else
    echo '{"current":{}}' > "$REVIEW/recheck-base-$i.json"; echo '{"current":{}}' > "$REVIEW/recheck-pr-$i.json"
  fi
done

node - "$REVIEW/recheck-base.json" "$REVIEW/recheck-pr.json" "$ref" "$base" <<'EOF'
const fs = require('fs');
const [baseFile, prFile, ref, base] = process.argv.slice(2);
const load = (f) => JSON.parse(fs.readFileSync(f, 'utf8')).current;
const a = load(baseFile), b = load(prFile);
const reruns = [1, 2].map(i => [load(baseFile.replace('.json', `-${i}.json`)), load(prFile.replace('.json', `-${i}.json`))]);
let fixed = [], broken = [], other = [], flaky = [];
for (const key of Object.keys(b)) {
  const before = a[key]?.master ?? null, after = b[key].master;
  if (before === after) continue;
  // Only report a change that every re-run agrees with.
  if (reruns.some(([ra, rb]) => (ra[key]?.master ?? null) !== before || (rb[key]?.master ?? null) !== after)) {
    flaky.push(key);
    continue;
  }
  if (before === 'WRONG' && after === 'correct') fixed.push(key);
  else if (before === 'correct' && after === 'WRONG') broken.push(key);
  else other.push(`${key}: ${before} -> ${after}`);
}
console.log(`### Stored issue repros: ${ref} vs ${base}\n`);
console.log(`${Object.keys(b).length} repro cases checked.\n`);
if (!fixed.length && !broken.length && !other.length) console.log('No repro changes its result.');
if (flaky.length) console.log(`Ignored as nondeterministic (results differed between runs): ${flaky.map(k => `#${k}`).join(', ')}\n`);
if (fixed.length) console.log(`**Fixed (${fixed.length}):**\n${fixed.map(k => `- #${k}`).join('\n')}\n`);
if (broken.length) console.log(`**Broken (${broken.length}):**\n${broken.map(k => `- #${k}`).join('\n')}\n`);
if (other.length) console.log(`**Other changes:**\n${other.map(k => `- #${k}`).join('\n')}\n`);
EOF
