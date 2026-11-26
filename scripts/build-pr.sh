#!/usr/bin/env bash
# Build the Lightning CSS CLI for a pull request and for its merge base, side by
# side, so outputs can be compared with scripts/cases.mjs.
#
# Usage: scripts/build-pr.sh [--on-master] <pr-number>
#        scripts/build-pr.sh [--on-master] <git-ref> [<base-ref>]
#
# Prints lines ready for `eval`:
#   PR_BIN=target/review/<name>/lightningcss
#   BASE_BIN=target/review/base-<sha>/lightningcss
# With --on-master (for PRs that are behind), the PR is merged onto current
# master in a temporary commit and BASE is current master instead:
#   PR_BIN=target/review/<name>-on-<sha>/lightningcss   (fails if it conflicts)
# A worktree of each build is left at target/review/<dir>/src for reading the
# code or running tests. Builds share target/review/cargo so they stay
# incremental; a first build of two trees can take 10+ minutes on a small
# machine, so run it in the background.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
REVIEW="$ROOT/target/review"
mkdir -p "$REVIEW"
export CARGO_TARGET_DIR="$REVIEW/cargo"

on_master=0
if [[ "${1:-}" == "--on-master" ]]; then on_master=1; shift; fi
ref="${1:?usage: scripts/build-pr.sh [--on-master] <pr-number|git-ref> [base-ref]}"

# Serialize git operations: parallel fetches race on .git/shallow.
exec 8>"$REVIEW/git.lock"
flock 8
git fetch --quiet --depth=200 origin master:refs/review/master --force >&2
if [[ "$ref" =~ ^[0-9]+$ ]]; then
  name="pr-$ref"
  git fetch --quiet --depth=200 origin "pull/$ref/head:refs/review/$name" --force >&2
  head="refs/review/$name"
else
  name="ref-$(git rev-parse --short "$ref")"
  head="$ref"
fi

if [[ $on_master == 1 ]]; then
  base_ref="refs/review/master"
  if ! tree="$(git merge-tree --write-tree "$base_ref" "$head")"; then
    echo "error: $head does not merge cleanly onto master" >&2
    exit 1
  fi
  head="$(git commit-tree "$tree" -p "$base_ref" -p "$head" -m "review merge of $name onto master")"
  name="$name-on-$(git rev-parse --short "$base_ref")"
else
  base_ref="${2:-}"
  if [[ -z "$base_ref" ]]; then
    base_ref="$(git merge-base "$head" refs/review/master)" || {
      echo "error: no merge base found; deepen with: git fetch --deepen=500 origin" >&2
      exit 1
    }
  fi
fi
base_sha="$(git rev-parse --short "$base_ref")"
base_commit="$(git rev-parse "$base_ref")"
head_commit="$(git rev-parse "$head")"
flock -u 8

build() { # <commit> <dir>
  local commit="$1" dir="$2"
  if [[ -x "$dir/lightningcss" && "$(cat "$dir/commit" 2>/dev/null)" == "$commit" ]]; then
    return
  fi
  (
    flock 8
    if [[ -d "$dir/src" ]]; then
      git -C "$dir/src" checkout --quiet --detach "$commit" >&2
    else
      mkdir -p "$dir"
      git worktree add --quiet --detach "$dir/src" "$commit" >&2
    fi
  ) 8>"$REVIEW/git.lock"
  # Builds share one target dir; hold a lock so the copied binary is ours.
  (
    flock 9
    (cd "$dir/src" && cargo build --quiet --release --features cli --bin lightningcss >&2)
    cp "$CARGO_TARGET_DIR/release/lightningcss" "$dir/lightningcss"
  ) 9>"$REVIEW/build.lock"
  echo "$commit" > "$dir/commit"
}

build "$base_commit" "$REVIEW/base-$base_sha"
build "$head_commit" "$REVIEW/$name"

echo "PR_BIN=$REVIEW/$name/lightningcss"
echo "BASE_BIN=$REVIEW/base-$base_sha/lightningcss"
