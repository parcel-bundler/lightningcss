#!/usr/bin/env bash
# Build the Lightning CSS CLI for a pull request and for its merge base, side by
# side, so outputs can be compared with scripts/cases.mjs.
#
# Usage: scripts/build-pr.sh <pr-number>
#        scripts/build-pr.sh <git-ref> [<base-ref>]
#
# Prints two lines, ready for `eval`:
#   PR_BIN=target/review/<name>/lightningcss
#   BASE_BIN=target/review/base-<sha>/lightningcss
# and leaves a worktree of the PR at target/review/<name>/src for reading or
# running its tests. Builds share target/review/cargo so they stay incremental.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
REVIEW="$ROOT/target/review"
mkdir -p "$REVIEW"
export CARGO_TARGET_DIR="$REVIEW/cargo"

ref="${1:?usage: scripts/build-pr.sh <pr-number|git-ref> [base-ref]}"
if [[ "$ref" =~ ^[0-9]+$ ]]; then
  name="pr-$ref"
  git fetch --quiet --depth=100 origin "pull/$ref/head:refs/review/$name" --force >&2
  head="refs/review/$name"
else
  name="ref-$(git rev-parse --short "$ref")"
  head="$ref"
fi

base_ref="${2:-}"
if [[ -z "$base_ref" ]]; then
  git fetch --quiet --depth=100 origin master >&2 || true
  base_ref="$(git merge-base "$head" FETCH_HEAD 2>/dev/null || git merge-base "$head" origin/master)"
fi
base_sha="$(git rev-parse --short "$base_ref")"

build() { # <commit> <dir>
  local commit="$1" dir="$2"
  if [[ -x "$dir/lightningcss" && "$(cat "$dir/commit" 2>/dev/null)" == "$(git rev-parse "$commit")" ]]; then
    return
  fi
  if [[ -d "$dir/src" ]]; then
    git -C "$dir/src" checkout --quiet --detach "$commit" >&2
  else
    mkdir -p "$dir"
    git worktree add --quiet --detach "$dir/src" "$commit" >&2
  fi
  # Builds share one target dir; hold a lock so the copied binary is ours.
  (
    flock 9
    (cd "$dir/src" && cargo build --quiet --release --features cli --bin lightningcss >&2)
    cp "$CARGO_TARGET_DIR/release/lightningcss" "$dir/lightningcss"
  ) 9>"$REVIEW/build.lock"
  git rev-parse "$commit" > "$dir/commit"
}

build "$base_ref" "$REVIEW/base-$base_sha"
build "$head" "$REVIEW/$name"

echo "PR_BIN=$REVIEW/$name/lightningcss"
echo "BASE_BIN=$REVIEW/base-$base_sha/lightningcss"
