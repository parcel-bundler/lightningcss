#!/usr/bin/env bash
# Compare minified output size (and time) of two Lightning CSS builds on the
# synthetic corpus from scripts/size-corpus.py, the pinned real-world corpus
# from scripts/real-corpus.json (fetched from npm and cached), and any extra
# CSS files given.
#
# Usage: scripts/size-check.sh [--no-real] BASE_BIN NEW_BIN [extra.css...] [-- cli args]
# By default every file runs in two configs: `--minify` (no targets) and
# `--minify` with older targets (chrome 100, safari 15, firefox 100), since
# lowering and fallbacks only happen with targets. `-- cli args` replaces both
# with a single config. Prints a markdown table per config. Times are the best of 3
# runs on this machine; treat differences under ~10% as noise.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
real=1
if [[ "${1:-}" == "--no-real" ]]; then real=0; shift; fi
base="$1"; new="$2"; shift 2
files=(); custom=()
while [[ $# -gt 0 ]]; do
  if [[ "$1" == "--" ]]; then shift; custom=("$@"); break; fi
  files+=("$1"); shift
done
corpus="$ROOT/target/review/size-corpus"
python3 "$ROOT/scripts/size-corpus.py" --all "$corpus"
files=("$corpus"/*.css "${files[@]}")
if [[ $real == 1 ]]; then
  mapfile -t realfiles < <(node "$ROOT/scripts/fetch-corpus.mjs")
  files+=("${realfiles[@]}")
fi
best_ms() { # <bin> <file>: best of 3 wall-clock runs in ms
  local best=999999 t0 t
  for _ in 1 2 3; do
    t0=$(date +%s%N)
    "$1" "${args[@]}" "$2" > /dev/null 2>&1 || true
    t=$(( ($(date +%s%N) - t0) / 1000000 ))
    (( t < best )) && best=$t
  done
  echo "$best"
}
if [[ ${#custom[@]} -gt 0 ]]; then
  configs=("$(IFS='|'; echo "${custom[*]}")")
else
  # Arguments separated by '|' so target queries can contain spaces.
  configs=("--minify" "--minify|--targets|chrome 100|--targets|safari 15|--targets|firefox 100")
fi
for config in "${configs[@]}"; do
IFS='|' read -r -a args <<< "$config"
echo "**\`${args[*]}\`**"
echo
echo "| input | input bytes | base | new | delta | base gz | new gz | base ms | new ms |"
echo "|---|---|---|---|---|---|---|---|---|"
for f in "${files[@]}"; do
  bo="$("$base" "${args[@]}" "$f" 2>/dev/null)" || { echo "| $(basename "$f") | base build failed |"; continue; }
  no="$("$new" "${args[@]}" "$f" 2>/dev/null)" || { echo "| $(basename "$f") | new build failed |"; continue; }
  b=${#bo}; n=${#no}
  bg="$(printf %s "$bo" | gzip -9 | wc -c)"; ng="$(printf %s "$no" | gzip -9 | wc -c)"
  delta=$(( n - b )); pct=$(awk "BEGIN{printf \"%+.1f%%\", ($b ? 100*$delta/$b : 0)}")
  echo "| $(basename "$f") | $(wc -c < "$f") | $b | $n | $delta ($pct) | $bg | $ng | $(best_ms "$base" "$f") | $(best_ms "$new" "$f") |"
done
echo
done
