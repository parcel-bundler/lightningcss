#!/usr/bin/env bash
# Compare minified output size of two Lightning CSS builds on the synthetic
# corpus from scripts/size-corpus.py plus any extra CSS files given.
#
# Usage: scripts/size-check.sh BASE_BIN NEW_BIN [extra.css...] [-- cli args]
# Default CLI args: --minify. Prints a markdown table (bytes and gzip bytes).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
base="$1"; new="$2"; shift 2
files=(); args=(--minify)
while [[ $# -gt 0 ]]; do
  if [[ "$1" == "--" ]]; then shift; args=("$@"); break; fi
  files+=("$1"); shift
done
corpus="$ROOT/target/review/size-corpus"
python3 "$ROOT/scripts/size-corpus.py" --all "$corpus"
files=("$corpus"/*.css "${files[@]}")
echo "| input | input bytes | base | new | delta | base gz | new gz |"
echo "|---|---|---|---|---|---|---|"
for f in "${files[@]}"; do
  b="$("$base" "${args[@]}" "$f" | wc -c)"; n="$("$new" "${args[@]}" "$f" | wc -c)"
  bg="$("$base" "${args[@]}" "$f" | gzip -9 | wc -c)"; ng="$("$new" "${args[@]}" "$f" | gzip -9 | wc -c)"
  delta=$(( n - b )); pct=$(awk "BEGIN{printf \"%+.1f%%\", ($b ? 100*$delta/$b : 0)}")
  echo "| $(basename "$f") | $(wc -c < "$f") | $b | $n | $delta ($pct) | $bg | $ng |"
done
