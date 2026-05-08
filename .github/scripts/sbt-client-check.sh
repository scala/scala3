#!/usr/bin/env bash
# Run `sbt --client <command>` and assert its output contains <expected>.
# Usage: sbt-client-check.sh <command> <expected>
set -uo pipefail

COMMAND="$1"
EXPECTED="$2"

OUTPUT=$(sbt -no-colors --client "$COMMAND" 2>&1 || true)
if printf '%s\n' "$OUTPUT" | grep -Faq "$EXPECTED"; then
  echo "PASS: sbt --client \"$COMMAND\" works"
else
  echo "FAIL: sbt --client \"$COMMAND\" did not produce expected output (looking for: $EXPECTED)"
  echo "--- OUTPUT BEGIN ---"
  echo "$OUTPUT"
  echo "--- OUTPUT END ---"
  exit 1
fi
