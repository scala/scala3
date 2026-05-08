#!/usr/bin/env bash
# Run `sbt --client <command>` and assert its output contains <expected>.
# Usage: sbt-client-check.sh <command> <expected>
set -uo pipefail

COMMAND="$1"
EXPECTED="$2"
LOG=$(mktemp)

sbt -no-colors --client "$COMMAND" 2>&1 | tee "$LOG" || true

if grep -Faq "$EXPECTED" "$LOG"; then
  echo "PASS: sbt --client \"$COMMAND\" works"
else
  echo "FAIL: sbt --client \"$COMMAND\" did not produce expected output (looking for: $EXPECTED)"
  exit 1
fi
