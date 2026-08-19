#!/usr/bin/env bash

set -euo pipefail

logs_dir="${1:-testlogs}"
logs=()
if [[ -d "$logs_dir" ]]; then
  while IFS= read -r -d '' log; do
    logs+=("$log")
  done < <(find "$logs_dir" -type f -name 'tests-*.log' -print0)
fi

if (( ${#logs[@]} == 0 )); then
  echo "No Vulpix test log was produced."
  exit 0
fi

for i in "${!logs[@]}"; do
  log="${logs[$i]}"
  if [[ -r /proc/sys/kernel/random/uuid ]]; then
    stop_marker="vulpix-$(</proc/sys/kernel/random/uuid)"
  else
    stop_marker="vulpix-$(uuidgen)"
  fi

  printf '::group::Full Vulpix log %d/%d\n' "$((i + 1))" "${#logs[@]}"

  # Test output is untrusted and may contain text that resembles a workflow command.
  printf '::stop-commands::%s\n' "$stop_marker"
  printf 'File: %s\n\n' "$log"
  cat_status=0
  cat -- "$log" || cat_status=$?
  printf '\n::%s::\n' "$stop_marker"

  printf '::endgroup::\n'
  if (( cat_status != 0 )); then
    exit "$cat_status"
  fi
done
