#!/usr/bin/env bash

set -uo pipefail

if (( $# == 0 )); then
  echo "usage: $0 command [arg ...]" >&2
  exit 2
fi

start_marker="__VULPIX_START_${BASHPID}_${RANDOM}_${RANDOM}__"
printf '::group::sbt startup and compiler build\n'

VULPIX_CI_START_MARKER="$start_marker" "$@" 2>&1 | {
  group_open=true
  while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ "$line" == "$start_marker" ]]; then
      if [[ "$group_open" == true ]]; then
        printf '::endgroup::\n'
        group_open=false
      fi
    else
      printf '%s\n' "$line"
    fi
  done

  if [[ "$group_open" == true ]]; then
    printf '::endgroup::\n'
  fi
}
pipeline_status=("${PIPESTATUS[@]}")

if (( pipeline_status[0] != 0 )); then
  exit "${pipeline_status[0]}"
fi
exit "${pipeline_status[1]}"
