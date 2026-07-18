#!/usr/bin/env bash
# Validate links in scaladoc output with htmlproofer.
#
# Setup (one-time): need Ruby and html-proofer. Example (macOS with Homebrew):
#   brew install ruby
#   gem install html-proofer
#
# Usage:
#   ./project/scripts/validateScaladocLinks.sh [path] [--checkExternalLinks=true|false]
#
#   path  Directory to check (default: scaladoc/output/scala3)
#   --checkExternalLinks  "true" = check external links, "false" = internal only (default: "true")
#
# Examples:
#   ./project/scripts/validateScaladocLinks.sh
#   ./project/scripts/validateScaladocLinks.sh scaladoc/output/scala3
#   ./project/scripts/validateScaladocLinks.sh scaladoc/output/scala3 --checkExternalLinks=false

set -e

CHECK_PATH="scaladoc/output/scala3"
CHECK_EXTERNAL="true"

for arg in "$@"; do
  case "$arg" in
    --checkExternalLinks=*) CHECK_EXTERNAL="${arg#*=}" ;;
    -*) ;;
    *) CHECK_PATH="$arg" ;;
  esac
done

IGNORE_URLS='/^http:\/\/localhost:8080\//'
# Font hosts (root URL returns 404; protocol-relative flagged by htmlproofer)
IGNORE_URLS="${IGNORE_URLS},/fonts.gstatic.com/"
IGNORE_URLS="${IGNORE_URLS},/fonts.googleapis.com/"
IGNORE_URLS="${IGNORE_URLS},/dottydoc.css/"
# Leads to rate limiting errors
IGNORE_URLS="${IGNORE_URLS},/github.com/"
# External sites that return 403/404 or change paths (redirects, moved pages)
IGNORE_URLS="${IGNORE_URLS},/dl.acm.org/"
# nightlyOf links to potentially not yet existing pages
IGNORE_URLS="${IGNORE_URLS},/docs.scala-lang.org\/scala3\/reference\//"

# Status code 0 = timeouts, connection refused, DNS errors; ignore so CI does not flake
IGNORE_STATUS_CODES="0"

# Takes too much time (~30 min) to validate all subdirs in api/scala; only package scala is checked
IGNORE_FILES='/api\/scala\/[^/]+\//'
# Cannot guarantee stability of links for old blog posts
IGNORE_FILES="${IGNORE_FILES},/blog\/(201[5-9]|202[0-4])\//"
IGNORE_FILES="${IGNORE_FILES},/release-notes-0.1.2.html/"
IGNORE_FILES="${IGNORE_FILES},/docs\/release-notes\//"

OPTIONAL_FLAGS=""
if [ "$CHECK_EXTERNAL" != "true" ]; then
  OPTIONAL_FLAGS="--disable-external"
fi

htmlproofer "$CHECK_PATH" \
  --checks "Links" \
  --allow-hash-href \
  --allow-missing-href \
  --no-check-external-hash \
  --check-internal-hash \
  --no-enforce-https \
  --ignore-urls "$IGNORE_URLS" \
  --ignore-files "$IGNORE_FILES" \
  --ignore-status-codes "$IGNORE_STATUS_CODES" \
  --log-level :debug \
  $OPTIONAL_FLAGS
