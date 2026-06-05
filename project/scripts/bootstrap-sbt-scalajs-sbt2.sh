#!/usr/bin/env bash
# Bootstrap org.scala-js sbt-scalajs for sbt 2 until 1.22.x is on Maven Central.
#
# Prerequisites: git, sbt 1.x (for building scala-js), JDK 21+
#
# Usage: ./project/scripts/bootstrap-sbt-scalajs-sbt2.sh [scala-js-repo-dir]

set -euo pipefail

SCALA_JS_DIR="${1:-$(mktemp -d /tmp/scala-js-sbt2.XXXXXX)}"
VERSION="1.22.0-SNAPSHOT"
SBT2_BRANCH="${SBT2_BRANCH:-main}"

if [[ ! -d "$SCALA_JS_DIR/.git" ]]; then
  git clone --depth 1 --branch "$SBT2_BRANCH" https://github.com/scala-js/scala-js.git "$SCALA_JS_DIR"
fi

cd "$SCALA_JS_DIR"

# sbt-scalajs sbt2 artifact (requires scala-js tree with sbt 2 support merged)
sbt -Dsbt.supershell=false \
  "set ThisBuild / version := \"$VERSION\"" \
  "+sbtPlugin/publishLocal"

# scalajs-ir sources consumed by scala3-compiler-* (CrossVersion.for3Use2_13)
sbt -Dsbt.supershell=false \
  "set ThisBuild / version := \"$VERSION\"" \
  "ir2_13/publishLocal"

echo "Published org.scala-js artifacts at version $VERSION to ~/.ivy2/local"
