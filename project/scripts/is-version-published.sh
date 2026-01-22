#!/usr/bin/env bash

# Check whether a specific version of the Scala 3 compiler is published to Maven Central
#
# Usage:
#   is-version-published.sh <version_string>
# e.g.
#   ./is-version-published.sh 3.0.1-RC1-bin-20210413-f3c1468-NIGHTLY
#
# Exit status:
#   zero      if the specified version is published on Maven Central
#   non-zero  otherwise
#
# Notes:
#   Will always say 'not yet published' for versions prior to 3.0.1-RC1-bin-20210413-f3c1468-NIGHTLY
#   since the binary version scheme was changed at that point.

ver=$1
if [[ -z "$ver" ]]; then
  echo "error: missing version parameter"
  echo "usage: $0 <version_string>"
  exit 2
fi

set -eu

# binary version is everything up to the first dot
binaryVersion="${ver%%.*}"

artifactId="scala3-compiler_$binaryVersion"
pom="$artifactId-$ver.pom"

maven_url=https://repo.scala-lang.org/artifactory/maven-nightlies/org/scala-lang/$artifactId/$ver/$pom

echo "Checking whether $ver is published"
echo "at $maven_url"
echo ""

if curl --head --fail -L "$maven_url" ; then
  echo "Version $ver is already published."
  exit 0
else
  echo "Version $ver is not yet published."
  exit 10
fi
