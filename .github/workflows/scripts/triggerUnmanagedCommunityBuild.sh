#!/usr/bin/env bash

# This is script for triggering unamanged community build upon releasing nightly version.
# Script sends request to CB Jenkins instance to start the build for given released Scala version
# Prints url of created job to stdout
#
# Requirement:
#   - the latest (nightly) version of scala should be published

set -u

if [ $# -ne 2 ]; then
  echo "Wrong number of script arguments, expected <token> <scala-version>, got $#: $@"
  exit 1
fi

CB_ENDPOINT=https://scala3.westeurope.cloudapp.azure.com
CB_BUILD_TOKEN="$1"
SCALA_VERSION="$2"

startRunResponse=$(curl "${CB_ENDPOINT}/job/runBuild/buildWithParameters?token=${CB_BUILD_TOKEN}&publishedScalaVersion=${SCALA_VERSION}" -v 2>&1)
echo "${startRunResponse}"
queueItem=$(echo "${startRunResponse}" | grep -oP "< Location: \K[\w\d:/.//]+")
# Wait until Jenkins does acknowledge the build (max 1 min )
for i in {1..12}; do
  buildUrl=$(curl -s "${queueItem}/api/json?tree=executable[url]" | jq .executable.url)
  if [[ "null" == "${buildUrl}" ]]; then
    echo "Waiting for build start..."
    sleep 5
  else
    echo "Created build url: ${buildUrl}"
    exit 0
  fi
done

# Set error if failed to resolve build url
exit 1
