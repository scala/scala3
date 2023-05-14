set -eux

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" >& /dev/null && pwd)/../.."

SBT="$ROOT/project/scripts/sbt" # if run on CI
# SBT="sbt" # if run locally

SOURCE="tests/pos/HelloWorld.scala"
MAIN="HelloWorld"
TASTY="HelloWorld.tasty"
EXPECTED_OUTPUT="hello world"

OUT=$(mktemp -d)
OUT1=$(mktemp -d)
tmp=$(mktemp)

die () {
    echo >&2 "$@"
    exit 1
}

clear_out()
{
  local out="$1"
  rm -rf "$out"/*
}
