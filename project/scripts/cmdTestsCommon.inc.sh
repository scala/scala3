set -eux

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" >& /dev/null && pwd)/../.."

SBT="$ROOT/project/scripts/sbt" # if run on CI
# SBT="sbt" # if run locally

SOURCE="tests/pos/HelloWorld.scala"
MAIN="HelloWorld"
TASTY="HelloWorld.tasty"
EXPECTED_OUTPUT="hello world"
EXPECTED_OUTPUT_ARGS="[0:abc],[1:true],[2:123]"

OUT=$(mktemp -d)
OUT1=$(mktemp -d)
tmp=$(mktemp)

# set the $DIST_PROJECT and $DIST_DIR variables
source "$ROOT/bin/common-platform"

die () {
    echo >&2 "$@"
    exit 1
}

clear_out()
{
  local out="$1"
  rm -rf "$out"/*
}

clear_cli_dotfiles()
{
  local out="$1"
  rm -rf "$out"/.bsp
  rm -rf "$out"/.scala-build

  rm -f "$ROOT"/.bsp/scala.json
  if [ -z "$(ls -A "$ROOT"/.bsp)" ]; then
    rm -rf "$ROOT"/.bsp
  fi
  rm -rf "$ROOT"/.scala-build
}
