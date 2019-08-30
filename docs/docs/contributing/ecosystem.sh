#!/usr/bin/env bash

#object Predef:
  set -ex

  stable=$1
  stable_patch=0
  rc_patch=1
  next_patch=1

  rc="$(($stable+1))"
  next="$(($rc+1))"

  stable_version="0.$stable.$stable_patch"
  rc_version_preview="0.$rc.$rc_patch"
  rc_version="$rc_version_preview-RC1"
  next_version="0.$next.$next_patch"
  stable_branch="0.$stable.x"
  rc_branch="0.$rc.x"

  if [ -z "$(which gsed)" ]
  then
    echo "Please install gsed to use this script – see e.g. https://stackoverflow.com/q/30003570"
    exit 1
  fi

#class Project:
  function deploy {
    local FUNCTION="deploy_$SELF"
    if type $FUNCTION &>/dev/null; then $FUNCTION; else
      git clone https://github.com/lampepfl/$SELF.git
      cd $SELF
    fi
  }

  function update {
    local FUNCTION="update_$SELF"
    $FUNCTION
  }

  function test {
    local FUNCTION="test_$SELF"
    if type $FUNCTION &>/dev/null; then
      if ! $FUNCTION; then
        echo "Test failed for $SELF"
        exit 1
      fi
    else
      echo "No test available for $SELF"
    fi
  }

  function publish {
    git commit -am "Upgrade Dotty to $rc_version"
    # git push  # TODO uncomment
  }

  function cleanup {
    local FUNCTION="cleanup_$SELF"
    if type $FUNCTION &>/dev/null; then $FUNCTION; else
      cd ..
      rm -rf $SELF
    fi
  }

  function process {
    SELF=$1

    deploy
    update
    test
    publish
    cleanup
  }

#package object util:
  function replace {
    local where="$1"
    local what="$2"
    local with_what="$3"
    gsed -i -E "s/$what/$with_what/g" "$where"
  }

#package object impl:
  #class DottyExample extends Project:
    function update_dotty-example-project {
      local what="scalaVersion\s*:=\s*\".*\""
      local with_what="scalaVersion := \"$rc_version\""

      replace "README.md" "$what" "$with_what"
      replace "build.sbt" "$what" "$with_what"
    }

    function test_dotty-example-project {
      echo sbt run  # TODO un-echo
    }

  #class DottyExampleMill extends Project:
    function deploy_dotty-example-project-mill {
      git clone https://github.com/lampepfl/dotty-example-project
      cd dotty-example-project
      git checkout mill
    }

    function update_dotty-example-project-mill {
      local what="def\s+scalaVersion\s*=\s*\".*\""
      local with_what="def scalaVersion = \"$rc_version\""

      replace "build.sc" "$what" "$with_what"
      replace "README.md" "$what" "$with_what"
    }

    function test_dotty-example-project-mill {
      echo mill root.run # TODO un-echo
    }

#object Main:
  PROJECTS='
  dotty-example-project
  dotty-example-project-mill
  '
  function main {
    export -f process
    for p_raw in $PROJECTS; do
      p=$(echo $p_raw | xargs)
      process $p
    done
  }

main
