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

#package object util:
  function replace {
    local where="$1"
    local what="$2"
    local with_what="$3"
    gsed -i -E "s/$what/$with_what/g" "$where"
  }

  function isDefined { type $1 &> /dev/null; }
  function notDefined { ! isDefined $1; }

#class Project:
  function project_call {
    local NAME=$1
    local SUPER_CALL=$2
    local FUNCTION="$NAME"_"$SELF"
    local PARENT_FUNCTION="$NAME"_"project"

    if $(notDefined $FUNCTION) || [ ! -z $SUPER_CALL ]; then
      $PARENT_FUNCTION
    else
      $FUNCTION
    fi
  }

  function deploy { project_call "deploy" $1; }
  function deploy_project {
    git clone https://github.com/lampepfl/$SELF.git
    cd $SELF
  }

  function update { project_call "update" $1; }
  function update_project {
    echo "Not implemented error: update"
    exit 1
  }

  function test {
    local FUNCTION="test_$SELF"
    local SUPER_CALL="$1"
    if $(isDefined $FUNCTION); then
      if ! $FUNCTION; then
        echo "Test failed for $SELF"
        exit 1
      fi
    else
      echo "No test available for $SELF"
    fi
  }

  function publish { project_call "publish" $1; }
  function publish_project {
    git commit -am "Upgrade Dotty to $rc_version"
  }

  function cleanup { project_call "cleanup" $1; }
  function cleanup_project {
    cd ..
    rm -rf $SELF
  }

  function process {
    SELF=$1

    deploy
    update
    test
    publish
    cleanup
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

    function cleanup_dotty-example-project-mill {
      cd ..
      rm -rf dotty-example-project
    }

  #class DottyG8 extends Project:
    function deploy_dotty.g8 {
      git clone https://github.com/lampepfl/dotty.g8
    }

    function update_dotty.g8 {
      local what="val\s+dottyVersion\s*=\s*\".*\""
      local with_what="val dottyVersion = \"$rc_version\""

      replace "dotty.g8/src/main/g8/build.sbt" "$what" "$with_what"
    }

    function test_dotty.g8 {
      echo 'sbt new file://./dotty.g8 --name=foo --description=bar && cd foo && sbt run'  # TODO unecho
    }

    function publish_dotty.g8 {
      cd "$SELF"
      publish super
    }

  #class DottyG8Cross extends Project:
    function deploy_dotty-cross.g8 {
      git clone https://github.com/lampepfl/dotty-cross.g8.git
    }

    function update_dotty-cross.g8 {
      local what="val\s+dottyVersion\s*=\s*\".*\""
      local with_what="val dottyVersion = \"$rc_version\""

      replace "dotty-cross.g8/src/main/g8/build.sbt" "$what" "$with_what"
    }

    function test_dotty-cross.g8 {
      echo 'sbt new file://./dotty-cross.g8 --name=foo --description=bar && cd foo && sbt run'  # TODO unecho
    }

  #class Homebrew extends Project:
    function update_homebrew-brew {
      local hash=$(curl -L -s https://github.com/lampepfl/dotty/releases/download/0.18.1-RC1/sha256sum.txt | grep ".tar.gz" | awk '{ print $1 }')

      replace "dotty.rb" \
        "url\s*\"https://github.com/lampepfl/dotty/releases/download/[\d\.\-RC]+/dotty-[\d\.\-RC]+.tar.gz\"" \
        "url \"https://github.com/lampepfl/dotty/releases/download/$rc_version/dotty-$rc_version.tar.gz\""

      replace "dotty.rb" \
        "sha256 \"[0-9a-z]+\"" \
        "sha256 \"$hash\""
    }

#object Main:
  PROJECTS='
  dotty-example-project
  '
  # dotty-example-project-mill
  # dotty.g8
  # dotty-cross.g8
  # homebrew-brew

  function main {
    export -f process
    for p_raw in $PROJECTS; do
      p=$(echo $p_raw | xargs)
      process $p
    done
  }

main
