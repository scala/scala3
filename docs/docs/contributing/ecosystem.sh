#!/usr/bin/env bash

#object Predef:
  set -ex

  stable=$1
  live=$2  # Push changes to github if it is defined

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

  function push {
    if [ ! -z $live ]; then
      git push $@
    else
      echo "Dry run, not pushing to github"
    fi
  }

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
    push
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
      sbt run
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
      mill root.run
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
      sbt new file://./dotty.g8 --name=foo --description=bar && cd foo && sbt run
    }

    function publish_dotty.g8 {
      cd "../$SELF"
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
      sbt new file://./dotty-cross.g8 --name=foo --description=bar && cd foo && sbt run
    }

    function publish_dotty-cross.g8 {
      cd "../$SELF"
      publish super
    }

  #trait Homebrew extends Project:
    RC_PATTERN="[0-9\.RC\-]+"

    function update_homebrew-brew {
      local hash=$(curl -L -s https://github.com/lampepfl/dotty/releases/download/0.18.1-RC1/sha256sum.txt | grep ".tar.gz" | awk '{ print $1 }')

      replace "dotty.rb" \
        "url\s+\"https:\/\/github\.com\/lampepfl\/dotty\/releases\/download\/$RC_PATTERN\/dotty-$RC_PATTERN\.tar\.gz\"" \
        "url \"https:\/\/github.com\/lampepfl\/dotty\/releases\/download\/$rc_version\/dotty-$rc_version.tar.gz\""

      replace "dotty.rb" \
        "sha256 \"[0-9a-z]+\"" \
        "sha256 \"$hash\""
    }

  #class Packtest extends Project with Homebrew:  // RC_PATTERN comes from Homebrew
    function update_packtest {
      replace "artifacts" \
        "zip=https:\/\/github.com\/lampepfl\/dotty\/releases\/download\/$RC_PATTERN\/dotty-$RC_PATTERN.zip" \
        "zip=https:\/\/github.com\/lampepfl\/dotty\/releases\/download\/$rc_version\/dotty-$rc_version.zip"

      replace "artifacts" \
        "tar=https:\/\/github.com\/lampepfl\/dotty\/releases\/download\/$RC_PATTERN\/dotty-$RC_PATTERN.tar.gz" \
        "tar=https:\/\/github.com\/lampepfl\/dotty\/releases\/download\/$rc_version\/dotty-$rc_version.tar.gz"
    }

  #class Scastie extends Project:
    function deploy_scastie {
      git clone https://github.com/scalacenter/scastie.git
      cd scastie
      git remote add staging https://github.com/dotty-staging/scastie
      git checkout -b "dotty-release-$rc_version"
    }

    function update_scastie {
      replace "project/SbtShared.scala" \
        "val\s+latestDotty\s*=\s*\".*\"" \
        "val latestDotty = \"$rc_version\""
    }

    function publish_scastie {
      git commit -am "Upgrade Dotty to $rc_version"
      push -u staging
    }

  #class Scalac extends Project:
    function deploy_scalac {
      git clone https://github.com/scala/scala.git
      cd scala
      git remote add staging https://github.com/dotty-staging/scala
      git checkout -b "dotty-release-$rc_version"
    }

    function update_scalac {
      replace "project/DottySupport.scala" \
        "val\s+dottyVersion\s*=\s*\".*\"" \
        "val dottyVersion = \"$rc_version\""
    }

    function publish_scalac {
      git commit -am "Upgrade Dotty to $rc_version"
      push -u staging
    }

#object Main:
  PROJECTS='
  dotty-example-project
  dotty-example-project-mill
  dotty.g8
  dotty-cross.g8
  homebrew-brew
  packtest
  scastie
  scalac'

  function main {
    export -f process
    for p_raw in $PROJECTS; do
      p=$(echo $p_raw | xargs)
      process $p
    done
  }

main
