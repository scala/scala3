#!/usr/bin/env bash
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

function set_version {
  version="$1"
  gsed -i -E "s/val\s*baseVersion\s*=\s*\".+\"/val baseVersion = \"$version\"/g" ./project/Build.scala
}

function release {
  version="$1"
  set_version "$1"
  git commit -am "Release Dotty $version"
  git tag "$version"
}

function reset {
  git branch -D "$rc_branch"
  git tag -d "$rc_version" "$stable_version"
  git checkout "$stable_branch"
  git reset --hard "origin/$stable_branch"
  git checkout master
  git reset --hard origin/master
}

function main {
  # On branch 0.17.x, set baseVersion to 0.17.0 and git tag it as 0.17.0
  git checkout "$stable_branch"
  release "$stable_version"

  # Create branch 0.18.x from master, set baseVersion to 0.18.1-RC1 and git tag it as 0.18.1-RC1
  git checkout master
  git checkout -b "$rc_branch"
  release "$rc_version"

  # On master, set baseVersion to 0.19.1
  git checkout master
  set_version "$next_version"
  git commit -am "Set baseVersion to $next_version"

  # Push
  git push --atomic --tags origin master "$stable_branch" "$rc_branch"
}

main
