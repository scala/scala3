#!/usr/bin/env bash

# Usage:
# RELEASE_DIR=<directory to place fresh clones of projects> ./release.sh

set -e

# TODO: clone all the projects
    # cd $RELEASE_DIR
    # git@github.com:lampepfl/dotty
    # git clone git@github.com:lampepfl/dotty-example-project.git
    # git clone --single-branch --branch mill git@github.com:lampepfl/dotty-example-project.git
    # git clone git@github.com:lampepfl/dotty.g8.git
    # git clone git@github.com:lampepfl/dotty-cross.g8.git
    # git clone git@github.com:lampepfl/homebrew-brew.git
    # git clone git@github.com:lampepfl/packtest.git
    # git clone git@github.com:scalacenter/scastie/.git

# Parsing latest release from the list of tags not ending in RCX
version=($(git tag --list '*.0' --sort=v:refname | tail -n1 | tr '.' '\n'))

major=${version[0]}
minor=${version[1]}
build=${version[2]}

LAST_RELEASE="$major.$minor"

RELEASE="$major.$((minor+1))"

RC_RELEASE="0.$((minor+2))"

V_NEXT="0.$((minor+3))"

echo "cd $RELEASE_DIR/dotty"

echo "git checkout $RELEASE.x"

echo "sed -i '' 's/^\([[:blank:]]*\)val baseVersion = \"$RELEASE.0-RC1\".*/\1val baseVersion = \"$RELEASE.0\"/' project/Build.scala"

echo "git commit -am 'Release Dotty $RELEASE.0'"

echo "git tag $RELEASE.0"

echo "git push --follow-tags --set-upstream origin $RELEASE"

echo "git checkout master"

echo "git merge $RELEASE.x"

echo "git push"

echo "git checkout -b $RC_RELEASE.x"

echo "sed -i '' 's/^\([[:blank:]]*\)val baseVersion = \"$RELEASE.0\".*/\1val baseVersion = \"$RC_RELEASE.0-RC1\"/' project/Build.scala"

echo "git commit -am 'Release Dotty $RC_RELEASE.0-RC1'"

echo "git tag $RC_RELEASE.0-RC1"

echo "git push --follow-tags --set-upstream origin $RC_RELEASE.x"

echo "git checkout master"

echo "sed -i '' 's/^\([[:blank:]]*\)val baseVersion = \"$RC_RELEASE.0\".*/\1val baseVersion = \"$V_NEXT.0\"/' project/Build.scala"

echo "git commit -am 'Set baseVersion to $V_NEXT.0'"

echo "git push"

# TODO: perform all the version bumps, commit, push and create the PRs