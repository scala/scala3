#!/usr/bin/env bash
set -eux

source $(dirname $0)/cmdTestsCommon.inc.sh

TASTY_FROMAT_FILE="tasty/src/dotty/tools/tasty/TastyFormat.scala"
MINOR_TASTY_VERSION_SUPPORTED_BY_TASTY_MIMA=3
MINOR_TASTY_VERSION=$(grep -oE 'val MinorVersion: Int = ([0-9]+)' $TASTY_FROMAT_FILE | grep -oE '[0-9]+')
EXPERIMENTAL_TASTY_VERSION=$(grep -oE 'val ExperimentalVersion: Int = ([0-9]+)' $TASTY_FROMAT_FILE | grep -oE '[0-9]+')

setTastyVersion() {
  sed -i -E -e "s/val MinorVersion: Int = [0-9]+/val MinorVersion: Int = $1/" -e "s/val ExperimentalVersion: Int = [0-9]+/val ExperimentalVersion: Int = $2/" $TASTY_FROMAT_FILE
}

setTastyVersion $MINOR_TASTY_VERSION_SUPPORTED_BY_TASTY_MIMA 0

# Run scala2-library-bootstrapped/tastyMiMaReportIssues using a custom TASTy version.
# We clean before to make sure all sources are recompiled using the new TASTY version.
# We clean after to make sure no other test will use the TASTy generated with this version.
# We set -Ycheck:all to check that -Ycompile-scala2-library does not gererate inconsistent trees.
"$SBT" 'clean; scala2-library-bootstrapped/clean; reload; set `scala2-library-bootstrapped`/scalacOptions += "-Ycheck:all"; scala2-library-bootstrapped/tastyMiMaReportIssues; clean; scala2-library-bootstrapped/clean'

setTastyVersion $MINOR_TASTY_VERSION $EXPERIMENTAL_TASTY_VERSION
