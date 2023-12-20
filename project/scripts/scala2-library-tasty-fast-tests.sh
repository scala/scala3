#!/usr/bin/env bash
set -eux

source $(dirname $0)/cmdTestsCommon.inc.sh

"$SBT" 'scala2-library-bootstrapped/compile; scala2-library-tasty/compile; scala2-library-tasty-tests/run; scala2-library-tasty-tests/test; set ThisBuild/Build.scala2Library := Build.Scala2LibraryTasty ;scala3-bootstrapped/testCompilation i5; scala3-bootstrapped/testCompilation tests/run/typelevel-peano.scala; scala3-bootstrapped/testOnly dotty.tools.backend.jvm.DottyBytecodeTests'
