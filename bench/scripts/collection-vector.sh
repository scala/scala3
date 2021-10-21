#!/usr/bin/env bash
sbt "dotty-bench-bootstrapped/jmh:run -wi 40 -i 40 -f 3 -- bench/tests/Vector.scala"
