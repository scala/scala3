#!/usr/bin/env bash
sbt "dotty-bench-bootstrapped/jmh:run 40 40 3 bench/tests/Vector.scala"
