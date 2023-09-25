#!/usr/bin/env bash
sbt "scala3-bench-bootstrapped/jmh:run 40 40 3 bench/tests/Vector.scala"
