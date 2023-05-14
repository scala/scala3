#!/usr/bin/env bash
find compiler/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench-bootstrapped/jmh:run 0 1 10" {} + | sbt
