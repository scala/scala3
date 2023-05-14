#!/usr/bin/env bash
find compiler/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench-bootstrapped/jmh:run 5 10" {} + | sbt
