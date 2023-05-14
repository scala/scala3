#!/usr/bin/env bash
find library/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench-bootstrapped/jmh:run 40 30" {} + | sbt
