#!/usr/bin/env bash
find library/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "scala3-bench-bootstrapped/jmh:run 0 1 10" {} + | sbt
