#!/usr/bin/env bash
find library/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench-bootstrapped/jmh:run -wi 40 -i 30 -- " {} + | sbt
