#!/usr/bin/env bash
find compiler/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench-bootstrapped/jmh:run -wi 5 -i 10 -- " {} + | sbt
