#!/usr/bin/env bash
find compiler/src/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench/jmh:run" {} + | sbt
