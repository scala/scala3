#!/usr/bin/env bash
find collection-strawman/collections/src/main/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench-bootstrapped/jmh:run" {} + | sbt
