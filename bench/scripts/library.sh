#!/usr/bin/env bash
find collection-strawman/src/main/ -type f \( -name "*.scala" -or -name "*.java" \) -exec echo "dotty-bench/jmh:run" {} + | sbt
