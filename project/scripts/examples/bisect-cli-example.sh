#!/usr/bin/env bash

# Don't use this example script modified in place as it might disappear from the repo during a checkout.
# Instead copy it to a different location first.

scala-cli compile -S "$1" --server=false file1.scala file2.scala
