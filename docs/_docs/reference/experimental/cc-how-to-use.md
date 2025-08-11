---
layout: doc-page
title: "How to Use the Capture Checker"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/cc-how-to-use.html
---

## Enabling Capture Checking

TODO

## Compilation Options

The following options are relevant for capture checking.

 - **-Vprint:cc** Prints the program with capturing types as inferred by capture checking.
 - **-Ycc-verbose** Prints capabilities and capturing types in more detail.
 - **-Ycc-debug** Gives more detailed, implementation-oriented information about capture checking, as described in the next section.

 The implementation supporting capture checking with these options is currently in branch `cc-experiment` on dotty.epfl.ch.
