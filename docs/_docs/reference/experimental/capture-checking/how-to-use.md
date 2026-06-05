---
layout: doc-page
title: "How to Use the Capture Checker"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/how-to-use.html
---

## Enabling Capture Checking

Use Scala 3 nightly for the latest features and fixes.

Add this import in any file that uses capture checking:
```scala sc:nocompile
import language.experimental.captureChecking
```

### Separation Checking

Requires the import above:
```scala sc:nocompile
import language.experimental.captureChecking
import language.experimental.separationChecking
```

### SBT Project Template

Alternatively, you can clone a pre-defined SBT project to get
started: https://github.com/lampepfl/scala3-cc-template

### The REPL/Scala-CLI

Using the command line through explicit parameters:
```bash
scala -S 3.nightly -language:experimental.captureChecking
```
or when reading from a file:
```scala sc:nocompile
// foo.scala:
//> using scala 3.nightly
import language.experimental.captureChecking
...
```
Then, it suffices to run
```bash
scala foo.scala
```

## API Documentation

Scaladoc supports capture checking. The nightly standard library API docs have it enabled by default:
https://nightly.scala-lang.org/api/

## Compilation Options

The following options are relevant for capture checking.

 - **-Vprint:cc** Prints the program with capturing types as inferred by capture checking.
 - **-Ycc-verbose** Prints capabilities and capturing types in more detail.
 - **-Ycc-debug** Gives more detailed, implementation-oriented information about capture checking, as described in the next section.