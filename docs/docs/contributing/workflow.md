---
layout: doc-page
title: Workflow
---

Check [Getting Started](getting-started.md) for instructions on how to obtain the source code of dotty.
This document details common workflow patterns when working with Dotty.

## Compiling files with scalac ##

As we have seen you can compile a test file either from sbt:

```bash
$ sbt
> scalac <OPTIONS> <FILE>
```

or from terminal:

```bash
$ scalac <OPTIONS> <FILE>
```

Here are some useful debugging `<OPTIONS>`:

* `-Xprint:PHASE1,PHASE2,...` or `-Xprint:all`: prints the `AST` after each
  specified phase. Phase names can be found by examining the
  `dotty.tools.dotc.transform.*` classes for their `phaseName` field e.g., `-Xprint:erasure`.
  You can discover all phases in the `dotty.tools.dotc.Compiler` class
* `-Ylog:PHASE1,PHASE2,...` or `-Ylog:all`: enables `ctx.log("")` logging for
  the specified phase.
* `-Ycheck:all` verifies the consistency of `AST` nodes between phases, in
  particular checks that types do not change. Some phases currently can't be
  `Ycheck`ed, therefore in the tests we run:
  `-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef`.
* the last frontier of debugging (before actual debugging) is the range of logging capabilities that
can be enabled through the `dotty.tools.dotc.config.Printers` object. Change any of the desired printer from `noPrinter` to
`default` and this will give you the full logging capability of the compiler.

## Inspecting Trees with Type Stealer ##

You can inspect types with the type stealer, open `compiler/test/dotty/tools/DottyTypeStealer.scala` and you'll see:

```scala
@main def steal() = {
  val s = DottyTypeStealer.stealType("class O { type X }", "O#X")
  val t = s._2(0)
  println(t)
}
```

```bash
$ sbt
> scala3-compiler-bootstrapped/Test/runMain dotty.tools.steal
TypeRef(TypeRef(ThisType(TypeRef(NoPrefix,module class <empty>)),class O),type X)
```

You can inspect other value types by editing the arguments of `stealType`.

## Pretty-printing ##
Many objects in the scalac compiler implement a `Showable` trait (e.g. `Tree`,
`Symbol`, `Type`). These objects may be prettyprinted using the `.show`
method

## SBT Commands Cheat Sheet ##
The basics of working with Dotty codebase are documented [here](https://dotty.epfl.ch/docs/contributing/getting-started.html) and [here](https://dotty.epfl.ch/docs/contributing/workflow.html). Below is a cheat sheet of some frequently used commands (to be used from SBT console – `sbt`).


|                        Command                       |                                                          Description                                                          |
|------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `scalac ../issues/Playground.scala`                  | Compile the given file – path relative to the Dotty directory. Output the compiled class files to the Dotty directory itself. |
| `scala Playground`                                   | Run the compiled class `Playground`. Dotty directory is on classpath by default.                                              |
| `repl`                                               | Start REPL                                                                                                                    |
| `testOnly dotty.tools.dotc.CompilationTests -- *pos` | Run test (method) `pos` from `CompilationTests` suite.                                                                        |
| `testCompilation sample`                             | In all test suites, run test files containing the word `sample` in their title.                                               |
