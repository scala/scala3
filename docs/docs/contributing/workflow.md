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

## Inspecting Types with Type Stealer ##

You can inspect types with the main method `dotty.tools.printTypes` from the sbt shell,
passing at least three arguments:
- The first argument is a string that introduces some
Scala definitions
- The second argument introduces how the the remaining arguments should be interpreted,
comprising of
  - `rhs` - the return type of a definition
  - `class` - the signature of a class, after its name
  - `method` - the signature of a method, after its name
  - `type` - the signature of a type, after its name
- The remaining arguments are type signatures, these may reference definitions introduced by the first argument.

Each type signature is then be printed, displaying their internal structure, alongside their class, using
the same representation that can later be used in pattern matching to decompose the type.

Here, we inspect a refinement of a class `Box`:
```bash
$ sbt
> scala3-compiler-bootstrapped/Test/runMain dotty.tools.printTypes "class Box { def x: Any }" "rhs" "Box { def x: Int }"
RefinedType(TypeRef(ThisType(TypeRef(NoPrefix, module class <empty>)),class Box), x, ExprType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix, module class <root>)), object scala), class Int))) [class dotty.tools.dotc.core.Types$CachedRefinedType]
```

You can also pass the empty string as the second
argument, e.g. to inspect a standard library type:
```bash
$ sbt
> scala3-compiler-bootstrapped/Test/runMain dotty.tools.printTypes "" "rhs" "1 *: EmptyTuple"
AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix, module class <root>)), object scala), class *:), List(ConstantType(Constant(1)), TypeRef(TermRef(ThisType(TypeRef(NoPrefix, module class scala)), object Tuple$package), type EmptyTuple)))
```

Here are some other examples you can follow:
- `...printTypes "" class "[T] extends Foo[T] {}"`
- `...printTypes "" method "(x: Int): x.type"`
- `...printTypes "" type "<: Int" "= [T] =>> List[T]"`

If you want to further inspect the types, and not just print them, the object `dotty.tools.DottyTypeStealer` has a
method `stealType`. It takes the same arguments as `printTypes`, but returns both a `Context` containing the
definitions passed, along with the list of types:
```scala
// compiler/test/dotty/tools/DottyTypeStealer.scala
object DottyTypeStealer extends DottyTest {

  enum Kind:
    case `rhs`, `method`, `class`, `type`
    ...

  def stealType(kind: Kind, source: String, typeStrings: String*): (Context, List[Type]) = {
    ...
  }
}
```
Any test source within `compiler/test` can then call `stealType` for custom purposes.

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
