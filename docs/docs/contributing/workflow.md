---
layout: default
title: "Workflow"
---

This document details common workflow patterns when working with Dotty.

## Cloning and building ##

```bash
# Start by cloning the repository:
git clone https://github.com/lampepfl/dotty.git
cd dotty
# Clone dotty-compatible stdlib. Needed for running the test suite.
git clone -b dotty-library https://github.com/DarkDimius/scala.git scala-scala
```

## Compiling files with dotc ##

From sbt:

```none
> dotc <OPTIONS> <FILE>
```

From terminal:

```none
$ ./bin/dotc <OPTIONS> <FILE>
```

Here are some useful debugging `<OPTIONS>`:

* `-Xprint:PHASE1,PHASE2,...` or `-Xprint:all`: prints the `AST` after each
  specified phase. Phase names can be found by searching
  `compiler/src/dotty/tools/dotc/transform/` for `phaseName`.
* `-Ylog:PHASE1,PHASE2,...` or `-Ylog:all`: enables `ctx.log("")` logging for
  the specified phase.
* `-Ycheck:all` verifies the consistency of `AST` nodes between phases, in
  particular checks that types do not change. Some phases currently can't be
  `Ycheck`ed, therefore in the tests we run:
  `-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef`.

Additional logging information can be obtained by changes some `noPrinter` to
`new Printer` in `compiler/src/dotty/tools/dotc/config/Printers.scala`. This enables the
`subtyping.println("")` and `ctx.traceIndented("", subtyping)` style logging.

## Running tests ##

```none
$ sbt
> partest --show-diff --verbose
```

## Running single tests ##
To test a specific test tests/x/y.scala (for example tests/pos/t210.scala):

```none
> partest-only-no-bootstrap --show-diff --verbose tests/partest-generated/x/y.scala
```

Currently this will re-run some unit tests and do some preprocessing because of
the way partest has been set up.

## Inspecting Trees with Type Stealer ##

There is no power mode for the REPL yet, but you can inspect types with the
type stealer:

```none
> repl
scala> import dotty.tools.DottyTypeStealer._; import dotty.tools.dotc.core._; import Contexts._,Types._
```

Now, you can define types and access their representation. For example:

```scala
scala> val s = stealType("class O { type X }", "O#X")
scala> implicit val ctx: Context = s._1
scala> val t = s._2(0)
t: dotty.tools.dotc.core.Types.Type = TypeRef(TypeRef(ThisType(TypeRef(NoPrefix,<empty>)),O),X)
scala> val u = t.asInstanceOf[TypeRef].underlying
u: dotty.tools.dotc.core.Types.Type = TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix,scala)),Nothing), TypeRef(ThisType(TypeRef(NoPrefix,scala)),Any))
```

## Pretty-printing ##
Many objects in the dotc compiler implement a `Showable` trait (e.g. `Tree`,
`Symbol`, `Type`). These objects may be prettyprinted using the `.show`
method
