---
layout: doc-page
title: Debugging Techniques
---

# Debugging Techniques
- [Setting up the playground](#setting-up-the-playground)
- [Show for human readable output](#show-for-human-readable-output)
- [How to disable color](#how-to-disable-color)
- [Reporting as a non-intrusive println](#reporting-as-a-non-intrusive-println)
- [Printing out trees after phases](#printing-out-trees-after-phases)
- [Printing out stack traces of compile time errors](#printing-out-stack-traces-of-compile-time-errors)
- [Configuring the printer output](#configuring-the-printer-output)
- [Figuring out an object creation site](#figuring-out-an-object-creation-site)
  * [Via ID](#via-id)
  * [Via tracer](#via-tracer)
- [Built-in Logging Architecture](#built-in-logging-architecture)
  * [Printers](#printers)
  * [Tracing](#tracing)
  * [Reporter](#reporter)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

## Setting up the playground
Consider the `../issues/Playground.scala` (relative to the Dotty directory) file is:

```scala
object Playground {
  def main(args: Array[String]) = {
    println("Hello World")
  }
}
```

Then, you can debug Dotty by compiling this file via `scalac ../issues/Playground.scala` (from the SBT console) and collecting various debug output in process. This section documents techniques you can use to collect the debug info.

[This](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/typer/Typer.scala#L2231) is the entry point to the Typer. The job of the Typer is to take an untyped tree, compute its type and turn it into a typed tree by attaching the type information to that tree. We will use this entry point to practice debugging techniques. E.g.:

```scala
  def typed(tree: untpd.Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Tree =
    trace(i"typing $tree", typr, show = true) {
      println("Hello Debug!")
      /*...*/
```

Then:

```shell
scalac ../issues/Playground.scala
```

The techniques discussed below can be tried out in place of `println("Hello Debug")` in that location. They are of course applicable throughout the codebase.

## Show for human readable output
Many objects in the compiler have a `show` method available on them via implicit rich wrapper:

```scala
println(tree.show)
```

This will output every single tree passing through the typer (or wherever else you inject it) in a human readable form. Try calling `show` on anything you want to be human-readable, and chances are it will be possible to do so.

## How to disable color
Note that the `show` command above outputs the code in color. This is achieved by injecting special characters into the strings which terminals interpret as commands to change color of the output. This however may not be what you want, e.g. if you want to zero-in on a particular tree:

```scala
if (tree.show == """println("Hello World")""")
  println(s"${tree.show}\n${pt.show}\n${tree.uniqueId}\n===\n")
```

The intention above is to output an extended debug info on a tree that matches a particular human-readable representation. However, because of the color characters, the comparison will fail.

To disable color output from `show`, run `scalac` as follows:

`scalac -color:never ../issues/Playground.scala`

## Reporting as a non-intrusive println
Consider you want to debug the `tree` that goes into `assertPositioned(tree)` in the `typed` method. You can do:

```scala
println(tree.show)
assertPositioned(tree)
```

But you can also do:

```scala
assertPositioned(tree.reporting(s"Tree is: $result"))
```

`extension (a: A) def reporting(f: WrappedResult[T] ?=> String, p: Printer = Printers.default): A` is defined on all types. The function `f` can be written without the argument since it is a context function. The `result` variable is a part of the `WrapperResult` – a tiny framework powering the `reporting` function. Basically, whenever you are using `reporting` on an object `A`, you can use the `result: A` variable from this function and it will be equal to the object you are calling `reporting` on.

## Printing out trees after phases
To print out the trees you are compiling after the FrontEnd (scanner, parser, namer, typer) phases:

```shell
scalac -Xprint:typer ../issues/Playground.scala
```

To print out the trees after Frontend and CollectSuperCalls phases:

```shell
scalac -Xprint:typer,collectSuperCalls ../issues/Playground.scala
```

To print out the trees after all phases:

```shell
scalac -Xprint:all ../issues/Playground.scala
```

To find out the list of all the phases and their names, check out [this](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/Compiler.scala#L34) line in `Compiler.scala`. Each `Phase` object has `phaseName` defined on it, this is the phase name.

## Printing out stack traces of compile time errors
You can use the flag `-Ydebug-error` to get the stack trace of all the compile-time errors. Consider the following file:

```scala
object Foo
object Foo
```

Clearly we cannot define an object `Foo` twice. Now compile it as follows: `scalac -Ydebug-error ../issues/Playground.scala` (use whatever path you saved it under). The result will be as follows:

```scala
-- Error: ../issues/Playground.scala:2:0 ---------------------------------------
2 |object Foo
  |^
  |object Foo has already been compiled once during this run
java.lang.Thread.getStackTrace(Thread.java:1552)
dotty.tools.dotc.reporting.Reporting.error(Reporter.scala:139)
dotty.tools.dotc.core.Contexts$Context.error(Contexts.scala:71)
dotty.tools.dotc.typer.Namer.errorName$2(Namer.scala:300)
dotty.tools.dotc.typer.Namer.checkNoConflict$1(Namer.scala:306)
dotty.tools.dotc.typer.Namer.createSymbol(Namer.scala:353)
dotty.tools.dotc.typer.Namer.recur$1(Namer.scala:490)
dotty.tools.dotc.typer.Namer.recur$3$$anonfun$2(Namer.scala:495)
...
```

So, the error happened in the Namer's `checkNoConflict` method (after which all the stack frames represent the mechanics of issuing an error, not an intent that produced the error in the first place).

## Configuring the printer output
Printing from the `show` and `-Xprint` is done from the Printers framework (discussed in more details below). The following settings influence the output of the printers:

```scala
val printLines        = BooleanSetting("-print-lines"        , "Show source code line numbers.") withAbbreviation "--print-lines"
val uniqid            = BooleanSetting("-uniqid"             , "Uniquely tag all identifiers in debugging output.") withAbbreviation "--unique-id"
val XprintInline      = BooleanSetting("-Xprint-inline"      , "Show  where inlined code comes from")
val XprintTypes       = BooleanSetting("-Xprint-types"       , "Print tree types (debugging option).")
val Ydebug            = BooleanSetting("-Ydebug"             , "Increase the quantity of debugging output.")
val YdebugFlags       = BooleanSetting("-Ydebug-flags"       , "Print all flags of definitions")
val YdebugMissingRefs = BooleanSetting("-Ydebug-missing-refs", "Print a stacktrace when a required symbol is missing")
val YdebugNames       = BooleanSetting("-Ydebug-names"       , "Show internal representation of names")
val YdebugPos         = BooleanSetting("-Ydebug-pos"         , "Show full source positions including spans")
val YdebugTrace       = BooleanSetting("-Ydebug-trace"       , "Trace core operations")
val YdebugTreeWithId  = IntSetting    ("-Ydebug-tree-with-id", "Print the stack trace when the tree with the given id is created", Int.MinValue)
val YprintDebug       = BooleanSetting("-Yprint-debug"       , "when printing trees, print some extra information useful for debugging.")
val YprintDebugOwners = BooleanSetting("-Yprint-debug-owners", "when printing trees, print owners of definitions.")
val YprintPos         = BooleanSetting("-Yprint-pos"         , "show tree positions.")
val YprintPosSyms     = BooleanSetting("-Yprint-pos-syms"    , "show symbol definitions positions.")
val YprintSyms        = BooleanSetting("-Yprint-syms"        , "when printing trees print info in symbols instead of corresponding info in trees.")
val YshowTreeIds      = BooleanSetting("-Yshow-tree-ids"     , "Uniquely tag all tree nodes in debugging output.")
val YshowVarBounds    = BooleanSetting("-Yshow-var-bounds"   , "Print type variables with their bounds")
val YtestPickler      = BooleanSetting("-Ytest-pickler"      , "self-test for pickling functionality; should be used with -Ystop-after:pickler")
```

They are defined in [ScalaSettings.scala](https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/config/ScalaSettings.scala). E.g. `YprintPos` is defined as:

```scala
val YprintPos: Setting[Boolean] = BooleanSetting("-Yprint-pos", "show tree positions.")
```

And is to be used as:

```scala
scalac -Yprint-pos  ../issues/Playground.scala
```

If used, all the trees output with `show` or via `-Xprint:typer` will also have positions attached to them, e.g.:

```scala
package <empty>@<Playground.scala:1> {
  module object Playground {
    def main(
      args:
        Array@<Playground.scala:2>[String@<Playground.scala:2>]@<
          Playground.scala:2
        >
      @<Playground.scala:2>
    ) =
      {
        println@<Playground.scala:3>("Hello World"@<Playground.scala:3>)@<
          Playground.scala:3
        >
      }@<Playground.scala:2>
    @<Playground.scala:2>
  }@<Playground.scala:1>
}@<Playground.scala:1>
<empty>@<Playground.scala:1>
```

## Figuring out an object creation site
### Via ID
Every [Positioned](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/ast/Positioned.scala) (a parent class of `Tree`) object has a `uniqueId` field. It is an integer that is unique for that tree and doesn't change from compile run to compile run. You can output these IDs from any printer (such as the ones used by `.show` and `-Xprint`) via `-Yshow-tree-ids` flag, e.g.:

```shell
scalac -Xprint:typer -Yshow-tree-ids  ../issues/Playground.scala
```

Gives:

```scala
package <empty>#1047 {
  final lazy module val Playground: Playground$#1049 =
    new Playground$#1049#1050#1051()#1052
  #1053
  final module class Playground$() extends Object#1090#1091#1092()#1093, _root_#
    1061
  .scala#1062.Serializable#1063 { this: Playground#1054.type#1055 =>
    def main(args: Array#1028[String#1033]#1034#1038): Unit#1039 =
      {
        println#1094("Hello World"#1041)#1095
      }#1096
    #1097
  }#1099
}#1100
```

You can then use these IDs to locate the creation site of a given tree using that ID via `-Ydebug-tree-with-id`, e.g.:

```shell
scalac -Ydebug-tree-with-id 1049 ../issues/Playground.scala
```

When the tree with the correspond id is allocated, the following prompt will appear:

```
Debug tree (id=1049) creation
Ident(Playground$)


a)bort, s)tack, r)esume
```

If you input `s`, you will get a stack trace like this:

```
java.lang.Throwable
  at dotty.tools.dotc.reporting.Reporter$.loop$1(Reporter.scala:55)
  at dotty.tools.dotc.reporting.Reporter$.displayPrompt(Reporter.scala:63)
  at dotty.tools.dotc.ast.Positioned.printTrace$1(Positioned.scala:32)
  at dotty.tools.dotc.ast.Positioned.uniqueId_$eq(Positioned.scala:34)
  at dotty.tools.dotc.ast.Positioned.<init>(Positioned.scala:45)
  at dotty.tools.dotc.ast.Trees$Tree.<init>(Trees.scala:53)
  at dotty.tools.dotc.ast.Trees$DenotingTree.<init>(Trees.scala:266)
  at dotty.tools.dotc.ast.Trees$NameTree.<init>(Trees.scala:292)
  at dotty.tools.dotc.ast.Trees$RefTree.<init>(Trees.scala:298)
  at dotty.tools.dotc.ast.Trees$Ident.<init>(Trees.scala:375)
  at dotty.tools.dotc.ast.untpd$.Ident(untpd.scala:301)
  at dotty.tools.dotc.ast.desugar$.moduleDef(Desugar.scala:804)
  at dotty.tools.dotc.ast.desugar$.defTree(Desugar.scala:1038)
  at dotty.tools.dotc.typer.Namer.expand(Namer.scala:441)
  at dotty.tools.dotc.typer.Namer.index$$anonfun$1(Namer.scala:722)
  at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:15)
  at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:10)
  at scala.collection.immutable.List.foreach(List.scala:392)
  at dotty.tools.dotc.typer.Namer.index(Namer.scala:722)
  at dotty.tools.dotc.typer.Namer.recur$1(Namer.scala:484)
  at dotty.tools.dotc.typer.Namer.indexExpanded(Namer.scala:501)
  at dotty.tools.dotc.typer.Namer.index(Namer.scala:474)
  at dotty.tools.dotc.typer.FrontEnd.enterSyms$$anonfun$1(FrontEnd.scala:69)
  at dotty.runtime.function.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.java:12)
  at dotty.tools.dotc.typer.FrontEnd.monitor(FrontEnd.scala:41)
  at dotty.tools.dotc.typer.FrontEnd.enterSyms(FrontEnd.scala:71)
  at dotty.tools.dotc.typer.FrontEnd.runOn(FrontEnd.scala:100)
  at dotty.tools.dotc.Run.runPhases$4$$anonfun$4(Run.scala:158)
  at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:15)
  at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:10)
  at scala.collection.IndexedSeqOptimized.foreach(IndexedSeqOptimized.scala:36)
  at scala.collection.IndexedSeqOptimized.foreach$(IndexedSeqOptimized.scala:33)
  at scala.collection.mutable.ArrayOps$ofRef.foreach(ArrayOps.scala:198)
  at dotty.tools.dotc.Run.runPhases$5(Run.scala:170)
  at dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:178)
  at dotty.runtime.function.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.java:12)
  at dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:102)
  at dotty.tools.dotc.Run.compileUnits(Run.scala:185)
  at dotty.tools.dotc.Run.compileSources(Run.scala:120)
  at dotty.tools.dotc.Run.compile(Run.scala:104)
  at dotty.tools.dotc.Driver.doCompile(Driver.scala:34)
  at dotty.tools.dotc.Driver.process(Driver.scala:172)
  at dotty.tools.dotc.Driver.process(Driver.scala:141)
  at dotty.tools.dotc.Driver.process(Driver.scala:153)
  at dotty.tools.dotc.Driver.main(Driver.scala:180)
  at dotty.tools.dotc.Main.main(Main.scala)
```

So that tree was created at:

```
  at dotty.tools.dotc.ast.desugar$.moduleDef(Desugar.scala:804)
```

Since all the stack frames above it are technical frames executing the tree creation command, and the frame in question is the location where the intent of the tree creation was expressed.

### Via tracer
Some objects may not be `Positioned` and hence their creation site is not debuggable via the technique in the section above. Say you target a tree at `Typer`'s `typed` method as follows:

```scala
if (tree.show == """println("Hello World")""") {
  val creationSite = "<creation site stack here>"
  println(creationSite)
}
```

In other words, you have a reference to the object and want to know were it was created. To do so, go to the class definition of that object. In our case, `tree` is a [`Tree`](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/ast/Trees.scala#L52). Now, create a new `val` member of that type:

```scala
val tracer = Thread.currentThread.getStackTrace.mkString("\n")
```

Then, from the `typed` method (or wherever you want to access the trace):

```scala
if (tree.show == """println("Hello World")""") {
  val creationSite = tree.tracer
  println(creationSite)
}
```

## Built-in Logging Architecture
Dotty has a lot of debug calls scattered throughout the code, most of which are disabled by default. At least three (possibly intertwined) architectures for logging are used for that:

- Printer
- Tracing
- Reporter

These do not follow any particular system and so probably it will be easier to go with `println` most of the times instead.

### Printers
Defined in [Printers.scala](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/config/Printers.scala) as a set of variables, each responsible for its own domain. To enable them, replace `noPrinter` with `default`. [Example](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/typer/Typer.scala#L2226) from the code:

```scala
typr.println(i"make contextual function $tree / $pt ---> $ifun")
```

`typr` is a printer.

### Tracing
Defined in [trace.scala](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/reporting/trace.scala). [Example](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/typer/Typer.scala#L2232) from the code:

```scala
trace(i"typing $tree", typr, show = true) { // ...
```

To enable globally, change [tracingEnabled](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/config/Config.scala#L164) to `true` (will recompile a lot of code).

You also need to set the printer referenced in the call (in the example, `typr`) to `default` as explained in the section on printers.

To enable for a single trace, do the following:

```scala
trace.force(i"typing $tree", typr, show = true) { // ...
```

### Reporter
Defined in [Reporter.scala](https://github.com/lampepfl/dotty/blob/10526a7d0aa8910729b6036ee51942e05b71abf6/compiler/src/dotty/tools/dotc/reporting/Reporter.scala). Enables calls such as `report.log`. To enable, run scalac with `-Ylog:typer` option.
