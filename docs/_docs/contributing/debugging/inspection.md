---
layout: doc-page
title: How to Inspect Values
redirectFrom: /docs/contributing/workflow/inspection.html
---

In this section, you will find out how to debug the contents of certain objects
while the compiler is running, and inspect produced artifacts of the compiler.

## Inspecting variables in-place

Frequently you will need to inspect the content of a particular variable.
You can either use `println`s or the debugger, more info on how to setup the latter.

In the remainder of this article we'll use `println(<someUsefulExpression>)`
inserted in the code, but the same effect can be accomplished by stopping at a
breakpoint, and typing `<someUsefulExpression>` in the [debug
console](./ide-debugging.md#the-debug-console) of the debugger.

When printing a variable, it's always a good idea to call `show` on that variable: `println(x.show)`.
Many objects of the compiler define `show`, returning a human-readable string.
e.g. if called on a tree, the output will be the tree's representation as source code, rather than
the underlying raw data.

Sometimes you need to print flags. Flags are metadata attached to [symbols] containing information such as whether a
class is abstract, comes from Java, what modifiers a variable has (private, protected etc) and so on.
Flags are stored in a single `Long` value, each bit of which represents whether a particular flag is set.

To print flags, you can use the `flagsString` method, e.g. `println(x.flagsString)`.

## Pretty Printing with a String Interpolator

You can also pretty print objects with string interpolators,
these default to call `.show` when possible, avoiding boilerplate
and also helping format error messages.

Import them with the following:

```scala
import dotty.tools.dotc.core.Decorators.*
```

Here is a table of explanations for their use:

| Usage  | Description                       |
|--------|-----------------------------------|
|`i""`   | General purpose string formatting. It calls `.show` on objects <br/> mixing in Showable, `String.valueOf` otherwise |
|`em""`  | Formatting for error messages: Like `i` but suppress <br/>follow-on, error messages after the first one if some <br/>of their arguments are "non-sensical". |
|`ex""`  | Formatting with added explanations: Like `em`, but add <br/>explanations to give more info about type variables<br/>and to disambiguate where needed. |


## Obtaining debug output from the compiler

As explained in [navigation](../issues/cause.md), we can debug the code being generated as it is transformed
through the compiler. As well as plain tree output, there are many compiler options that
add extra debug information to trees when compiling a file; you can find the full list
in [ScalaSettings].

## Stopping the compiler early
Sometimes you may want to stop the compiler after a certain phase, for example to prevent
knock-on errors from occurring from a bug in an earlier phase. Use the flag
`-Ystop-after:<phase-name>` to prevent any phases executing afterwards.

> e.g. `-Xprint:<phase>` where `phase` is a miniphase, will print after
> the whole phase group is complete, which may be several miniphases after `phase`.
> Instead you can use `-Ystop-after:<phase> -Xprint:<phase>` to stop
> immediately after the miniphase and see the trees that you intended.

## Printing TASTy of a Class

If you are working on an issue related to TASTy, it is good to know how to inspect
the contents of a TASTy file, produced from compilation of Scala files.

The next example uses an [issue directory](../issues/reproduce.md#dotty-issue-workspace) to compile a class and print its TASTy.
In the directory, you should create a file `tasty/Foo.scala` (with contents of `class Foo`),
and create a file `tasty/launch.iss` with the following contents:

```
$ (rm -rv out || true) && mkdir out # clean up compiler output, create `out` dir.

scala3/scalac -d $here/out $here/Foo.scala

scala3/scalac -print-tasty $here/out/Foo.tasty
```

With sbt command `issue tasty` you will see output such as the following:

```
--------------------------------------------------------------------------------
local/foo/out/Foo.tasty
--------------------------------------------------------------------------------
Names:
   0: ASTs
   1: <empty>
   2: Foo
   3: <init>
...
```
and so on.

## Inspecting The Representation of Types

> [learn more about types](../architecture/types.md) in `dotc`.

If you are curious about the representation of a type, say `[T] =>> List[T]`,
you can use a helper program [dotty.tools.printTypes][DottyTypeStealer],
it prints the internal representation of types, along with their class. It can be
invoked from the sbt shell with three arguments as follows:

```bash
sbt:scala3> scala3-compiler/Test/runMain
  dotty.tools.printTypes
  <source>
  <kind>
  <typeStrings*>
```

- The first argument, `source`, is an arbitrary string that introduces some Scala definitions.
It may be the empty string `""`.
- The second argument, `kind`, determines the format of the following arguments,
accepting one of the following options:
  - `rhs` - accept return types of definitions
  - `class` - accept signatures for classes
  - `method` - accept signatures for methods
  - `type` - accept signatures for type definitions
  - The empty string `""`, in which case `rhs` will be assumed.
- The remaining arguments are type signature strings, accepted in the format determined by
`kind`, and collected into a sequence `typeStrings`. Signatures are the part of a definition
that comes after its name, (or a simple type in the case of `rhs`) and may reference
definitions introduced by the `source` argument.

Each one of `typeStrings` is then printed, displaying their internal structure, alongside their class.

### Examples

Here, given a previously defined `class Box { type X }`, you can inspect the return type `Box#X`:
```bash
sbt:scala3> scala3-compiler/Test/runMain
> dotty.tools.printTypes
> "class Box { type X }"
> "rhs"
> "Box#X"
[info] running (fork) dotty.tools.printTypes "class Box { type X }" rhs Box#X
TypeRef(TypeRef(ThisType(TypeRef(NoPrefix,module class <empty>)),class Box),type X) [class dotty.tools.dotc.core.Types$CachedTypeRef]
```

Here are some other examples you can try:
- `...printTypes "" "class" "[T] extends Seq[T] {}"`
- `...printTypes "" "method" "(x: Int): x.type"`
- `...printTypes "" "type" "<: Int" "= [T] =>> List[T]"`

### Don't just print: extracting further information

`dotty.tools.printTypes` is useful to to see the representation
of a type at a glance, but sometimes you want to extract more. Instead, you can use the
method `dotty.tools.DottyTypeStealer.stealType`. With the same inputs as `printTypes`,
it returns both a `Context` containing the definitions passed, along with the list of types.

As a worked example let's create a test case to verify the structure of `Box#X` that you saw earlier:
```scala
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*

import org.junit.Test

import dotty.tools.DottyTypeStealer, DottyTypeStealer.Kind

class StealBox:

  @Test
  def stealBox: Unit =
    val (ictx, List(rhs)) =
      DottyTypeStealer.stealType("class Box { type X }", Kind.rhs, "Box#X")

    given Context = ictx

    rhs match
      case X @ TypeRef(Box @ TypeRef(ThisType(empty), _), _) =>
        assert(Box.name.toString == "Box")
        assert(X.name.toString == "X")
        assert(empty.name.toString == "<empty>")
```

[DottyTypeStealer]: https://github.com/scala/scala3/blob/master/compiler/test/dotty/tools/DottyTypeStealer.scala
[ScalaSettings]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/config/ScalaSettings.scala
[symbols]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/SymDenotations.scala
