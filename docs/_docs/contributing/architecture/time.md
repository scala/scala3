---
layout: doc-page
title: Time in the Compiler
---

In the [compiler overview](lifecycle.md) section, we saw that `dotc` is an interactive compiler,
and so can answer questions about entities as they come into existance and change throughout time,
for example:
- which new definitions were added in a REPL session?
- which definitions were replaced in an incremental build?
- how are definitions simplified as they are adapted to the runtime system?

## Hours, Minutes, and Periods

For the compiler to be able to resolve the above temporal questions, and more, it maintains
a concept of time. Additionally, because interactions are frequent, it is important to
persist knowledge of entities between interactions, allowing the compiler to remain performant.
Knowing about time allows the compiler to efficiently mark entities as being outdated.

Conceptually, `dotc` works like a clock, where its minutes are represented by [phases](phases.md),
and its hours by [runs]. Like a clock, each run passes once each of its phases have completed
sequentially, and then a new run can begin. Phases are further grouped into [periods], where
during a period certain entities of the compiler remain stable.

## Time Travel

During a run, each phase can rewrite the world as the compiler sees it, for example:
- to transform trees,
- to gradually simplify type from Scala types to JVM types,
- to move definitions out of inner scopes to outer ones, fitting the JVM's model,
- and so on.

Because definitions can [change over time](symbols.md#definitions-are-dynamic), various artifacts associated with them
are stored non-destructively, and views of the definition created earlier, or later
in the compiler can be accessed by using the `atPhase` method, defined in [Contexts].

As an example, assume the following definitions are available in a [Context](context.md):
```scala
class Box { type X }

def foo(b: Box)(x: b.X): List[b.X] = List(x)
```

You can compare the type of definition `foo` after the [typer] phase and after the [erasure] phase
by using `atPhase`:
```scala
import dotty.tools.dotc.core.Contexts.{Context, atPhase}
import dotty.tools.dotc.core.Phases.{typerPhase, erasurePhase}
import dotty.tools.dotc.core.Decorators.i

given Context = …

val fooDef: Symbol = … // `def foo(b: Box)(x: b.X): List[b.X]`

println(i"$fooDef after typer   => ${atPhase(typerPhase.next)(fooDef.info)}")
println(i"$fooDef after erasure => ${atPhase(erasurePhase.next)(fooDef.info)}")
```
and see the following output:
```
method foo after typer   => (b: Box)(x: b.X): scala.collection.immutable.List[b.X]
method foo after erasure => (b: Box, x: Object): scala.collection.immutable.List
```

[runs]: https://github.com/scala/scala3/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/Run.scala
[periods]: https://github.com/scala/scala3/blob/a527f3b1e49c0d48148ccfb2eb52e3302fc4a349/compiler/src/dotty/tools/dotc/core/Periods.scala
[Contexts]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/Contexts.scala
[typer]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/typer/TyperPhase.scala
[erasure]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/transform/Erasure.scala
