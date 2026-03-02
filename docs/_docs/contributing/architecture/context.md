---
layout: doc-page
title: Contexts
---

`dotc` has almost no global state (with the exception of the name table,
which hashes strings into unique names). Instead, all
essential bits of information that can vary over a compiler [run](./lifecycle.md) are collected
in a `Context` (defined in [Contexts]).

Most methods in the compiler depend on an implicit anonymous `Context` parameter,
and a typical definition looks like the following:
```scala
import dotty.tools.dotc.Contexts.{Context, ctx}

def doFoo(using Context): Unit =
  val current = ctx.run // access the Context parameter with `ctx`
```

## Memory Leaks
> **Careful:** Contexts can be heavy so beware of memory leaks

It is good practice to ensure that implicit contexts are not
captured in closures or other long-lived objects, in order to avoid space leaks
in the case where a closure can survive several compiler runs (e.g. a
lazy completer for a library class that is never required). In that case, the
convention is that the `Context` be an explicit parameter, to track its usage.

## Context Properties

| Context property  | description                            |
|-------------------|----------------------------------------|
| `compilationUnit` | current compilation unit               |
| `phase`           | current phase                          |
| `run`             | current run                            |
| `period`          | current period                         |
| `settings`        | the config passed to the compiler      |
| `reporter`        | operations for logging errors/warnings |
| `definitions`     | the standard built in definitions      |
| `platform`        | operations for the underlying platform |
| `tree`            | current tree                           |
| `scope`           | current scope                          |
| `typer`           | current typer                          |
| `owner`           | current owner symbol                   |
| `outer`           | outer Context                          |
| `mode`            | type checking mode                     |
| `typerState`      |                                        |
| `searchHistory`   |                                        |
| `implicits`       |                                        |
| ...               | and so on                              |


[Contexts]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/core/Contexts.scala
