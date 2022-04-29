---
layout: doc-page
title: "Wildcard Context Bounds"
---

This page describes experimental support for context bounds on wildcard types Scala 3, available in 3.2 or later. It is enabled by the language import
```scala
import language.experimental.wildcardContextBounds
```
The reason for publishing this extension now is to get feedback on its usefulness.

## Why Wildcard Context Bounds?

Typeclasses are [first-class consideration of Scala 3](https://docs.scala-lang.org/scala3/book/ca-type-classes.html) 
and [context bound sugar](https://docs.scala-lang.org/scala3/book/ca-context-bounds.html) already exists to support their use.
Taking an example from [here](https://docs.scala-lang.org/scala3/book/ca-type-classes.html), context bounds allow one
to write
```scala
trait ShowableTypeclass[T] {
  def show(t: T): String
}

def showAll[S: ShowableTypeclass](xs: Set[S]): Unit = xs.foreach(x => println(x.show))
```

instead of

```scala
def showAll[S](xs: Set[S])(using ShowableTypeclass[S]): Unit = xs.foreach(x => println(x.show))
```

See [this StackOverflow answer](https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012)
for more discussion and motivation. At the same time, Scala 3 has sought to avoid forcing the user to 
[provide names that are never referenced](https://docs.scala-lang.org/scala3/reference/contextual/). When using
typeclasses, it is often the case that the type parameter is never referenced -- all that matters is that
that the compiler can prove that a `given` instance is available for that type. The `wildcardContextBounds` extension
allows the user to avoid specifying a name for the type of a parameter with a context bound:

```scala
def showAll(xs: Set[? : ShowableTypeclass]): Unit = xs.foreach(x => println(x.show))
```

is sugar for

```scala
def showAll[S: ShowableTypeclass](xs: Set[S]): Unit = xs.foreach(x => println(x.show))
```

which further desugars as above. Rust, which also has first-class support for typeclasses,
has a similar syntax with its [`impl` traits](https://doc.rust-lang.org/reference/types/impl-trait.html)


## Syntax

The syntax closely follows existing syntax for type bounds: note the similarity to 
```scala
trait ShowableInheritance {
  def show: String
}

def showAll(xs: Set[? <: ShowableInheritance]): Unit = xs.foreach(x => println(x.show))
```

and

```scala
def showAll[S <: ShowableInheritance](xs: Set[S]): Unit = xs.foreach(x => println(x.show))
```

Unlike type bounds on wildcard types, context bounds on wildcard types can appear as the top-level type of a parameter.

```scala
def showOne(x: ? : ShowableTypeclass): Unit = x.show // okay
def showOne[S <: ShowableInheritance](x: S): Unit = x.show // okay
def showOne(x: ? <: ShowableInheritance): Unit = x.show // error: Unbound wildcard type
```


