---
layout: doc-page
title: "Automatic Eta Expansion - More Details"
movedTo: https://docs.scala-lang.org/scala3/reference/changed-features/eta-expansion-spec.html
---

## Motivation

Scala maintains a convenient distinction between _methods_ and _functions_.
Methods are part of the definition of a class that can be invoked in objects while functions are complete objects themselves, making them first-class entities. For example, they can be assigned to variables.
These two mechanisms are bridged in Scala by a mechanism called
[_eta-expansion_](https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#eta-expansion-section)
(also called eta-abstraction), which converts a reference to a method into a function. Intuitively, a method `m` can be passed around by turning it into an object: the function `x => m(x)`.

In this snippet which assigns a method to a `val`, the compiler will perform _automatic eta-expansion_, as shown in the comment:

```scala
def m(x: Int, y: String) = ???
val f = m // becomes: val f = (x: Int, y: String) => m(x, y)
```

In Scala 2, a method reference `m` is converted to a function value only if the expected type is a function type, which means the conversion in the example above would not have been triggered, because `val f` does not have a type ascription. To still get eta-expansion, a shortcut `m _` would force the conversion.

For methods with one or more parameters like in the example above, this restriction has now been dropped. The syntax `m _` is no longer needed and will be deprecated in the future.

## Automatic eta-expansion and partial application
In the following example `m` can be partially applied to the first two parameters.
Assigning `m` to `f1` will automatically eta-expand.

```scala
def m(x: Boolean, y: String)(z: Int): List[Int]
val f1 = m
val f2 = m(true, "abc")
```

This creates two function values:

```scala
f1: (Boolean, String) => Int => List[Int]
f2: Int => List[Int]
```

## Automatic eta-expansion and implicit parameter lists

Methods with implicit parameter lists will always get applied to implicit arguments.

```scala
def foo(x: Int)(implicit p: Double): Float = ???
implicit val bla: Double = 1.0

val bar = foo // val bar: Int => Float = ...
```

## Automatic Eta-Expansion and query types

A method with context parameters can be expanded to a value of a context type by writing the expected context type explicitly.

```scala
def foo(x: Int)(using p: Double): Float = ???
val bar: Double ?=> Float = foo(3)
```

## Rules

- If `m` has an argument list with one or more parameters, we always eta-expand
- If `m` is has an empty argument list (i.e. has type `()R`):
    1. If the expected type is of the form `() => T`, we eta expand.
    2. If m is defined by Java, or overrides a Java defined method, we insert `()`.
    3. Otherwise we issue an error of the form:

Thus, an unapplied method with an empty argument list is only converted to a function when a function type is expected. It is considered best practice to either explicitly apply the method to `()`, or convert it to a function with `() => m()`.

The method value syntax `m _` is deprecated.

## Reference

For more information, see [PR #2701](https://github.com/lampepfl/dotty/pull/2701).
