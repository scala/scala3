---
layout: doc-page
title: "Deferred Givens"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/deferred-givens.html
---

Scala 3.6 introduces a new way to implement a given definition in a trait like this:
```scala
given T = deferred
```
Such givens can be implemented automatically in subclasses. `deferred` is a new method in the `scala.compiletime` package, which can appear only as the right hand side of a given defined in a trait. Any class implementing that trait will provide an implementation of this given. If a definition is not provided explicitly, it will be synthesized by searching for a given of type `T` in the scope of the inheriting class. Specifically, the scope in which this given will be searched is the environment of that class augmented by its parameters but not containing its members (since that would lead to recursive resolutions). If an implementation _is_ provided explicitly, it counts as an override of a concrete definition and needs an `override` modifier.

Deferred givens allow a clean implementation of context bounds in traits,
as in the following example:
```scala
trait Sorted:
  type Element : Ord

class SortedSet[A : Ord as ord] extends Sorted:
  type Element = A
```
The compiler expands this to the following implementation.
```scala
trait Sorted:
  type Element
  given Ord[Element] = compiletime.deferred

class SortedSet[A](using ord: Ord[A]) extends Sorted:
  type Element = A
  override given Ord[Element] = ord
```

The using clause in class `SortedSet` provides an implementation for the deferred given in trait `Sorted`.

One can also provide an explicit implementation of a deferred given, as in the following example:

```scala
class SortedString[A] extends Sorted:
  type Element = String
  override given Ord[String] = ...
```

Note that the implementing given needs an `override` modifier since the `deferred` given in class `Sorted` counts as a concrete (i.e. not abstract) definition. In a sense, `deferred` on the right-hand side in `Sorted` is like a (magic, compiler-supported) macro, with the peculiarity that the macro's implementation also affects subclasses.

## Abstract Givens

A given may also be an abstract member, with the restriction that it must have an explicit name. Example:

```scala
trait HasOrd[T]:
  given ord: Ord[T]
```
An abstract given has the form `given name: Type` without a right-hand side or arguments to the type.

Since Scala 3.6, abstract givens are made redundant by deferred givens. Deferred givens have better ergonomics, since they get naturally implemented in inheriting classes, so there is no longer any need for boilerplate to fill in definitions of abstract givens.

It is therefore recommended that software architectures relying on  abstract givens be migrated to use deferred givens instead. Abstract givens are still supported in Scala 3.6, but will likely be deprecated and phased out over time.
