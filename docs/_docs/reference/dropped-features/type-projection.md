---
layout: doc-page
title: "Dropped: General Type Projection"
nightlyOf: https://docs.scala-lang.org/scala3/reference/dropped-features/type-projection.html
---

Scala 2 allowed general type projection `T#A` where `T` is an arbitrary type and `A` names a type member of `T`.
This turns out to be [unsound](https://github.com/scala/scala3/issues/1050) (at least when combined with other Scala 3 features).

To remedy this, Scala 3 only allows type projection if `T` is a concrete type (any type which is not abstract), an example for such a type would be a class type (`class T`).
A type is abstract if it is:
* An abstract type member (`type T` without `= SomeType`)
* A type parameter (`[T]`)
* An alias to an abstract type (`type T = SomeAbstractType`).
There are no restriction on `A` apart from the fact it has to be a member type of `T`, for example a subclass (`class T { class A }`).

To rewrite code using type projections on abstract types, consider using
path-dependent types or implicit parameters.

This restriction rules out the [type-level encoding of a combinator
calculus](https://michid.wordpress.com/2010/01/29/scala-type-level-encoding-of-the-ski-calculus/).