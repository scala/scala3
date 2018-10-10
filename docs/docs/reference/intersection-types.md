---
layout: doc-page
title: "Intersection Types"
---

Used on types, the `&` operator creates an intersection type.

```scala
trait Resettable {
  def reset(): this.type
}
trait Growable[T] {
  def add(x: T): this.type
}
def f(x: Resettable & Growable[String]) = {
  x.reset()
  x.add("first")
}
```

The value `x` is required to be _both_ a `Resettable` and a
`Growable[String]`.

The members of an intersection type `A & B` are all the members of `A`
and all the members of `B`. For instance `Resettable & Growable[String]`
has member methods `reset` and `add`.

[More details](./type-lambdas-spec.html)
