---
layout: doc-page
title: Dropped: Implicit Classes and Value Classes
---

Scala uses implicit classes to define extension methods and conversions. E.g., from `Predef.scala`:

```scala
implicit final class ArrowAssoc[A](private val self: A) extends AnyVal {
  def -> [B](y: B): (A, B) = (self, y)
}
```

Implicit classes and value classes are still supported in Dotty, in order to support cross compilation with Scala 2. But they will be phased out in the future. The `ArrowAssoc` class can be written as an [augmentation](../augments/method-augments.html) as follows:

```scala
augment (type A) {
  def -> [B](that: B): (A, B) = (this, that)
}
```

Most other uses of value classes can be expressed by [opaque type aliases](../opaque.html).
There are only two aspects of value classes that are not covered:

 - A user-definable `toString` method. E.g. if `Meter` is a value class implemented in terms of `Double`, it is possible to define `toString` to that `new Meter(10).toString` prints `10m` instead of `10`.

 - Type tests and checked type casts for value classes. E.g. if `x: Any` once can have a pattern match like this:

      x match { case m: Meter => ... }

   The match succeeds if `x` is a `Meter` but not if it is `Double`. By contrast, an implementation in terms of
   opaque type aliases would flag the match with an unchecked warning and succeed for both. This is a consequence of using the same runtime representation for `Meter` and `Double`.

Given the quite high complexity of value classes, in terms of rules what is legal and what is not, and in terms of their boxing model, it is attractive to drop them. The combinaton of opaque types and augmentations is both simpler and generally more pleasant to use.

