---
layout: doc-page
title: "Deprecated: Nonlocal Returns"

nightlyOf: https://docs.scala-lang.org/scala3/reference/dropped-features/nonlocal-returns.html
---

Returning from nested anonymous functions has been deprecated, and will produce a warning from version `3.2`.

Nonlocal returns are implemented by throwing and catching `scala.runtime.NonLocalReturnException`-s. This is rarely what is intended by the programmer. It can be problematic because of the hidden performance cost of throwing and catching exceptions. Furthermore, it is a leaky implementation: a catch-all exception handler can intercept a `NonLocalReturnException`.

A drop-in library replacement is provided in [`scala.util.control.NonLocalReturns`](https://scala-lang.org/api/3.x/scala/util/control/NonLocalReturns$.html). Example:

```scala
import scala.util.control.NonLocalReturns.*

extension [T](xs: List[T])
  def has(elem: T): Boolean = returning {
    for x <- xs do
      if x == elem then throwReturn(true)
    false
  }

@main def test(): Unit =
  val xs = List(1, 2, 3, 4, 5)
  assert(xs.has(2) == xs.contains(2))
```
