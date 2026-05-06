---
layout: doc-page
title: "Deprecated: Nonlocal Returns"

nightlyOf: https://docs.scala-lang.org/scala3/reference/dropped-features/nonlocal-returns.html
---

Returning from nested anonymous functions is deprecated since Scala 3.2.0.

Nonlocal returns are implemented by throwing and catching `scala.runtime.NonLocalReturnException`-s. This is rarely what is intended by the programmer. It can be problematic because of the hidden performance cost of throwing and catching exceptions. Furthermore, it is a leaky implementation: a catch-all exception handler can intercept a `NonLocalReturnException`.

A better alternative to nonlocal returns and also the `scala.util.control.Breaks` API is provided by [`scala.util.boundary` and `boundary.break`](https://nightly.scala-lang.org/api/scala/util/boundary$.html).

Example:

```scala
import scala.util.boundary, boundary.break
def firstIndex[T](xs: List[T], elem: T): Int =
  boundary:
    for (x, i) <- xs.zipWithIndex do
      if x == elem then break(i)
    -1
```
