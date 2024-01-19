---
layout: doc-page
title: Given Loop Prevention
redirectFrom: /docs/reference/other-new-features/into-modifier.html
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/into-modifier.html
---

Implicit resolution now avoids generating recursive givens that can lead to an infinite loop at runtime. Here is an example:

```scala
object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // was error, now avoided
  }
}
```

Previously, implicit resolution would resolve the `summon` to the given in `Price`, leading to an infinite loop (a warning was issued in that case). We now use the underlying given in `BigDecimal` instead. We achieve that by adding the following rule for implicit search:

 - When doing an implicit search while checking the implementation of a `given` definition `G` of the form
    ```
    given ... = ....
    ```
    discard all search results that lead back to `G` or to a given with the same owner as `G` that comes later in the source than `G`.

The new behavior is enabled with the `experimental.givenLoopPrevention` language import. If no such import or setting is given, a warning is issued where the behavior would change under that import (for source version 3.4 and later).

Old-style implicit definitions are unaffected by this change.

