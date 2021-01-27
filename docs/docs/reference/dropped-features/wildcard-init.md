---
layout: doc-page
title: "Dropped: wildcard initializer"
---

The syntax
```scala
  var x: A = _
```
that was used to indicate an uninitialized field has been dropped.
At its place there is a special value `notInitialized` in the `scala.compiletime` package. To get an uninitialized field, you now write
```scala
import scala.compiletime.notInitialized

var x: A = notInitialized
```
To enable cross-compilation, `_` is still supported, but it will be dropped in a future 3.x version.

