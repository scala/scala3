---
layout: doc-page
title: "The @threadUnsafe annotation"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/threadUnsafe-annotation.html
---

A new annotation [`@threadUnsafe`](https://scala-lang.org/api/3.x/scala/annotation/threadUnsafe.html) can be used on a field which defines
a `lazy val`. When this annotation is used, the initialization of the
[`lazy val`](../changed-features/lazy-vals-init.md) will use a faster mechanism which is not thread-safe.

### Example

```scala
import scala.annotation.threadUnsafe

class Hello:
   @threadUnsafe lazy val x: Int = 1
```
