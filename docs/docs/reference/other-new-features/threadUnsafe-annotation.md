---
layout: doc-page
title: "The @threadUnsafe annotation"
movedTo: https://docs.scala-lang.org/scala3/reference/other-new-features/threadUnsafe-annotation.html
---

A new annotation `@threadUnsafe` can be used on a field which defines
a `lazy val`. When this annotation is used, the initialization of the
`lazy val` will use a faster mechanism which is not thread-safe.

### Example

```scala
import scala.annotation.threadUnsafe

class Hello:
   @threadUnsafe lazy val x: Int = 1
```
