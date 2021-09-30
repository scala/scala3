---
title: "The @threadUnsafe annotation"
type: section
num: 44
previous-page: /scala3/reference/other-new-features/matchable
next-page: /scala3/reference/other-new-features/targetName
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
