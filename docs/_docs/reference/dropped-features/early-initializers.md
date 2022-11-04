---
layout: doc-page
title: "Dropped: Early Initializers"
nightlyOf: https://docs.scala-lang.org/scala3/reference/dropped-features/early-initializers.html
---

Early initializers of the form

```scala
class C extends { ... } with SuperClass ...
```

have been dropped. They were rarely used, and mostly to compensate for the lack of
[trait parameters](../other-new-features/trait-parameters.md), which are now directly supported in Scala 3.

For more information, see [SLS §5.1.6](https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#early-definitions).
