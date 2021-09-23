---
title: "Dropped: Early Initializers"
type: section
num: 78
previous-page: /scala3/reference/dropped-features/package-objects
next-page: /scala3/reference/dropped-features/class-shadowing
---

Early initializers of the form

```scala
class C extends { ... } with SuperClass ...
```

have been dropped. They were rarely used, and mostly to compensate for the lack of
[trait parameters](../other-new-features/trait-parameters.html), which are now directly supported in Scala 3.

For more information, see [SLS §5.1.6](https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#early-definitions).
