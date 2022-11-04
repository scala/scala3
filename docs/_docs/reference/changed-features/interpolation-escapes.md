---
layout: doc-page
title: "Escapes in interpolations"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/interpolation-escapes.html
---

In Scala 2 there is no straightforward way to represent a single quote character `"` in a single quoted interpolation. A `\` character can't be used for that because interpolators themselves decide how to handle escaping, so the parser doesn't know whether the `"` character should be escaped or used as a terminator.

In Scala 3, we can use the `$` meta character of interpolations to escape a `"` character. Example:

```scala
  val inventor = "Thomas Edison"
  val interpolation = s"as $inventor said: $"The three great essentials to achieve anything worth while are: Hard work, Stick-to-itiveness, and Common sense.$""
```
