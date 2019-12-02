---
layout: doc-page
title: Escapes in interpolations
---

In Scala 2 there was no straightforward way to represnt a single quote character `"` in a single quoted interpolation. A \ character can't be used for that because interpolators themselves decide how to handle escaping, so the parser doesn't know whether or not the " shold be escaped or used as a terminator.

In Dotty, you can use the `$` meta character of interpolations to escape a `"` character.

```scala
  val inventor = "Thomas Edison"
  val interpolation = s"as $inventor said: $"The three great essentials to achieve anything worth while are: Hard work, Stick-to-itiveness, and Common sense.$""
```
