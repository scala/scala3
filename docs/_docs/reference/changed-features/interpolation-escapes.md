---
layout: doc-page
title: "Escapes in interpolations"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/interpolation-escapes.html
---

Scala 3 allows `"` (Unicode Quotation Mark) in an interpolation that uses `"` as a delimiter.

The quotation mark must be escaped by either `$` (Dollar Sign) or `\` (Reverse Solidus or "backslash").

Note that `\` may also escape `"` in an ordinary string. This behavior is consistent with Scala 2.

```scala
  val inventor = "Thomas Edison"
  val essentials =
    s"As $inventor said: $"The three great essentials to achieve anything worthwhile are, first, hard work; second, stick-to-itiveness; third, common sense.$""
  val perspiration = "interpolation"
  val attribution =
    s"As $inventor wrote in a letter in 1927: \"It is quite true I once made the statement that genius is one percent inspiration and 99 percent $perspiration, and I am still of the same opinion.\""
```
