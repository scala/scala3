---
layout: doc-page
title: "Dropped: Symbol Literals"
movedTo: https://docs.scala-lang.org/scala3/reference/dropped-features/symlits.html
---

Symbol literals are no longer supported.

The `scala.Symbol` class still exists, so a
literal translation of the symbol literal `'xyz` is `Symbol("xyz")`. However, it is recommended to use a plain string literal `"xyz"` instead. (The `Symbol` class will be deprecated and removed in the future).
