---
title: "Dropped: Symbol Literals"
type: section
num: 83
previous-page: /scala3/reference/dropped-features/xml
next-page: /scala3/reference/dropped-features/auto-apply
---

Symbol literals are no longer supported.

The `scala.Symbol` class still exists, so a
literal translation of the symbol literal `'xyz` is `Symbol("xyz")`. However, it is recommended to use a plain string literal `"xyz"` instead. (The `Symbol` class will be deprecated and removed in the future).
