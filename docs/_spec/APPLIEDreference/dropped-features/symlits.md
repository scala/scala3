---
layout: doc-page
title: "Dropped: Symbol Literals"
nightlyOf: https://docs.scala-lang.org/scala3/reference/dropped-features/symlits.html
---

Symbol literals are no longer supported.

The [`scala.Symbol`](https://scala-lang.org/api/3.x/scala/Symbol.html) class still exists, so a literal translation of the symbol literal `'xyz` is `Symbol("xyz")`. However, it is recommended to use a plain string literal `"xyz"` instead. (The `Symbol` class will be deprecated and removed in the future). Example:


```
scalac Test.scala
-- Error: Test.scala:1:25 ------------------------------------------------------------------------------------------------

1 |@main def test = println('abc)
  |                         ^
  |                         symbol literal 'abc is no longer supported,
  |                         use a string literal "abc" or an application Symbol("abc") instead,
  |                         or enclose in braces '{abc} if you want a quoted expression.
  |                         For now, you can also `import language.deprecated.symbolLiterals` to accept
  |                         the idiom, but this possibility might no longer be available in the future.
1 error found
```
